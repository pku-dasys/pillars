package tetriski.pillars.core

import java.io.{File, PrintWriter}

import tetriski.pillars.archlib.{ElementAlu, ElementConst}
import tetriski.pillars.hardware.PillarsModuleInfo
import tetriski.pillars.core.MRRGMode._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/** A special block describing the architecture of the top desgin.
 * It is the highest level in the hierarchy.
 */
class ArchitctureHierarchy extends BlockTrait {

  initName("cgra")

  /** Get a class containing the parameters of modules and the port number of top design.
   *
   * @return the class containing the parameters of modules and the port number of top design
   */
  def getPillarsModuleInfo(): PillarsModuleInfo = {
    var moduleNums = List[Int]()
    var moduleParams = List[List[Int]]()
    for (i <- 0 until elementsArray.size) {
      moduleNums = moduleNums :+ elementsArray(i).size
      for (j <- elementsArray(i).indices) {
        moduleParams = moduleParams :+ elementsArray(i)(j).asInstanceOf[ElementTrait].getParams()
      }
    }
    new PillarsModuleInfo(moduleNums, moduleParams, inPorts.size, outPorts.size)
  }

  /** Get a list of configuration regions where modules share a configuration controller.
   *
   * @example If there is a configuration region,
   *          which is List(List(1,2),List(2,2)),
   *          the module "2" in the "1" type and the module "2" in the type "2"
   *          will share a configuration controller.
   * @return the list of regions where modules share a configuration controller
   */
  def getRegionList(): List[List[List[Int]]] = {
    //TODO: abstract the concept of configuration region as a class.
    var ret = List[List[List[Int]]]()

    /** Find configuration regions with a map between the name of sub-blocks and themselves.
     *
     * @param blockMap the map between the name of sub-blocks and themselves
     * @return blocks declared as configuration regions
     */
    def findConfigRegion(blockMap: Map[String, BlockTrait]): List[BlockTrait] = {
      var ret = List[BlockTrait]()
      for (subBlock <- blockMap.values) {
        if (subBlock.isConfigRegion) {
          ret = ret :+ subBlock
        } else {
          ret = ret ::: findConfigRegion(subBlock.blockMap)
        }
      }
      ret
    }

    val configRegions = findConfigRegion(blockMap).sortBy(x => x.getName())
    //Translate the information of configuration regions into the List format.
    for (configRegion <- configRegions) {
      var moduleList = List[List[Int]]()
      for (i <- 0 until configRegion.elementsArray.size) {
        for (j <- 0 until configRegion.elementsArray(i).size) {
          val module = configRegion.elementsArray(i)(j).asInstanceOf[ElementTrait]
          if (module.getConfigBit() > 0) {
            moduleList = moduleList :+ List(module.getTypeID(), module.getModuleID())
          }
        }
      }
      ret = ret :+ moduleList
    }
    ret
  }

  /** Get the configurations of modules in BigInt format.
   * They are sorted by the configuration regions.
   *
   * @return the configuration in BigInt format
   */
  def getConfigBitStream(): BigInt = {
    var bitBuffer = ArrayBuffer[Int]()
    val regionList = getRegionList()
    for (configRegionModules <- regionList) {
      for (configRegionModule <- configRegionModules) {
        val typeID = configRegionModule(0)
        val moduleID = configRegionModule(1)
        val module = elementsArray(typeID)(moduleID).asInstanceOf[ElementTrait]
        module.configArray.foreach(i => bitBuffer.append(i))
      }
    }
    bitBuffer = bitBuffer.reverse
    var ret: BigInt = 0
    for (i <- 0 until bitBuffer.size) {
      ret = ret << 1
      ret += bitBuffer(i)
    }
    ret
  }

  /** Initialize elements in the hierarchy.
   * After initialization, all module's ModuleID, also called global index,
   * is set as its sequence number in relevant array of ArchitctureHierarchy.
   */
  def init(): Unit = {
    for (i <- 0 until elementsArray.size) {
      for (j <- 0 until elementsArray(i).size) {
        val module = elementsArray(i)(j).asInstanceOf[ElementTrait]
        module.setModuleID(j)
      }
    }
    updateConnect()
  }

  /** Save hierarchy information as modules.json.
   */
  def dumpArchitecture(): Unit = {
    val writer = new PrintWriter(new File("modules.json"))
    writer.flush()

    writer.println("{")
    printModules(writer)
    writer.println("}")

    writer.close()
  }

  /** Reset schedules of ALUs and LSUs.
   */
  def resetSchedules(): Unit = {
    for (alu <- ALUsArray) {
      alu.asInstanceOf[ElementTrait].resetFireTimes()
      alu.asInstanceOf[ElementTrait].resetSkew()
    }
    for (lsu <- LSUsArray) {
      lsu.asInstanceOf[ElementTrait].resetFireTimes()
      lsu.asInstanceOf[ElementTrait].resetSkew()
    }
  }

  /** Reset configurations of modules.
   */
  def resetConfigs(): Unit = {
    for (moduleArray <- elementsArray) {
      for (module <- moduleArray) {
        module.asInstanceOf[ElementTrait].updateConfigArray(0)
        module.asInstanceOf[ElementTrait].bannedINodeSet = Set[BigInt]()
      }
    }
  }

  /** Generate configurations of modules.
   * The secondary consistency requirement, runtime consistency, should be guaranteed here.
   * The runtime consistency ensures the context generated for the hardware modules in a specific cycle
   * should be consistent with the corresponding RRG time slice and the functional description.
   * In this function, after generating origin contexts by pattern matching with the RRG dataflow slices,
   * we split and reconstruct the contexts according to the RRG time slices to produce correct runtime contexts.
   *
   * @example "<0:cgra.tile_0.pe_1_1.muxOut.internalNode_0>"
   *          "1                                           "
   *          "0                                           "
   *          means that the module corresponding to a element
   *          whose hierarchy name is "cgra.tile_0.pe_1_1.muxOut",
   *          should route the data from input port "1" to the output port "0".
   *          So its configuration should be 1 at reconfiguration cycle 0.
   * @example "<0:cgra.tile_0.pe_3_0.alu0.internalNode_0>"
   *          "SELECTED_OP                               "
   *          "8                                         "
   *          means that the module corresponding to a element
   *          whose hierarchy name is "cgra.tile_0.pe_3_0.alu0",
   *          should perform the opcode "8" (i.e. ADD).
   *          So its configuration should be 0, seen in OpcodeTranslator.
   * @param filename  the file name of information TXT
   * @param II        the targeted initiation interval (II)
   * @param constInfo the class containing const values, the corresponding RCs and identification number of const units
   */
  def genConfig(filename: String, II: Int, constInfo: ConstInfo = null): Array[BigInt] = {
    val retBitstreams = new ArrayBuffer[BigInt]()
    val infos = Source.fromFile(filename).getLines().toArray

    //Get information of the configurations of modules
    //and elements corresponding to them from the information TXT
    val infoArrays = new ArrayBuffer[ArrayBuffer[String]]()
    val reconfigModuleArrays = new ArrayBuffer[ArrayBuffer[ElementTrait]]()
    for (i <- 0 until II) {
      val tempIA = new ArrayBuffer[String]()
      infoArrays.append(tempIA)
      val tempMA = new ArrayBuffer[ElementTrait]()
      reconfigModuleArrays.append(tempMA)
    }

    val pattern = "[0-9]+:".r

    println("infos(0)" + infos(0))
    println("infos(1)" + infos(1))
    println("infos(2)" + infos(2))
    for (i <- 0 until (infos.size / 3)) {
      println("")
      val targetStr = infos(i * 3)
      println("targetStr " + targetStr)
      val tempStr = (pattern findFirstIn targetStr).toArray
      println("tempStr " + tempStr.mkString(" "))
      val tempII = tempStr(0).replace(":", "").toInt
      println("tempII " + tempII)

      val moduleName = infos(i * 3).replaceAll("([0-9]+:)|<|>", "")
        .split("\\.", 0)
      println("moduleName " + moduleName.mkString(" "))
      var temp = this.asInstanceOf[BlockTrait]
      for (j <- 1 until moduleName.size - 2) {
        temp = temp(moduleName(j))
        println("j " + j)
        println("moduleName(j) " + moduleName(j))
        println("temp " + temp)
      }
      val module = temp.getElement(moduleName(moduleName.size - 2))
      println("module " + module)
      reconfigModuleArrays(tempII).append(module)

      if (II == 1) {
        infoArrays(tempII).append(moduleName(moduleName.size - 1))
        infoArrays(tempII).append(infos(i * 3 + 1))
        infoArrays(tempII).append(infos(i * 3 + 2))
      } else {
        val mode = module.mode
        println("mode " + mode)
        if (mode == REG_MODE) {
          //Split and reconstruct the contexts.
          infoArrays(tempII).append(moduleName(moduleName.size - 1))
          infoArrays(tempII).append("-1")
          infoArrays(tempII).append(infos(i * 3 + 2))

          val preII = (tempII - 1 + II) % II

          infoArrays(preII).append(moduleName(moduleName.size - 1))
          reconfigModuleArrays(preII).append(module)
          infoArrays(preII).append(infos(i * 3 + 1))
          infoArrays(preII).append("-1")
        } else {
          infoArrays(tempII).append(moduleName(moduleName.size - 1))
          for (j <- 1 until 3) {
            infoArrays(tempII).append(infos(i * 3 + j))
          }
        }
      }
      println("infoArrays(tempII) " + infoArrays(tempII).mkString(" "))
      println("reconfigModuleArrays(tempII) " + reconfigModuleArrays(tempII).mkString(" "))
    }
    println("")
    println("update module configs:::")
    for (ii <- 0 until II) {
      resetConfigs()
      val infoArray = infoArrays(ii)
      for (i <- 0 until infoArray.size / 3) {
        val offset = i * 3

        val module = reconfigModuleArrays(ii)(i)
        val second = infoArray(offset + 1)
        println("")
        println(s"ii:$ii module:$module")
        if (second == "SELECTED_OP") {
          val opcode = infoArray(offset + 2).toInt
          module.updateConfig(opcode)
          println(s"opcode:$opcode")
        }
        else {
          val fanInNums = second.split(" ").toList.map(i => i.toInt)
          val fanOutNode = infoArray(offset + 2)
          val fanOutNums = fanOutNode.split(" ").toList.map(i => i.toInt)
          val internalNodeName = infoArray(offset)
          var internalNum = 0

          //Only a internal node whose name is in the name set of internal nodes of the module is decisive.
          var isDecisive = false
          for (i <- 0 until module.internalNodes.size) {
            val iN = module.internalNodes(i)
            if (iN == internalNodeName) {
              internalNum = i
              isDecisive = true
            }
          }
          if (isDecisive) {
            println(s"faninNums:$fanInNums fanOutNums:$fanOutNums internalNum:$internalNum")
            module.updateConfig(fanInNums, fanOutNums, internalNum)
          }
          module.configArray
          println(s"faninNums:$fanInNums fanOutNums:$fanOutNums internalNum:$internalNum fanOutNode:$fanOutNode")
        }
      }

      //Update configurations of const units.
      for (j <- 0 until constInfo.constIDArray(ii).size) {
        val constID = constInfo.constIDArray(ii)(j)
        val constVal = constInfo.constValArray(ii)(j)
        this.ConstsArray(constID).asInstanceOf[ElementConst].updateConfigArray(constVal)
      }
      retBitstreams.append(getConfigBitStream())
      println("retBitstreans: " + retBitstreams)
    }
    retBitstreams.toArray
  }

  /** Get schedules of ALUs and LSUs.
   *
   * @return the schedules of ALUs and LSUs
   */
  def getSchedules(): List[Int] = {
    var schedules = ALUsArray.map(alu => alu.getSchedule().toList).reduce(_ ++ _)
    schedules = schedules ++ LSUsArray.map(lsu => lsu.getSchedule().toList).reduce(_ ++ _)
    schedules
  }
}