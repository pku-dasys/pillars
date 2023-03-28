package pillars.core

import java.io.{File, PrintWriter}

import chisel3.Module
import pillars.archlib.{ElementAlu, ElementConst, ElementCounter, ElementLSU}
import pillars.hardware.PillarsModuleInfo
import pillars.core.MRRGMode._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/** A special block describing the architecture of the top desgin.
 * It is the highest level in the hierarchy.
 */
class ArchitectureHierarchy extends BlockTrait {

  initName("cgra")

  /** Get a class containing the parameters of modules and the port number of top design.
   *
   * @return the class containing the parameters of modules and the port number of top design
   */
  def getPillarsModuleInfo(): PillarsModuleInfo = {
    var moduleNums = List[Int]()
    var moduleParams = List[List[Int]]()
    var genModuleRules = List[() => Module]()
    for (i <- 0 until elementsArray.size) {
      moduleNums = moduleNums :+ elementsArray(i).size
      for (j <- elementsArray(i).indices) {
        moduleParams = moduleParams :+ elementsArray(i)(j).getParams()
        genModuleRules = genModuleRules :+ elementsArray(i)(j).genModuleRule()
      }
    }
    new PillarsModuleInfo(moduleNums, moduleParams, inPorts.size, outPorts.size, genModuleRules)
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
    var unallocatedElements = elementsArray.map(i => i.toSet).reduce(_ ++ _)
    //Translate the information of configuration regions into the List format.
    for (configRegion <- configRegions) {
      var moduleList = List[List[Int]]()
      for (i <- 0 until configRegion.elementsArray.size) {
        for (j <- 0 until configRegion.elementsArray(i).size) {
          val module = configRegion.elementsArray(i)(j)
          unallocatedElements -= module
          if (module.getConfigBit() > 0) {
            moduleList = moduleList :+ List(module.getTypeID(), module.getModuleID())
          }
        }
      }
      ret = ret :+ moduleList
    }
    if (unallocatedElements.size != 0) {
      var moduleList = List[List[Int]]()
      for (module <- unallocatedElements) {
        if (module.getConfigBit() > 0) {
          moduleList = moduleList :+ List(module.getTypeID(), module.getModuleID())
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

    for (counter <- CountersArray) {
      counter.asInstanceOf[ElementTrait].resetFireTimes()
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
   * @param filename    the file name of behavioral modeling information TXT
   * @param II          the targeted initiation interval (II)
   * @param constInfo   the class containing const values, the corresponding RCs
   *                    and identification number of const units
   * @param counterInfo the class containing basic parameters (configs), the corresponding RCs
   *                    and identification number of counters.
   */
  def genConfig(filename: String, II: Int, constInfo: ConstInfo = null,
                counterInfo: CounterInfo = null): Array[BigInt] = {
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

    for (i <- 0 until (infos.size / 3)) {
      val targetStr = infos(i * 3)
      val tempStr = (pattern findFirstIn targetStr).toArray
      val tempII = tempStr(0).replace(":", "").toInt

      val moduleName = infos(i * 3).replaceAll("([0-9]+:)|<|>", "")
        .split("\\.", 0)
      var temp = this.asInstanceOf[BlockTrait]
      for (j <- 1 until moduleName.size - 2) {
        temp = temp(moduleName(j))
      }
      val module = temp.getElement(moduleName(moduleName.size - 2))
      reconfigModuleArrays(tempII).append(module)

      if (II == 1) {
        infoArrays(tempII).append(moduleName(moduleName.size - 1))
        infoArrays(tempII).append(infos(i * 3 + 1))
        infoArrays(tempII).append(infos(i * 3 + 2))
      } else {
        val mode = module.mode
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
    }

    for (rc <- 0 until II) {
      resetConfigs()
      val infoArray = infoArrays(rc)
      for (i <- 0 until infoArray.size / 3) {
        val offset = i * 3

        val module = reconfigModuleArrays(rc)(i)
        val second = infoArray(offset + 1)
        if (second == "SELECTED_OP") {
          val opcode = infoArray(offset + 2).toInt
          module.updateConfig(opcode)
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
            module.updateConfig(fanInNums, fanOutNums, internalNum, rc)
          }
          module.configArray
        }
      }

      //Update configurations of const units.
      if (constInfo != null) {
        if (constInfo.IDArray.size > 0) {
          for (j <- 0 until constInfo.IDArray(rc).size) {
            val constID = constInfo.IDArray(rc)(j)
            val constVal = constInfo.configArray(rc)(j)
            this.ConstsArray(constID).asInstanceOf[ElementConst].updateConfigArray(constVal)
          }
        }
      }


      //Update configurations of counters.
      if (counterInfo != null) {
        if (counterInfo.IDArray.size > 0) {
          for (j <- 0 until counterInfo.IDArray(rc).size) {
            val ID = counterInfo.IDArray(rc)(j)
            val value = counterInfo.configArray(rc)(j)
            this.CountersArray(ID).asInstanceOf[ElementCounter].updateConfigArray(value)
          }
        }
      }
      retBitstreams.append(getConfigBitStream())
    }
    retBitstreams.toArray
  }

  /** Get schedules of ALUs and LSUs.
   *
   * @return the schedules of ALUs and LSUs
   */
  def getSchedules(): List[Int] = {
    var schedules = ALUsArray.map(alu => alu.getSchedule().toList).reduce(_ ++ _)
    if (LSUsArray.size > 0) {
      schedules = schedules ++ LSUsArray.map(lsu => lsu.getSchedule().toList).reduce(_ ++ _)
    }
    if (CountersArray.size > 0) {
      schedules = schedules ++ CountersArray.map(counter => counter.getSchedule().toList).reduce(_ ++ _)
    }
    schedules
  }
}
