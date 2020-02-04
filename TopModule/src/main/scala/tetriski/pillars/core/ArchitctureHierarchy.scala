package tetriski.pillars.core

import java.io.{File, PrintWriter}

import tetriski.pillars.archlib.OpConst
import tetriski.pillars.hardware.PillarsModuleInfo

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

//This class describes the archtectures of the designed CGRA Demo
class ArchitctureHierarchy extends BlockTrait {

  setName("cgra")
  hierName.append(name)

  //Get integer module list.
  def getPillarsModuleInfo(): PillarsModuleInfo = {
    //var ret = List[List[Int]]()
    var moduleNums = List[Int]()
    var moduleParams = List[List[Int]]()
    for (i <- 0 until modulesArray.size) {
      moduleNums = moduleNums :+ modulesArray(i).size
      for (j <- 0 until modulesArray(i).size) {
        moduleParams = moduleParams :+ modulesArray(i)(j).asInstanceOf[ModuleTrait].getParams()
      }
    }
    new PillarsModuleInfo(moduleNums, moduleParams, inPorts.size, outPorts.size)
  }

  def getConfigList(): List[List[List[Int]]] = {
    var ret = List[List[List[Int]]]()
    def findConfigRegion(blockMap : Map[String, BlockTrait]): List[BlockTrait] ={
      var ret = List[BlockTrait]()
      for(subBlock <- blockMap.values){
        if(subBlock.isConfigRegion){
          ret = ret :+ subBlock
        }else{
          ret = ret ::: findConfigRegion(subBlock.blockMap)
        }
      }
      ret
    }
    val configRegions = findConfigRegion(blockMap)
    for (configRegion <- configRegions){
      var moduleList = List[List[Int]]()
      for (i <- 0 until configRegion.modulesArray.size) {
        for (j <- 0 until configRegion.modulesArray(i).size) {
          val module = configRegion.modulesArray(i)(j).asInstanceOf[ModuleTrait]
          if(module.getConfigBit()>0)
            moduleList = moduleList :+ List(module.getTypeID(), module.getModuleID())
        }
      }
      ret = ret :+ moduleList
    }
    ret
  }

  def getConfigBitStream(): BigInt = {
    var bitBuffer = ArrayBuffer[Int]()
    val configList = getConfigList()
    for (configRegionModules <- configList){
      for(configRegionModule <- configRegionModules){
        val typeID = configRegionModule(0)
        val moduleID = configRegionModule(1)
        val module = modulesArray(typeID)(moduleID).asInstanceOf[ModuleTrait]
        module.configArray.foreach(i => bitBuffer.append(i))
      }
    }
    bitBuffer = bitBuffer.reverse
    //bitBuffer.reverse
    var ret : BigInt = 0
    for(i <- 0 until bitBuffer.size){
      ret = ret << 1
      ret += bitBuffer(i)
    }
    ret
  }

  //After initialization, all module's ModuleID, also called global index,
  //is set as it's sequence number in relevant array of ArchitctureHierarchy
  def init(): Unit = {
    for (i <- 0 until modulesArray.size) {
      for (j <- 0 until modulesArray(i).size) {
        val module = modulesArray(i)(j).asInstanceOf[ModuleTrait]
        module.setModuleID(j)
      }
    }
    updateConnect()
  }

  //Save hierarchy information as modules.json
  def dumpArchitcture() = {
    val writer = new PrintWriter(new File("modules.json"))
    writer.flush()

    writer.println("{")
    printModules(writer)
    writer.println("}")

    writer.close()
  }

  def resetConfigs(): Unit ={
    for(moduleArray <- modulesArray){
      for(module <- moduleArray){
        module.asInstanceOf[ModuleTrait].updateConfigArray(0)
      }
    }
  }

  def genConfig(filename: String, II: Int, constInfo: ConstInfo = null): Array[BigInt] ={
    val retBitstreams = new ArrayBuffer[BigInt]()
    val infos = Source.fromFile(filename).getLines().toArray
    val infoArrays = new ArrayBuffer[ArrayBuffer[String]]()
    for(i <- 0 until II){
      val temp = new ArrayBuffer[String]()
      infoArrays.append(temp)
    }

    val pattern = "[0-9]+:".r

//    for(info <- infos){
//      infoArray.append(info)
//    }

    for(i <- 0 until (infos.size/3)){
      val targetStr = infos(i*3)
      val tempStr = (pattern findFirstIn targetStr).toArray
      val tempII = tempStr(0).replace(":","").toInt
      for(j <-0 until 3){
        infoArrays(tempII).append(infos(i * 3 + j))
      }
    }

//    val routeConfigGroup = ArrayBuffer[List[String]]()
//    val funConfigGroup = Map[ModuleTrait, Int]
    for(ii <- 0 until II) {
      resetConfigs()
      val infoArray = infoArrays(ii)
      for (i <- 0 until infoArray.size / 3) {
        val offset = i * 3
        // val moduleName = infoArray(offset).substring(1, infoArray(offset).size-1).split("\\.", 0)
        val moduleName = infoArray(offset).replaceAll("([0-9]+:)|<|>", "").split("\\.", 0)
        var temp = this.asInstanceOf[BlockTrait]
        for (j <- 1 until moduleName.size - 2) {
          temp = temp(moduleName(j))
        }
        val module = temp.getModule(moduleName(moduleName.size - 2))
        val second = infoArray(offset + 1)
        if (second == "SELECTED_OP") {
          val opcode = infoArray(offset + 2).toInt
          module.updateConfig(opcode)
          // println(opcode)
        }
        else {
          val fanInNums = second.split(" ").toList.map(i => i.toInt)
          //val fanInNum = second(second.size-1).toString.toInt
          val fanOutNode = infoArray(offset + 2)
          val fanOutNums = fanOutNode.split(" ").toList.map(i => i.toInt)
          val internalNodeName = moduleName(moduleName.size - 1)
          var internalNum = 0
          var isDecisive = false
          for (i <- 0 until module.internalNodes.size) {
            val iN = module.internalNodes(i)
            if (iN == internalNodeName) {
              internalNum = i
              isDecisive = true
            }
          }
          if (isDecisive) {
            module.updateConfig(fanInNums, fanOutNums, internalNum)
          }
          module.configArray
        }
      }
     for(j <- 0 until constInfo.constIDArray(ii).size){
       val constID = constInfo.constIDArray(ii)(j)
       val constVal = constInfo.constValArray(ii)(j)
       this.ConstsArray(constID).asInstanceOf[OpConst].updateConfigArray(constVal)
     }
      retBitstreams.append(getConfigBitStream())
    }
    retBitstreams.toArray
//    println(infoArray)
  }
}