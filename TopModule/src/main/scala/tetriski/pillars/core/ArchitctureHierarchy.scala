package tetriski.pillars.core

import java.io.{File, PrintWriter}

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
    new PillarsModuleInfo(moduleNums, moduleParams)
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

  def genConfig(filename :String): Unit ={
    val infos = Source.fromFile(filename).getLines()
    val infoArray = ArrayBuffer[String]()

    for(info <- infos){
      infoArray.append(info)
    }

//    val routeConfigGroup = ArrayBuffer[List[String]]()
//    val funConfigGroup = Map[ModuleTrait, Int]

    for(i <- 0 until infoArray.size/3){
      val offset = i * 3
      val moduleName = infoArray(offset).substring(1, infoArray(offset).size-1).split("\\.", 0)
      var temp = this.asInstanceOf[BlockTrait]
      for(j <- 1 until moduleName.size - 2){
        temp = temp(moduleName(j))
      }
      val module = temp.getModule(moduleName(moduleName.size - 2))
      val second = infoArray(offset+1)
      if(second == "SELECTED_OP"){
        val opcode = infoArray(offset + 2).toInt
        module.updateConfig(opcode)
       // println(opcode)
      }else{
        val fanInNum = second(second.size-1).toString.toInt
        val fanOutNode = infoArray(offset + 2)
        val fanOutNum = fanOutNode(fanOutNode.size-1).toString.toInt
        val internalNode = moduleName(moduleName.size - 1)
        val internalNum = internalNode(internalNode.size-1).toString.toInt
        module.updateConfig(fanInNum, fanOutNum, internalNum)
        module.configArray
      }
    }

    println(infoArray)
  }
}