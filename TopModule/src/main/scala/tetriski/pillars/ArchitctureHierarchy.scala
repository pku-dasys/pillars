package tetriski.pillars

import java.io.{File, PrintWriter}

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
}