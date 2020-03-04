package tetriski.pillars.core

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class ValidationHelper(arch: ArchitctureHierarchy) {
  var size =0
  val opArray = new ArrayBuffer[String]()
  val moduleArray = new ArrayBuffer[ModuleTrait]()
  val IIArray = new ArrayBuffer[Int]()
  val waitCycleArray = new ArrayBuffer[Int]()
  val skewArray = new ArrayBuffer[Int]()
  val constInfo = new ConstInfo()
  val outPorts = new ArrayBuffer[Int]()
  var outputCycle = 0

  def addResult(result: String): Unit ={
    val tempList = result.split(" ").toList
    val op = tempList(0)
    opArray.append(op)

    val moduleName = tempList(1).replaceAll("([0-9]+:)|<|>", "").split("\\.", 0)
    var temp = arch.asInstanceOf[BlockTrait]
    for (j <- 1 until moduleName.size - 2) {
      temp = temp(moduleName(j))
    }
    if(op.contains("output") || op.contains("input")){
      moduleArray.append(arch)
      if(op.contains("output")){
        var tempStr = ("out_[0-9]+".r findFirstIn tempList(1)).toArray
        tempStr = ("[0-9]+".r findFirstIn tempStr(0)).toArray
        val port = tempStr(0).toInt
        outPorts.append(port)
      }
    }else{
      val module = temp.getModule(moduleName(moduleName.size - 2))
      moduleArray.append(module)
    }

    val pattern = "[0-9]+:".r
    val tempStr = (pattern findFirstIn tempList(1)).toArray
    val II = tempStr(0).replace(":","").toInt
    IIArray.append(II)

    var waitCycle = tempList(2).toInt
    waitCycleArray.append(waitCycle)
    if(op.contains("output")){
      outputCycle = waitCycle + 1
    }

    val skew = tempList(3).toInt
    skewArray.append(skew)
  }

  def reset(): Unit ={
    size = 0
    opArray.clear()
    moduleArray.clear()
    IIArray.clear()
    waitCycleArray.clear()
    skewArray.clear()
    outPorts.clear()
  }

  def init(resultFilename: String): Unit ={
    reset()
    val resultArray = Source.fromFile(resultFilename).getLines().toArray
    resultArray.map(r => addResult(r))
    size = opArray.size
  }

  def getSchedules(): List[Int] ={
    arch.resetSchedules()
    for(i <- 0 until size){
      moduleArray(i).setSkew(skewArray(i), IIArray(i))
      moduleArray(i).setWaitCycle(waitCycleArray(i), IIArray(i))
    }
    arch.getSchedules()
  }

  def setConst(vals: Array[Int], testII: Int): Unit ={
    constInfo.reset(testII)
    var valIndex = 0
    for(i <- 0 until size){
      val module = moduleArray(i)
      if(module.getTypeID() == 3){
        constInfo.addConst(module.getModuleID(), IIArray(i), vals(valIndex))
        valIndex = valIndex + 1
      }
    }
  }

  def getConstInfo(): ConstInfo ={
    constInfo
  }

  def getDataWithAddr(dataSize: Int = 0, addrArray: Array[Int] = null, inDataArrays: Array[Array[Int]] = null,
                      outDataArrays: Array[Array[Int]] = null, refDataArrays: Array[Array[Int]] = null): Array[Any] = {
    val inDataAddr = new ArrayBuffer[List[Int]]()
    val outDataAddr = new ArrayBuffer[List[Int]]()
    val refDataAddr = new ArrayBuffer[Int]()
    var inDatas = Map[List[Int], Array[Int]]()
    var outDatas = Map[List[Int], Array[Int]]()
    var refDatas = Map[Int, Array[Int]]()
    var index = 0
    var p = 0
    if(addrArray != null) {
      for (i <- 0 until size) {
        val moduleID = moduleArray(i).getModuleID()
        val op = opArray(i)
        if (op.contains("load")) {
          inDataAddr.append(List(moduleID, addrArray(index)))
          index += 1
        } else if (op.contains("store")) {
          outDataAddr.append(List(moduleID, addrArray(index)))
          index += 1
        } else if (op.contains("output")) {
          refDataAddr.append(outPorts(p))
          index += 1
          p += 1
        }
      }
    }
    else{//need to be update
      var addrMap = Map[Int, Int]()
      var addr = 0
      for(i <- 0 until size){
        val moduleID = moduleArray(i).getModuleID()
        val op = opArray(i)
        if(op.contains("load")){
          if(addrMap.contains(moduleID)){
            addr = addrMap(moduleID)
          }else{
            addr = 0
          }
          inDataAddr.append(List(moduleID, addr))
          addrMap = addrMap + (moduleID -> (addr + dataSize))
        }else if(op.contains("store")){
          if(addrMap.contains(moduleID)){
            addr = addrMap(moduleID)
          }else{
            addr = 0
          }
          outDataAddr.append(List(moduleID, addr))
          addrMap = addrMap + (moduleID -> (addr + dataSize))
        }else if(op.contains("output")){
          refDataAddr.append(outPorts(p))
          p = p + 1
        }
      }
    }
    for(i <- 0 until inDataAddr.size){
      inDatas = inDatas ++ Map(inDataAddr(i) -> inDataArrays(i))
    }
    for(i <- 0 until outDataAddr.size){
      outDatas = outDatas ++ Map(outDataAddr(i) -> outDataArrays(i))
    }
    for(i <- 0 until refDataAddr.size){
      refDatas = refDatas ++ Map(refDataAddr(i) -> refDataArrays(i))
    }
    Array(inDatas, outDatas, refDatas)
  }

  //get outputCycle for output opcode
  def getOutputCycle(): Int ={
    outputCycle
  }
}
