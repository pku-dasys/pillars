package tetriski.pillars.core
import tetriski.pillars.testers.AppTestHelper

import sun.jvm.hotspot.oops.DataLayout

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/** A class that helps users to automatically generate simulation codes for an architecture.
 * A result TXT can be interpreted in this class.
 * The result TXT indicate the mapping result of DFG nodes,
 * which consists of the name of these nodes,
 * mapped nodes in a MRRG representing this architecture, fire time and skew.
 *
 * @param arch the abstract model of the top design
 */
class SimulationHelper(arch: ArchitctureHierarchy) {
  /** The size of the DFG. It is equal to the size of arrays in this class.
   */
  var size = 0

  /** The array of the DFG nodes.
   */
  val opArray = new ArrayBuffer[String]()

  /** The array of the constant values.
   */
  val constArray = new ArrayBuffer[Int]()

  /** The array of elements corresponding mapped MRRG nodes.
   */
  val moduleArray = new ArrayBuffer[ElementTrait]()

  /** The array of the reconfiguration cycle of mapped MRRG nodes.
   */
  val RCArray = new ArrayBuffer[Int]()

  /** The array of the fire time of mapped MRRG nodes.
   */
  val fireTimeArray = new ArrayBuffer[Int]()

  /** The array of the skew of mapped MRRG nodes.
   */
  val skewArray = new ArrayBuffer[Int]()

  /** A class containing const values, the corresponding RCs and identification number of const units.
   */
  val constInfo = new ConstInfo()

  /** The array of the identification number of the mapped output ports.
   */
  val outPorts = new ArrayBuffer[Int]()

  /** The array of the identification number of the mapped input ports.
   */
  val inputPorts = new ArrayBuffer[Int]()

  /** The cycle we can obtain the result.
   */
  var outputCycle = 0

  /** A map between the index of a opNode with "output" opcode and the ID of output port.
   */
  var outputPortCycleMap = Map[Int, Int]()

  /** A map between the index of a opNode with "input" opcode and the ID of input port.
   */
  var inputPortCycleMap = Map[Int, Int]()

  /** Add the mapping result of a mapped DFG node.
   *
   * @example If the input is "add0 0:cgra.tile_0.pe_3_3.alu0.internalNode_0 1 0",
   *          it means that the DFG node whose name is "add0" is mapped onto
   *          a MRRG node whose name is "0:cgra.tile_0.pe_3_3.alu0.internalNode_0".
   *          The reconfiguration cycle of this MRRG node is 0.
   *          The fire time is 1 and the skew is 0.
   * @param result a line in the result TXT indicating the mapping result of a mapped DFG node
   */
  def addResult(result: String): Unit = {
    val tempList = result.split(" ").toList

    //Add the name of the DFG node into opArray.
    val op = tempList(0)
    opArray.append(op)

    //Add the element corresponding the mapped MRRG node into moduleArray.
    val moduleName = tempList(1).replaceAll("([0-9]+:)|<|>", "").split("\\.", 0)
    var temp = arch.asInstanceOf[BlockTrait]
    for (j <- 1 until moduleName.size - 2) {
      temp = temp(moduleName(j))
    }
    if (op.contains("output") || op.contains("input")) {
      moduleArray.append(arch)
      if (op.contains("output")) {
        //Add the identification number of the mapped output ports into outPorts.
        var tempStr = ("out_[0-9]+".r findFirstIn tempList(1)).toArray
        tempStr = ("[0-9]+".r findFirstIn tempStr(0)).toArray
        val port = tempStr(0).toInt
        outPorts.append(port)
      } else if (op.contains("input")) {
        //Add the identification number of the mapped input ports into inputPorts.
        var tempStr = ("input_[0-9]+".r findFirstIn tempList(1)).toArray
        tempStr = ("[0-9]+".r findFirstIn tempStr(0)).toArray
        val port = tempStr(0).toInt
        inputPorts.append(port)
      }
    } else {
      val module = temp.getElement(moduleName(moduleName.size - 2))
      moduleArray.append(module)
    }

    //Add the reconfiguration cycle of the mapped MRRG node into RCArray.
    val pattern = "[0-9]+:".r
    val tempStr = (pattern findFirstIn tempList(1)).toArray
    val RC = tempStr(0).replace(":", "").toInt
    RCArray.append(RC)

    //Add the fire time of the mapped MRRG node into fireTimeArray.
    val fireTime = tempList(2).toInt
    fireTimeArray.append(fireTime)
    if (op.contains("output")) {
      outputPortCycleMap += outPorts.last -> fireTime
    } else if (op.contains("input")) {
      inputPortCycleMap += inputPorts.last -> fireTime
    }

    //Add the skew of the mapped MRRG node into skewArray.
    val skew = tempList(3).toInt
    skewArray.append(skew)
  }

  /** Add the mapping result of a mapped DFG node.
   *
   * @example If the input is "add0 0:cgra.tile_0.pe_3_3.alu0.internalNode_0 1 0",
   *          it means that the DFG node whose name is "add0" is mapped onto
   *          a MRRG node whose name is "0:cgra.tile_0.pe_3_3.alu0.internalNode_0".
   *          The reconfiguration cycle of this MRRG node is 0.
   *          The fire time is 1 and the skew is 0.
   * @param result a line in the result TXT indicating the mapping result of a mapped DFG node
   */
  def addResultNew(result: String): Unit = {
    val tempList = result.split(" ").toList

    //Add the name of the DFG node into opArray.
    val op = tempList(0)
    opArray.append(op)

    //Add the name of the DFG node into opArray.
    val constval = tempList(4).split(":").toList(1).toInt
    //println("Constval:" + constval)
    constArray.append(constval)

    //Add the element corresponding the mapped MRRG node into moduleArray.
    val moduleName = tempList(1).replaceAll("([0-9]+:)|<|>", "").split("\\.", 0)
    var temp = arch.asInstanceOf[BlockTrait]
    for (j <- 1 until moduleName.size - 2) {
      temp = temp(moduleName(j))
    }
    if (op.contains("output") || op.contains("input")) {
      moduleArray.append(arch)
      if (op.contains("output")) {
        //Add the identification number of the mapped output ports into outPorts.
        var tempStr = ("out_[0-9]+".r findFirstIn tempList(1)).toArray
        tempStr = ("[0-9]+".r findFirstIn tempStr(0)).toArray
        val port = tempStr(0).toInt
        outPorts.append(port)
      } else if (op.contains("input")) {
        //Add the identification number of the mapped input ports into inputPorts.
        var tempStr = ("input_[0-9]+".r findFirstIn tempList(1)).toArray
        tempStr = ("[0-9]+".r findFirstIn tempStr(0)).toArray
        val port = tempStr(0).toInt
        inputPorts.append(port)
      }
    } else {
      val module = temp.getElement(moduleName(moduleName.size - 2))
      moduleArray.append(module)
    }

    //Add the reconfiguration cycle of the mapped MRRG node into RCArray.
    val pattern = "[0-9]+:".r
    val tempStr = (pattern findFirstIn tempList(1)).toArray
    val RC = tempStr(0).replace(":", "").toInt
    RCArray.append(RC)

    //Add the fire time of the mapped MRRG node into fireTimeArray.
    val fireTime = tempList(2).toInt
    fireTimeArray.append(fireTime)
    if (op.contains("output")) {
      outputPortCycleMap += outPorts.last -> fireTime
    } else if (op.contains("input")) {
      inputPortCycleMap += inputPorts.last -> fireTime
    }

    //Add the skew of the mapped MRRG node into skewArray.
    val skew = tempList(3).toInt
    skewArray.append(skew)
  }

  /** Reset values in this class.
   */
  def reset(): Unit = {
    size = 0
    opArray.clear()
    moduleArray.clear()
    RCArray.clear()
    fireTimeArray.clear()
    skewArray.clear()
    outPorts.clear()
    outputPortCycleMap = Map[Int, Int]()
    inputPortCycleMap = Map[Int, Int]()
  }

  /** Initialize values in this class according to a result TXT.
   *
   * @param resultFilename the file name of the result TXT
   */
  def init(resultFilename: String): Unit = {
    reset()
    val resultArray = Source.fromFile(resultFilename).getLines().toArray
    resultArray.map(r => addResult(r))
    size = opArray.size
    //Set the cycle we can obtain the last result.
    if(outputPortCycleMap.size>0) {
      outputCycle = outputPortCycleMap.map(t => t._2)
        .reduce((t1, t2) => Math.max(t1, t2))
    }else{
      outputCycle = 0
    }
  }

  /** Initialize values in this class according to a result TXT.
   *
   * @param resultFilename the file name of the result TXT
   */
  def initNew(resultFilename: String): Unit = {
    reset()
    val resultArray = Source.fromFile(resultFilename).getLines().toArray
    resultArray.map(r => addResultNew(r))
    size = opArray.size
    //Set the cycle we can obtain the last result.
    if(outputPortCycleMap.size>0) {
      outputCycle = outputPortCycleMap.map(t => t._2)
        .reduce((t1, t2) => Math.max(t1, t2))
    }else{
      outputCycle = 0
    }
  }

  def bytesToInt(bytes: Array[Byte], littleEndian: Boolean): Int = {
    val buffer = java.nio.ByteBuffer.wrap(bytes)
    if(littleEndian) {buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt}
    else {buffer.getInt}
  }
  /** Create data map from morpher data trace and data layout.
   * morpher data layout is byte addressable
   * pillars memory enqdata method only support word (4 bytes) address
   *
   */
  def createDataMap(appTestHelper:AppTestHelper, testdataFilename: String, dataLayoutFilename: String
                    , dataMemDetailsFilename: String): Unit = {
    val testdataArray = Source.fromFile(testdataFilename).getLines().drop(1).toArray
    val dataLayoutArray = Source.fromFile(dataLayoutFilename).getLines().drop(1).toArray
    val dataMemDetails = Source.fromFile(dataMemDetailsFilename).getLines().toArray

//    val numofbanks = dataMemDetails(0).toInt
    val bankSizeInB = dataMemDetails(0).toInt


    // var name -> List[int offset, prerundata, postrundata]
    var dataMemContent = scala.collection.mutable.Map[String, List[List[Int]]]()
    var dataLayout = Map[String, Int]()
    for(var_base <- dataLayoutArray) {
//      println(var_base)
      val tempList = var_base.split(",").toList
      val varName = tempList(0).toString
      val baseAddr = tempList(1).toInt
      dataLayout = dataLayout + (varName -> baseAddr)
      dataMemContent += (varName -> Nil)
    }
//    println(dataLayout)

    for(testData <- testdataArray){
      val tempList = testData.split(",").toList
      val varName = tempList(0).toString
      val offset = tempList(1).toInt
      val prerundata = tempList(2).toInt
      val postrundata = tempList(3).toInt

      dataMemContent.get(varName) match {
        case Some(xs:List[List[Int]]) => dataMemContent.update(varName, xs :+ List(offset,prerundata,postrundata))
        case None => dataMemContent
      }
    }
    var inputDataMap = Map[List[Int], Array[Int]]()
    var expectedDataMap = Map[List[Int], Array[Int]]()
    var varNameList = List[String]()
    /*
    * dataMemContent is a map of var name -> List[int offset, prerundata, postrundata]
    * Loop through the offset,prerundata,postrundata tuples of each variable
    * and create the intDataMap  Map[List[Int], Array[Int]]()
    * In Morpher data trace, there are integer (4 byte) arrays/scalars
    * and also 1 byte scalars like loop start and loop end.
    * Also morpher data trace is a byte array.
    * Following loop creates the integer arrays from morpher byte trace and create inputDataMap.
    * */
    for(memCont <- dataMemContent){
//      println("var name:" + memCont._1 + ", var size in bytes:" + memCont._2.size)
      val varName = memCont._1
      // There are integer (4 byte) arrays/scalars and also 1 byte scalars like loop start and loop end.
      if(memCont._2.size % 4 == 0) {// For Int variables
        val inputData = new Array[Int](memCont._2.size/4)
        val expectedData = new Array[Int](memCont._2.size/4)
        var i = 0
        var j = 0
        val byteInData = new Array[Byte](4)
        val byteExpData = new Array[Byte](4)
        for (prepostdata <- memCont._2) {
          byteInData(i) = prepostdata(1).toByte
          byteExpData(i) = prepostdata(2).toByte
          i += 1
          if(i%4 == 0) {
            i = 0
            inputData(j) = bytesToInt(byteInData,true)
            expectedData(j) = bytesToInt(byteExpData,true)
            j += 1
          }
        }
        val LSUnum = (dataLayout(memCont._1)/bankSizeInB).floor.toInt
        val LSUbase = (dataLayout(memCont._1) - LSUnum*bankSizeInB)/4
        inputDataMap += (List(LSUnum, LSUbase) -> inputData)
        expectedDataMap += (List(LSUnum, LSUbase) -> expectedData)
        varNameList = varNameList :+ varName
//        println(LSUnum, LSUbase, inputData, expectedData)
      }
      else if(memCont._2.size == 1 ){
        // For Byte values such as loop start and loop end.
        // This will cause an error if the 4 byte values near the loop start byte are used by another variable since
        // this will zero out all other 3 bytes except the loop start byte
        val intData = new Array[Int](1)
        val base = dataLayout(memCont._1)
        val whichByte = base%4
        val byteData = new Array[Byte](4)
        byteData(whichByte) = memCont._2(0)(1).toByte
        intData(0) = bytesToInt(byteData,true)
        val LSUnum = (dataLayout(memCont._1)/bankSizeInB).floor.toInt
        val LSUbase = (dataLayout(memCont._1) - LSUnum*bankSizeInB)/4
        inputDataMap += (List(LSUnum, LSUbase) -> intData)
//        println(LSUnum, LSUbase, intData(0))
      }
      else{
        println("Not an int array/scalar or byte scalar. Exiting..")
        sys.exit(0)
      }
    }
//    println(inputDataMap)
    appTestHelper.addInData(inputDataMap)
    appTestHelper.addOutData(expectedDataMap)
    appTestHelper.addVarNameData(varNameList)
  }

  /** Get the schedules of the mapping result.
   *
   * @return the schedules of the mapping result
   */
  def getSchedules(): List[Int] = {
    arch.resetSchedules()
    for (i <- 0 until size) {
      moduleArray(i).setSkew(skewArray(i), RCArray(i))
      moduleArray(i).setFireTime(fireTimeArray(i), RCArray(i))
    }
    arch.getSchedules()
  }

  /** Set const values, the corresponding RC and identification number of a const unit.
   *
   * @param vals   the const values
   * @param testII the targeted II
   */
  def setConst(vals: Array[Int], testII: Int): Unit = {
    constInfo.reset(testII)
    var valIndex = 0
//    println("\nSet Const:  size " + size)
    for (i <- 0 until size) {
      val module = moduleArray(i)
//      println("module " + module)
      if (module.getTypeID() == 3) {
        constInfo.addConst(module.getModuleID(), RCArray(i), vals(valIndex))
//        println("module.getModuleID() " + module.getModuleID())
//        println("vals(valIndex) " + vals(valIndex))
        valIndex = valIndex + 1
      }
//      println("")
    }
  }

  /** Get the class containing const values, the corresponding RC and identification number of a const unit.
   *
   * @return the class containing const values, the corresponding RC and identification number of a const unit
   */
  def getConstInfo(): ConstInfo = {
    constInfo
  }

  /** Get data arrays with corresponding address.
   *
   * @param dataSize      the data size of a data array
   * @param addrArray     the address of the data arrays for LSUs
   * @param inDataArrays  the input data arrays for LSUs
   * @param outDataArrays the expected data arrays for LSUs
   * @param refDataArrays the expected data arrays for the output ports
   * @return Array(the input data arrays for LSUs with corresponding address,
   *         the expected data arrays for LSUs with corresponding address,
   *         the expected data arrays for the output ports with corresponding identification number)
   */
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

    if (addrArray != null) {
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
    else { //TODO: update the automatic inference of address for any situations.
      //It can not always work well in some situations currently.
      //So we suggest users to define address explicitly.
      var addrMap = Map[Int, Int]()
      var addr = 0
      for (i <- 0 until size) {
        val moduleID = moduleArray(i).getModuleID()
        val op = opArray(i)
        if (op.contains("load")) {
          if (addrMap.contains(moduleID)) {
            addr = addrMap(moduleID)
          } else {
            addr = 0
          }
          inDataAddr.append(List(moduleID, addr))
          addrMap = addrMap + (moduleID -> (addr + dataSize))
        } else if (op.contains("store")) {
          if (addrMap.contains(moduleID)) {
            addr = addrMap(moduleID)
          } else {
            addr = 0
          }
          outDataAddr.append(List(moduleID, addr))
          addrMap = addrMap + (moduleID -> (addr + dataSize))
        } else if (op.contains("output")) {
          refDataAddr.append(outPorts(p))
          p = p + 1
        }
      }
    }

    for (i <- 0 until inDataAddr.size) {
      inDatas = inDatas ++ Map(inDataAddr(i) -> inDataArrays(i))
    }
    for (i <- 0 until outDataAddr.size) {
      outDatas = outDatas ++ Map(outDataAddr(i) -> outDataArrays(i))
    }
    for (i <- 0 until refDataAddr.size) {
      refDatas = refDatas ++ Map(refDataAddr(i) -> refDataArrays(i))
    }
    Array(inDatas, outDatas, refDatas)
  }

  /** Get the cycle we can obtain the result.
   *
   * @return the cycle we can obtain the result
   */
  def getOutputCycle(): Int = {
    outputCycle
  }
}
