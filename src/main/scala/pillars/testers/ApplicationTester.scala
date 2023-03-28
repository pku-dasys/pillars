package pillars.testers

import chisel3.{UInt, assert}
import chisel3.iotesters.PeekPokeTester
import pillars.core.{ArchitectureHierarchy, RuntimeInfo, SimulationHelper}
import pillars.hardware.PillarsConfig._
import pillars.hardware.{TokenIO, TopModule}
import pillars.util.SplitOrConcat

import scala.collection.mutable.ArrayBuffer

/** A class which is helpful when creating testers.
 * Since configuration controllers can repeat stored configurations every II cycles,
 * the "throughput" indicates after how many times of such repeats, a new result can be obtained.
 * For example, if II = 2 and throughput = 2, we can obtain a new result every 4 cycles.
 * However, it is not recommended to use this parameter, and we suggest users to
 * update the targeted architecture or mapping parameters.
 *
 * The "outputCycle" must be exactly right when the results are verified at output
 * ports of the top module during the activating process, while if only the
 * data obtained from LSUs is needed to be verified, it should be large enough.
 *
 * @param testII the targeted II
 */
class AppTestHelper(testII: Int) {
  /** An array of configurations.
   */
  var bitStreams: Array[BigInt] = null

  /** A list of modules' schedules.
   */
  var schedules: List[Int] = null

  /** The number of counters in the architecture.
   */
  var counterNum = 0

  /** A constructor function
   *
   * @param bitStreams the configurations
   * @param schedules  the schedules of modules
   * @param testII     the targeted II
   */
  def this(bitStreams: Array[BigInt], schedules: List[Int],
           testII: Int, counterNum: Int = 0) = {
    this(testII)
    this.bitStreams = bitStreams
    this.schedules = schedules
    this.counterNum = counterNum
  }

  /** Initialize this class
   *
   * @param arch               the architecture under test
   * @param simulationHelper   the class that helps users to automatically generate simulation codes
   * @param moduleInfoFilename the file name of behavioral modeling information TXT
   * @param     runtimeInfo    the runtime information
   */
  def init(arch: ArchitectureHierarchy, simulationHelper: SimulationHelper,
           moduleInfoFilename: String, runtimeInfo: RuntimeInfo): Unit = {
    val constInfo = simulationHelper.constInfo
    val counterInfo = simulationHelper.counterInfo
    counterNum = arch.CountersArray.size
    bitStreams = arch.genConfig(moduleInfoFilename, testII, constInfo, counterInfo)
    //NOTE: getSchedules should be called after genConfig if ALUs are allowed to perform bypass.
    schedules = arch.getSchedules()

    //Set cycles when we can put data through the input ports or get the result from the output ports,
    // which can be obtained from "*_r.txt"
    setPortCycle(simulationHelper)

    if (runtimeInfo != null) {
      var inputDataMap = Map[List[Int], Array[Int]]()
      runtimeInfo.inputToSRAM.foreach(i => inputDataMap += List(i.SRAMID, i.offset) -> i.data.toArray)
      addInData(inputDataMap)

      var outputDataMap = Map[List[Int], Array[Int]]()
      runtimeInfo.outputFromSRAM.foreach(i => outputDataMap += List(i.SRAMID, i.offset) -> i.expectedData.toArray)
      addOutData(outputDataMap)

      //Please make sure that the name of those ports are "input_[0-9]+" or "out_[0-9]+"
      // when using simulationHelper.outPorts and simulationHelper.inputPorts.
      val inputPorts = simulationHelper.inputPorts
      var inputToPortMap = Map[Int, Array[Int]]()
      for (i <- 0 until inputPorts.size) {
        inputToPortMap += inputPorts(i) -> runtimeInfo.inputToPort(i).data.toArray
      }
      setInputPortData(inputToPortMap)

      val outPorts = simulationHelper.outPorts
      var outFromPortMap = Map[Int, Array[Int]]()
      for (i <- 0 until outPorts.size) {
        outFromPortMap += outPorts(i) -> runtimeInfo.outputFromPort(i).expectedData.toArray
      }
      setOutPortRefs(outFromPortMap)
    }

  }

  /** A map between a list and the input data array.
   * The list consists of the identification number of targeted LSU and base address.
   */
  var inDataMap = Map[List[Int], Array[Int]]()

  /** A map between a list and the expected output data array.
   * The list consists of the identification number of targeted LSU and base address.
   */
  var outDataMap = Map[List[Int], Array[Int]]()

  /** A map between the targeted output port and the expected data array.
   */
  var outPortRefs = Map[Int, Array[Int]]()

  /** A map between the targeted input port and the input data array.
   */
  var inputPortData = Map[Int, Array[Int]]()

  /** The cycle we can obtain the result.
   */
  var outputCycle = testII + 1

  /** A map between the index of a opNode with "output" opcode and the ID of output port.
   */
  var outputPortCycleMap = Map[Int, Int]()

  /** A map between the index of a opNode with "input" opcode and the ID of input port.
   */
  var inputPortCycleMap = Map[Int, Int]()

  /** A parameter indicating the throughput of mapping result.
   * The default value is 1, and it is not recommended to use this parameter.
   */
  var throughput = 1

  def setPortCycle(simulationHelper: SimulationHelper): Unit = {
    outputPortCycleMap = simulationHelper.outputPortCycleMap
    inputPortCycleMap = simulationHelper.inputPortCycleMap
    outputCycle = simulationHelper.outputCycle
  }

  /** Set the parameter indicating the throughput of mapping result.
   *
   * @param arg a parameter indicating the throughput of mapping result
   */
  def setThroughput(arg: Int): Unit = {
    throughput = arg
  }

  /** Get the parameter indicating the throughput of mapping result.
   *
   * @return a parameter indicating the throughput of mapping result
   */
  def getThroughput(): Int = {
    throughput
  }

  /** Set the cycle we can obtain the result.
   *
   * @param arg the cycle we can obtain the result
   */
  def setOutputCycle(arg: Int): Unit = {
    outputCycle = arg
  }

  /** Set the map between the targeted output port and the expected data array.
   */
  def setOutPortRefs(arg: Map[Int, Array[Int]]): Unit = {
    outPortRefs = arg
  }

  /** Set the map between the targeted input port and the input data array.
   */
  def setInputPortData(arg: Map[Int, Array[Int]]): Unit = {
    inputPortData = arg
  }

  /** Get the input data array.
   *
   * @param numLSU identification number of targeted LSU
   * @param base   the base address
   * @return the input data array
   */
  def getInData(numLSU: Int, base: Int): Array[Int] = {
    inDataMap(List(numLSU, base))
  }

  /** Add a input data array into the corresponding map.
   *
   * @param numLSU identification number of targeted LSU
   * @param base   the base address
   * @param inData the input data array
   */
  def addInData(numLSU: Int, base: Int, inData: Array[Int]): Unit = {
    inDataMap = inDataMap + (List(numLSU, base) -> inData)
  }

  /** Concat inDataMap with inDatas.
   *
   * @param inDatas a map between a list and the input data array
   */
  def addInData(inDatas: Map[List[Int], Array[Int]]): Unit = {
    inDataMap = inDataMap ++ inDatas
  }

  /** Get the expected output data array.
   *
   * @param numLSU identification number of targeted LSU
   * @param base   the base address
   * @return the expected output data array
   */
  def getOutData(numLSU: Int, base: Int): Array[Int] = {
    outDataMap(List(numLSU, base))
  }

  /** Add a expected output data array into the corresponding map.
   *
   * @param numLSU  identification number of targeted LSU
   * @param base    the base address
   * @param outData the expected output data array
   */
  def addOutData(numLSU: Int, base: Int, outData: Array[Int]): Unit = {
    outDataMap = outDataMap + (List(numLSU, base) -> outData)
  }

  /** Concat outDataMap with outDatas.
   *
   * @param outDatas a map between a list and the output data array
   */
  def addOutData(outDatas: Map[List[Int], Array[Int]]): Unit = {
    outDataMap = outDataMap ++ outDatas
  }

  /** Get the configurations.
   *
   * @return the configurations
   */
  def getBitStreams(): Array[BigInt] = {
    bitStreams
  }

  /** Get the schedules.
   *
   * @return the schedules
   */
  def getSchedules(): List[Int] = {
    schedules
  }

  /** Get the schedules as BigInt.
   *
   * @return the schedules as BigInt
   */
  def getSchedulesBigInt(): Array[BigInt] = {
    var ret: Array[BigInt] = new Array[BigInt](II_UPPER_BOUND)
    for (ii <- 0 until II_UPPER_BOUND) {
      ret(ii) = BigInt(0)
    }
    val sches = schedules.reverse
    for (j <- 0 until sches.size / II_UPPER_BOUND) {
      var scheWidth = LOG_SCHEDULE_SIZE + SKEW_WIDTH
      if (j < counterNum) {
        scheWidth = LOG_SCHEDULE_SIZE
      }
      for (ii <- 0 until II_UPPER_BOUND) {
        val sche = sches(j * II_UPPER_BOUND + ii)
        ret(ii) = (ret(ii) << scheWidth) + sche
      }
    }
    ret.reverse
  }

  /** Get the targeted II.
   *
   * @return the targeted II
   */
  def getTestII(): Int = {
    testII
  }

  /** Get the cycle we can obtain the result.
   *
   * @return the cycle we can obtain the result
   */
  def getOutputCycle(): Int = {
    outputCycle
  }

  /** Get the map between the targeted output port and the expected data array.
   *
   * @return the map between the targeted output port and the expected data array
   */
  def getOutPortRefs(): Map[Int, Array[Int]] = {
    outPortRefs
  }

  /** Get the map between the targeted input port and the input data array.
   *
   * @return the map between the targeted input port and the input data array
   */
  def getInputPortData(): Map[Int, Array[Int]] = {
    inputPortData
  }
}

/** A base class of testers for applications.
 * It help users to construct the simulation processes and produce classes in the
 * specific format of Chisel testers using the Verilator backend.
 * We suggest users test their own CGRA architectures with this packed class.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class ApplicationTester(c: TopModule, appTestHelper: AppTestHelper) extends PeekPokeTester(c) {
  /** Translate a signed Int as unsigned BigInt.
   * Since the data type the output port of our top design is unsigned,
   * a translation is needed in some test cases.
   *
   * @param signedInt the signed Int
   * @return the unsigned BigInt
   */
  def asUnsignedInt(signedInt: Int): BigInt = (BigInt(signedInt >>> 1) << 1) + (signedInt & 1)

  //  def dataConcat(inData: Array[Int], factor: Int): Array[BigInt] ={
  //    val result = ArrayBuffer[BigInt]()
  //    for(i <- 0 until inData.size / factor){
  //      result.append(BigInt((0 until factor).map(j => inData(i*factor + j)
  //        .toHexString).reduce(_ + _), 16))
  //    }
  //    result.toArray
  //  }

  /** Enters data into a LSU.
   *
   * @param numInLSU the identification number of this LSU
   * @param inData   the input data array
   * @param base     the base address
   */
  def enqData(numInLSU: Int, inData: Array[Int], base: Int): Unit = {
    poke(c.io.startLSU(numInLSU), 1)
    poke(c.io.enqEnLSU(numInLSU), 1)
    poke(c.io.streamInLSU(numInLSU).valid, 0)
    poke(c.io.baseLSU(numInLSU), base)
    step(1)

    //    val manip = c.LSUs(0).memWrapper.enq_mem.manip
    //    val inputs = manip.mode match {
    //      case SplitOrConcat.Normal =>
    //        inData.map(i => BigInt(i))
    //      case SplitOrConcat.Split =>
    //        dataConcat(inData, manip.factor)
    //      case SplitOrConcat.Concat =>
    //        inData.map(i => BigInt(i))
    //    }

    // push
    for (x <- inData) {
      poke(c.io.streamInLSU(numInLSU).valid, 1)
      expect(c.io.streamInLSU(numInLSU).valid, 1)
      poke(c.io.streamInLSU(numInLSU).bits, x)
      if (peek(c.io.streamInLSU(numInLSU).ready) == 0) {
        while (peek(c.io.streamInLSU(numInLSU).ready) == 0) {
          step(1)
        }
      } else {
        step(1)
      } // exit condition: (c.io.in.ready === true.B) and step()
    }
    poke(c.io.streamInLSU(numInLSU).valid, 0)

    // exec
    while (peek(c.io.idleLSU(numInLSU)) == 0) {
      step(1)
    }

    poke(c.io.enqEnLSU(numInLSU), 0)
  }

  /** Verifies data in a LSU during the post-process.
   *
   * @param numInLSU the identification number of this LSU
   * @param refArray the expected output data array
   * @param base     the base address
   */
  def deqData(numInLSU: Int, refArray: Array[Int], base: Int): Unit = {
    // exec
    poke(c.io.startLSU(numInLSU), 1)
    poke(c.io.baseLSU(numInLSU), base)
    poke(c.io.lenLSU(numInLSU), refArray.length)
    poke(c.io.deqEnLSU(numInLSU), 1)
    step(1)
    poke(c.io.startLSU(numInLSU), 0)

    for (i <- 0 until refArray.length) {
      poke(c.io.streamOutLSU(numInLSU).ready, 1)
      if (peek(c.io.streamOutLSU(numInLSU).valid) == 0) {
        while (peek(c.io.streamOutLSU(numInLSU).valid) == 0) {
          poke(c.io.streamOutLSU(numInLSU).ready, 1)
          step(1)
        }
      }
      expect(c.io.streamOutLSU(numInLSU).bits, asUnsignedInt(refArray(i)))
      println(asUnsignedInt(refArray(i)).toString + " " + peek(c.io.streamOutLSU(numInLSU).bits).toString())
      step(1)
    }

    poke(c.io.deqEnLSU(numInLSU), 0)
  }

  /** Enters data into LSUs under the guide of inDataMap in appTestHelper.
   */
  def inputData(): Unit = {
    for (inDataItem <- appTestHelper.inDataMap) {
      val numInLSU = inDataItem._1(0)
      val base = inDataItem._1(1)
      val inData = inDataItem._2
      enqData(numInLSU, inData, base)
    }
  }

  /** Set the configurations and schedules in the pre-process.
   *
   * @param testII the targeted II
   */
  def inputConfig(testII: Int): Unit = {
    val schedules = appTestHelper.getSchedulesBigInt()
    val bitStreams = appTestHelper.getBitStreams()

    poke(c.io.enConfig, 1)
    poke(c.io.II, testII)
    //    poke(c.io.schedules, schedules)

    for (i <- 0 until testII) {
      poke(c.io.configuration, bitStreams(i))
      poke(c.io.schedules, schedules(i))
      step(1)
    }
  }

  /** Verifies data in output ports during the activating process.
   *
   * @param testII the targeted II
   */
  def checkPortOuts(testII: Int): Unit = {
    val refs = appTestHelper.getOutPortRefs()
    val throughput = appTestHelper.getThroughput()
    if (throughput > 1) {
      step((throughput - 1) * testII)
    }
    for (ref <- refs) {
      for (i <- ref._2) {
        val outputData = if (USE_TOKEN) {
          c.io.outs(ref._1).asInstanceOf[TokenIO].data
        } else {
          c.io.outs(ref._1).asInstanceOf[UInt]
        }
        expect(outputData, asUnsignedInt(i))
        println(asUnsignedInt(i).toString + " " + peek(outputData).toString())
        step(testII * throughput)
      }
    }
  }

  /** Verifies data in output ports during the activating process
   * when transferring data through the input ports.
   * This function is correct when appTestHelper.getThroughput() is 1.
   *
   * @param testII the targeted II
   */
  def checkPortOutsWithInput(testII: Int): Unit = {
    val refs = appTestHelper.getOutPortRefs()
    val outputCycle = appTestHelper.getOutputCycle()
    val inputDataMap = appTestHelper.getInputPortData()
    val outputPortCycleMap = appTestHelper.outputPortCycleMap
    val inputPortCycleMap = appTestHelper.inputPortCycleMap

    println("Checking the results from the output port(s) of the CGRA.")

    //Wait till the configuration controllers are ready.
    step(testII + 1)

    val dataSize = refs.toArray.last._2.size
    val T = (dataSize + outputCycle / testII) * testII
    for (t <- 0 until T) {
      for (port <- inputDataMap.keys) {
        val data = inputDataMap(port)
        val cycle = inputPortCycleMap(port)
        if (t >= cycle && t < cycle + dataSize * testII) {
          if (cycle % testII == t % testII) {
            val inputData = if (USE_TOKEN) {
              c.io.inputs(port).asInstanceOf[TokenIO].data
            } else {
              c.io.inputs(port).asInstanceOf[UInt]
            }
            poke(inputData, data((t - cycle) / testII))
            if(USE_TOKEN){
              poke(c.io.inputs(port).asInstanceOf[TokenIO].token, true)
            }
          }else{
            if(USE_TOKEN){
              poke(c.io.inputs(port).asInstanceOf[TokenIO].token, false)
            }
          }
        }
      }
      for (port <- refs.keys) {
        val data = refs(port)
        val cycle = outputPortCycleMap(port)
        if (t >= cycle && t < cycle + dataSize * testII) {
          if (cycle % testII == t % testII) {
            val outputData = if (USE_TOKEN) {
              c.io.outs(port).asInstanceOf[TokenIO].data
            } else {
              c.io.outs(port).asInstanceOf[UInt]
            }
            expect(outputData, asUnsignedInt(data((t - cycle) / testII)))
            var outputStr = asUnsignedInt(data((t - cycle) / testII)).toString + " " + peek(outputData).toString()
            if(USE_TOKEN){
              outputStr = "TOKEN: " + peek(c.io.outs(port).asInstanceOf[TokenIO].token).toString() +
                ", DATA: " + outputStr
            }
            println(outputStr)
          }
        }
      }
      step(1)
    }
  }

  /** Verifies data in LSUs under the guide of outDataMap in appTestHelper during the post-process.
   */
  def checkLSUData(): Unit = {
    println("Checking the results stored in the SRAM(s) of the CGRA.")
    //stream deq test
    for (inDataItem <- appTestHelper.outDataMap) {
      val numInLSU = inDataItem._1(0)
      val base = inDataItem._1(1)
      val refArray = inDataItem._2
      deqData(numInLSU, refArray, base)
    }
  }
}

/** A tester for sum.
 * The post-process is not necessary since there are no store operations in the targeted DFG.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class SumTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c, appTestHelper) {
  poke(c.io.en, 0)
  inputData()
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)

  val outputCycle = appTestHelper.getOutputCycle()
  step(outputCycle + 1)

  //  checkPortOutsWithInput(testII)
  checkPortOuts(testII)
  //  checkLSUData()
}

/** A tester for accumulate.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class AccumTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c, appTestHelper) {

  poke(c.io.en, 0)
  inputData()
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)

  val outputCycle = appTestHelper.getOutputCycle()
  step(outputCycle + 1)

  checkPortOuts(testII)
  checkLSUData()

}

/** A tester for vadd.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class VaddTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c, appTestHelper) {

  poke(c.io.en, 0)
  inputData()
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)

  val outputCycle = appTestHelper.getOutputCycle()
  step(outputCycle + 1)

  checkPortOuts(testII)
  checkLSUData()

}

/** A tester for cap.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class CapTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c, appTestHelper) {

  poke(c.io.en, 0)
  inputData()
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)

  val outputCycle = appTestHelper.getOutputCycle()
  step(outputCycle + 1)

  checkLSUData()
}
