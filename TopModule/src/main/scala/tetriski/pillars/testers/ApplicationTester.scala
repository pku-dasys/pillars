package tetriski.pillars.testers

import chisel3.iotesters.PeekPokeTester
import tetriski.pillars.hardware.PillarsConfig._
import tetriski.pillars.hardware.TopModule

/** A class which is helpful when creating testers.
 * Since configuration controllers can repeat stored configurations every II cycles,
 * the "throughput" indicates after how many times of such repeats, a new result can be obtained.
 * For example, if II = 2 and throughput = 2, we can obtain a new result every 4 cycles.
 * However, it is not recommended to use this parameter, and we suggest users to
 * update the target architecture or mapping parameters.
 *
 * The "outputCycle" must be exactly right when the results are verified at output
 * ports of the top module during the activating process, while if only the
 * data obtained from LSUs is needed to be verified, it should be large enough.
 *
 * @param bitStreams the configurations
 * @param schedules  the schedules of modules
 * @param testII     the target II
 */
class AppTestHelper(bitStreams: Array[BigInt], schedules: List[Int],
                    testII: Int) {
  /** A map between a list and the input data array.
   * The list consists of the serial number of target LSU and base address.
   */
  var inDataMap = Map[List[Int], Array[Int]]()

  /** A map between a list and the expected output data array.
   * The list consists of the serial number of target LSU and base address.
   */
  var outDataMap = Map[List[Int], Array[Int]]()

  /** A map between the target output port and the expected data array.
   */
  var outPortRefs = Map[Int, Array[Int]]()

  /** The cycle we can obtain the result.
   */
  var outputCycle = testII + 1

  /** A parameter indicating the throughput of mapping result.
   * The default value is 1, and it is not recommended to use this parameter.
   */
  var throughput = 1

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

  /** Set the map between the target output port and the expected data array.
   */
  def setOutPortRefs(arg: Map[Int, Array[Int]]): Unit = {
    outPortRefs = arg
  }

  /** Get the input data array.
   *
   * @param numLSU serial number of target LSU
   * @param base   the base address
   * @return the input data array
   */
  def getInData(numLSU: Int, base: Int): Array[Int] = {
    inDataMap(List(numLSU, base))
  }

  /** Add a input data array into the corresponding map.
   *
   * @param numLSU serial number of target LSU
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
   * @param numLSU serial number of target LSU
   * @param base   the base address
   * @return the expected output data array
   */
  def getOutData(numLSU: Int, base: Int): Array[Int] = {
    outDataMap(List(numLSU, base))
  }

  /** Add a expected output data array into the corresponding map.
   *
   * @param numLSU  serial number of target LSU
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
  def getSchedulesBigInt(): BigInt = {
    var ret: BigInt = 0
    for (sche <- schedules.reverse) {
      ret = (ret << LOG_SCHEDULE_SIZE * 2 + 1) + sche
    }
    ret
  }

  /** Get the target II.
   *
   * @return the target II
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

  /** Get the map between the target output port and the expected data array.
   *
   * @return the map between the target output port and the expected data array
   */
  def getOutPortRefs(): Map[Int, Array[Int]] = {
    outPortRefs
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

  /** Enters data into a LSU.
   *
   * @param numInLSU the serial number of this LSU
   * @param inData   the input data array
   * @param base     the base address
   */
  def enqData(numInLSU: Int, inData: Array[Int], base: Int): Unit = {
    poke(c.io.startLSU(numInLSU), 1)
    poke(c.io.enqEnLSU(numInLSU), 1)
    poke(c.io.streamInLSU(numInLSU).valid, 0)
    poke(c.io.baseLSU(numInLSU), base)
    step(1)

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
   * @param numInLSU the serial number of this LSU
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
   * @param testII the target II
   */
  def inputConfig(testII: Int): Unit = {
    val schedules = appTestHelper.getSchedulesBigInt()
    val bitStreams = appTestHelper.getBitStreams()

    poke(c.io.enConfig, 1)
    poke(c.io.II, testII)
    poke(c.io.schedules, schedules)

    for (i <- 0 until testII) {
      poke(c.io.configuration, bitStreams(i))
      step(1)
    }
  }

  /** Verifies data in output ports during the activating process.
   *
   * @param testII the target II
   */
  def checkPortOuts(testII: Int): Unit = {
    val refs = appTestHelper.getOutPortRefs()
    val throughput = appTestHelper.getThroughput()
    if (throughput > 1) {
      step((throughput - 1) * testII)
    }
    for (ref <- refs) {
      for (i <- ref._2) {
        expect(c.io.outs(ref._1), asUnsignedInt(i))
        println(asUnsignedInt(i).toString + " " + peek(c.io.outs(ref._1)).toString())
        step(testII * throughput)
      }
    }
  }

  /** Verifies data in LSUs under the guide of outDataMap in appTestHelper during the post-process.
   */
  def checkLSUData(): Unit = {
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
 * The post-process is not necessary since there are no store operations in the target DFG.
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
  step(outputCycle)

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
  step(outputCycle)

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
  step(outputCycle)

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
  step(outputCycle)

  checkLSUData()
}
