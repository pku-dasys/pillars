package tetriski.pillars.testers

import chisel3.iotesters.PeekPokeTester
import tetriski.pillars.hardware.TopModuleWrapper

/** A base class of testers for applications when the top design has a wrapper.
 * It help users to construct the simulation processes and produce classes in the
 * specific format of Chisel testers using the Verilator backend.
 * We suggest users test their own CGRA architectures with this packed class.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class ApplicationWrapperTester(c: TopModuleWrapper, appTestHelper: AppTestHelper) extends PeekPokeTester(c) {
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
   * @param numInLSU the identity number of this LSU
   * @param inData   the input data array
   * @param base     the base address
   */
  def enqData(numInLSU: Int, inData: Array[Int], base: Int): Unit = {
    poke(c.io.startLSU, 1)
    poke(c.io.enqEnLSU, 1)
    poke(c.io.streamInLSU.valid, 0)
    poke(c.io.baseLSU, base)
    poke(c.io.LSUnitID, numInLSU)
    step(1)

    // push
    for (x <- inData) {
      poke(c.io.streamInLSU.valid, 1)
      expect(c.io.streamInLSU.valid, 1)
      poke(c.io.streamInLSU.bits, x)
      if (peek(c.io.streamInLSU.ready) == 0) {
        while (peek(c.io.streamInLSU.ready) == 0) {
          step(1)
        }
      } else {
        step(1)
      } // exit condition: (c.io.in.ready === true.B) and step()
    }
    poke(c.io.streamInLSU.valid, 0)

    // exec
    while (peek(c.io.idleLSU) == 0) {
      step(1)
    }

    poke(c.io.enqEnLSU, 0)
  }

  /** Verifies data in a LSU during the post-process.
   *
   * @param numInLSU the identity number of this LSU
   * @param refArray the expected output data array
   * @param base     the base address
   */
  def deqData(numInLSU: Int, refArray: Array[Int], base: Int): Unit = {
    // exec
    poke(c.io.startLSU, 1)
    poke(c.io.baseLSU, base)
    poke(c.io.lenLSU, refArray.length)
    poke(c.io.deqEnLSU, 1)
    poke(c.io.LSUnitID, numInLSU)
    step(1)
    poke(c.io.startLSU, 0)

    for (i <- 0 until refArray.length) {
      poke(c.io.streamOutLSU.ready, 1)
      if (peek(c.io.streamOutLSU.valid) == 0) {
        while (peek(c.io.streamOutLSU.valid) == 0) {
          poke(c.io.streamOutLSU.ready, 1)
          step(1)
        }
      }
      expect(c.io.streamOutLSU.bits, asUnsignedInt(refArray(i)))
      println(asUnsignedInt(refArray(i)).toString + " " + peek(c.io.streamOutLSU.bits).toString())
      step(1)
    }

    poke(c.io.deqEnLSU, 0)
  }

  /** Enters data into LSUs under the guide of inDataMap in appTestHelper.
   */
  def inputData(): Unit = {
    //input data into LSU
    for (inDataItem <- appTestHelper.inDataMap) {
      val numInLSU = inDataItem._1(0)
      val base = inDataItem._1(1)
      val inData = inDataItem._2
      enqData(numInLSU, inData, base)
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

  /** Set the configurations and schedules in the pre-process.
   * The target II is 1.
   */
  def inputConfig(): Unit = {
    /** Get a single bit of a input data at a given position.
     *
     * @param bigInt the input data
     * @param pos    the given position
     * @return 0 or 1
     */
    def getSingleBit(bigInt: BigInt, pos: Int): Int = {
      val mask = (1: BigInt) << pos
      val bit = (bigInt & mask) >> pos
      bit.toInt
    }

    val schedules = appTestHelper.getSchedulesBigInt()
    val bitStreams = appTestHelper.getBitStreams()

    val size = Math.max(c.topModule.io.configuration.getWidth, c.topModule.io.schedules.getWidth)
    //only support II = 1
    for (i <- 0 until size) {
      val schedulePos = c.topModule.io.schedules.getWidth - i - 1
      val configPos = c.topModule.io.configuration.getWidth - i - 1
      if (schedulePos >= 0) {
        poke(c.io.singleBitSchedule, getSingleBit(schedules, schedulePos))
      }
      if (configPos >= 0) {
        poke(c.io.singleBitConfig, getSingleBit(bitStreams(0), configPos))
      }
      step(1)
    }

  }
}

/** A tester for vadd.
 *
 * @param c             the top design with a wrapper
 * @param appTestHelper the class which is helpful when creating testers
 */
class VaddWrapperTester(c: TopModuleWrapper, appTestHelper: AppTestHelper)
  extends ApplicationWrapperTester(c, appTestHelper) {

  poke(c.io.en, 0)
  inputConfig()
  inputData()
  val testII = appTestHelper.getTestII()
  poke(c.io.II, testII)
  poke(c.io.enConfig, 1)
  //wait cgra read configuration and schedule
  step(testII)
  poke(c.io.en, 1)

  val outputCycle = appTestHelper.getOutputCycle()
  step(outputCycle)

  checkLSUData()

}

/** A tester for sum.
 *
 * @param c             the top design with a wrapper
 * @param appTestHelper the class which is helpful when creating testers
 */
class SumWrapperTester(c: TopModuleWrapper, appTestHelper: AppTestHelper)
  extends ApplicationWrapperTester(c, appTestHelper) {

  poke(c.io.en, 0)
  inputConfig()
  inputData()
  val testII = appTestHelper.getTestII()
  poke(c.io.enConfig, 1)
  poke(c.io.II, testII)
  //wait cgra read configuration and schedule
  step(testII)
  poke(c.io.en, 1)

  val outputCycle = appTestHelper.getOutputCycle()
  step(outputCycle + testII + 1)

  checkPortOuts(testII)
  checkLSUData()

}
