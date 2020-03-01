package tetriski.pillars.testers

import chisel3.{Bool, Bundle, Flipped, Input, Output, UInt, Vec}
import chisel3.iotesters.PeekPokeTester
import chisel3.util.{DeqIO, EnqIO, MixedVec, log2Ceil}
import tetriski.pillars.hardware.{TopModule, TopModuleWrapper}
import tetriski.pillars.hardware.PillarsConfig._

class AppTestHelper(bitStreams: Array[BigInt], schedules: List[Int],
                    testII: Int) {
  var inDataMap = Map[List[Int], Array[Int]]()
  var outDataMap = Map[List[Int], Array[Int]]()
  var outPortRefs = Map[Int, Array[Int]]()
  var outputCycle = testII + 1
  var throughput = 1

  def setThroughput(arg: Int): Unit ={
    throughput = arg
  }
  def getThroughput(): Int ={
    throughput
  }
  def setOutputCycle(arg : Int): Unit ={
    outputCycle = arg
  }
  def setOutPortRefs(arg: Map[Int, Array[Int]]): Unit ={
    outPortRefs = arg
  }
  def getInData(numLSU: Int, base: Int): Array[Int] ={
    inDataMap(List(numLSU, base))
  }
  def addInData(numLSU: Int, base: Int, inData: Array[Int]): Unit ={
    inDataMap = inDataMap + (List(numLSU, base) -> inData)
  }
  def addInData(inDatas: Map[List[Int], Array[Int]]): Unit ={
    inDataMap = inDataMap ++ inDatas
  }
  def getOutData(numLSU: Int, base: Int): Array[Int] ={
    outDataMap(List(numLSU, base))
  }
  def addOutData(numLSU: Int, base: Int, outData: Array[Int]): Unit ={
    outDataMap = outDataMap + (List(numLSU, base) -> outData)
  }
  def addOutData(outDatas: Map[List[Int], Array[Int]]): Unit ={
    outDataMap = outDataMap ++ outDatas
  }
  def getBitStreams(): Array[BigInt] ={
    bitStreams
  }
  def getSchedules(): List[Int] ={
    schedules
  }
  def getSchedulesBigInt(): BigInt ={
    var ret: BigInt = 0
    for(sche <- schedules.reverse){
      ret = (ret << LOG_SCHEDULE_SIZE * 2 + 1) + sche
    }
    ret
  }
  def getTestII(): Int ={
    testII
  }
  def getOutputCycle(): Int ={
    outputCycle
  }
  def getOutPortRefs(): Map[Int, Array[Int]] ={
    outPortRefs
  }
}

class ApplicationTester(c: TopModule, appTestHelper: AppTestHelper) extends PeekPokeTester(c) {
  def asUnsignedInt(signedInt: Int): BigInt = (BigInt(signedInt >>> 1) << 1) + (signedInt & 1)
  def enqData(numInLSU: Int, inData: Array[Int], base: Int): Unit ={
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
  def deqData(numInLSU: Int, refArray: Array[Int], base: Int): Unit ={
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
      expect(c.io.streamOutLSU(numInLSU).bits,  asUnsignedInt(refArray(i)))
      println(asUnsignedInt(refArray(i)).toString + " " + peek(c.io.streamOutLSU(numInLSU).bits).toString())
      step(1)
    }

    poke(c.io.deqEnLSU(numInLSU), 0)
  }
  def inputData(): Unit ={
    //input data into LSU
    for(inDataItem <- appTestHelper.inDataMap){
      val numInLSU = inDataItem._1(0)
      val base = inDataItem._1(1)
      val inData = inDataItem._2
      enqData(numInLSU, inData, base)
    }
  }
  def inputConfig(testII: Int): Unit ={
    val schedules = appTestHelper.getSchedulesBigInt()
    val bitStreams = appTestHelper.getBitStreams()

    poke(c.io.enConfig, 1)
    poke(c.io.II, testII)
    poke(c.io.schedules, schedules)

    for(i <- 0 until testII){
      poke(c.io.configuration, bitStreams(i))
      step(1)
    }
  }
  def checkPortOuts(testII: Int): Unit ={
    val refs = appTestHelper.getOutPortRefs()
    val throughput = appTestHelper.getThroughput()
    if(throughput > 1){
      step((throughput -1) * testII)
    }
    for(ref <- refs){
      for(i <- ref._2){
        expect(c.io.outs(ref._1), asUnsignedInt(i))
        println(asUnsignedInt(i).toString + " " + peek(c.io.outs(ref._1)).toString())
        step(testII * throughput)
      }
    }
  }
  def checkLSUData(): Unit ={
    //stream deq test
    for(inDataItem <- appTestHelper.outDataMap){
      val numInLSU = inDataItem._1(0)
      val base = inDataItem._1(1)
      val refArray = inDataItem._2
      deqData(numInLSU, refArray, base)
    }
  }
}

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
  checkLSUData()
}

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
