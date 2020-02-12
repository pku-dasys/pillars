package tetriski.pillars.testers

import chisel3.iotesters.PeekPokeTester
import tetriski.pillars.hardware.TopModule

class AppTestHelper(bitStreams: Array[BigInt], waitCycles: List[Int],
                    testII: Int, outputCycle: Int, outPortRefs: Map[Int, Array[Int]]) {
  var inDataMap = Map[List[Int], Array[Int]]()
  var outDataMap = Map[List[Int], Array[Int]]()

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
  def getBitStreams(): Array[BigInt] ={
    bitStreams
  }
  def getWaitCycles(): List[Int] ={
    waitCycles
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

class ApplicationTester(c: TopModule) extends PeekPokeTester(c) {
  def asUnsignedInt(signedInt: Int): BigInt = (BigInt(signedInt >>> 1) << 1) + (signedInt & 1)
  def enqData(numInLSU: Int, inData: Array[Int], base: Int): Unit ={
    poke(c.io.startLSU(numInLSU), 1)
    poke(c.io.enqEnLSU(numInLSU), 1)
    poke(c.io.inLSU(numInLSU).valid, 0)
    poke(c.io.baseLSU(numInLSU), base)
    step(1)

    // push
    for (x <- inData) {
      poke(c.io.inLSU(numInLSU).valid, 1)
      expect(c.io.inLSU(numInLSU).valid, 1)
      poke(c.io.inLSU(numInLSU).bits, x)
      if (peek(c.io.inLSU(numInLSU).ready) == 0) {
        while (peek(c.io.inLSU(numInLSU).ready) == 0) {
          step(1)
        }
      } else {
        step(1)
      } // exit condition: (c.io.in.ready === true.B) and step()
    }
    poke(c.io.inLSU(numInLSU).valid, 0)

    // exec
    while (peek(c.io.idleLSU(numInLSU)) == 0) {
      step(1)
    }

    poke(c.io.enqEnLSU(numInLSU), 0)
  }
}

class SumTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c) {

  poke(c.io.en, 0)

  //input data into LSU
  for(inDataItem <- appTestHelper.inDataMap){
    val numInLSU = inDataItem._1(0)
    val base = inDataItem._1(1)
    val inData = inDataItem._2
    enqData(numInLSU, inData, base)
  }


  val testII = appTestHelper.getTestII()
  val waitCycles = appTestHelper.getWaitCycles()
  val bitStreams = appTestHelper.getBitStreams()

  poke(c.io.en, 1)
  poke(c.io.II, testII)

  for(i <- 0 until waitCycles.size){
    poke(c.io.aluSchedule(i), waitCycles(i))
  }

  for(i <- 0 until testII){
    poke(c.io.configuration, bitStreams(i))
    step(1)
  }

  val outputCycle = appTestHelper.getOutputCycle()
  step(outputCycle - testII)

  val refs = appTestHelper.getOutPortRefs()
  for(ref <- refs){
    for(i <- ref._2){
      expect(c.io.outs(ref._1), asUnsignedInt(i))
//      println(asUnsignedInt(i).toString + " " + peek(c.io.outs(ref._1)).toString())
      step(testII)
    }
  }

}