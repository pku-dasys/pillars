package tetriski.pillars.testers

import chisel3.iotesters.PeekPokeTester
import tetriski.pillars.hardware.TopModule

class ApplicationTester(c: TopModule) extends PeekPokeTester(c) {
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

class SumTester(c: TopModule, bitstreams: Array[BigInt], waitCycles: List[Int]) extends ApplicationTester(c) {

  val inData = (10 to 100).toArray
  poke(c.io.en, 0)

  val base = 0

  val numInLSU = 3


  enqData(numInLSU, inData, base)

  poke(c.io.en, 1)
  poke(c.io.II, 3)

  for(i <- 0 until waitCycles.size){
    poke(c.io.aluSchedule(i), waitCycles(i))
  }

  poke(c.io.configuration, bitstreams(0))

  step(1)
  poke(c.io.configuration, bitstreams(1))

  step(1)
  poke(c.io.configuration, bitstreams(2))

  step(7)
  var ref = 0
  for( i <- 10 until 100){
    //    println("cycle "+ i.toString)
    //poke(c.input_1, i)
    //if(i % 5 == 0)\
    ref = ref + i
    expect(c.io.outs(3), ref)
    //    println(ref.toString + " " + peek(c.io.outs(3)).toString())
    step(3)
  }
}