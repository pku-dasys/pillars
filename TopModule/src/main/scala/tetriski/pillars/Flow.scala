package tetriski.pillars

import chisel3._

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

object Flow {
  def main(args: Array[String]): Unit = {
    //Function: (a+b)*a+b

    val moduleNum = List(2,1)

    val maplist = Map(List(0, 0 ,0) -> List(List(1, 0, 1)),
      List(2, 0 ,2) -> List(List(1, 0, 2), List(0, 0, 2)),
      List(2, 0 ,1) -> List(List(0, 0, 1), List(0, 1, 2)),
      List(1, 0, 0) -> List(List(0, 1, 1)),
        List(0, 1 ,0) -> List(List(2, 0, 0))
    )
//    val topmodule = Module(new TopMo())
    chisel3.Driver.execute(args, () => new TopModule(moduleNum,2, maplist))  //verilog generation

    iotesters.Driver.execute(args, () => new TopModule(moduleNum,2, maplist)) {
      c => new TopMoUnitTest(c)
    }
  }
}

class TopMoUnitTest(c: TopModule) extends PeekPokeTester(c) {
  poke(c.io.input_0,2)
  poke(c.io.input_1,3)
  expect(c.io.out, 13)
}


//object TopMo extends App{
//  chisel3.Driver.execute(args, () => new TopMo())
//}