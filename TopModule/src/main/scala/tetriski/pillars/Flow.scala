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

    val moduleInfo = List(moduleNum, List(16, 32, 16))
//    val topmodule = Module(new TopMo())
    chisel3.Driver.execute(args, () => new TopModule(moduleInfo, maplist, 32))  //verilog generation

    iotesters.Driver.execute(args, () => new TopModule(moduleInfo, maplist, 32)) {
      c => new TopModuleUnitTest(c)
    }
  }
}

