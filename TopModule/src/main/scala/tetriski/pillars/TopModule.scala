package tetriski.pillars

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.{Bundle, Input, Module, Output, UInt}


class Adder(w : Int) extends Module {
  val io = IO(new Bundle{
    //port sequnces: 0:out, 1:input_b, 2: input_a
    val input_a = Input(UInt(w.W))
    val input_b= Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })

  io.out := io.input_a + io.input_b
}

class Multiplier(w : Int) extends Module {
  val io = IO(new Bundle{
    //port sequnces: 0:out, 1:input_b, 2: input_a
    val input_a = Input(UInt(w.W))
    val input_b= Input(UInt(w.W))
    val out = Output(UInt((2 * w).W))
  })

  io.out := io.input_a * io.input_b
}

class TopModule (val moduleInfo: List[List[Int]], val connect: Map[List[Int] , List[List[Int]]], w : Int)  extends Module {
  val io = IO(new Bundle{
    //port sequnces: 0:out, 1:input_1, 2: input_0
    val input_0 = Input(UInt(w.W))
    val input_1= Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })
  val moduleNums = moduleInfo(0)
  val types = moduleNums.size
  //  println(io.getElements(0))
  var currentNum = 0
  val addNum = moduleNums(0)
  val adders = (0 until addNum).toArray.map( t => Module(new Adder(moduleInfo(1)(t + currentNum))))
  currentNum += addNum

  val mulNum = moduleNums(1)
  val muls = (0 until mulNum).toArray.map( t => Module(new Multiplier(moduleInfo(1)(t + currentNum))))

  val modules = List(adders, muls)

  //  val adder_one = Module(new Adder())
  //  val adder_two = Module(new Adder())

  for (i <- 0 until connect.keys.size) {
    println(connect.keys.toList(i))
  }

  for (i <- 0 until connect.keys.size) {
    val src = connect.keys.toList(i)
    val dsts = connect(src)
    for (j <-0 until dsts.size){
      val dst = dsts(j)
      println(dst, src)
      if (dst(0) == types){
        io.getElements(dst(2)) := modules(src(0))(src(1)).io.getElements(src(2))
      }else if(src(0) == types){
        modules(dst(0))(dst(1)).io.getElements(dst(2)) := io.getElements(src(2))
      }else{
        modules(dst(0))(dst(1)).io.getElements(dst(2)) := modules(src(0))(src(1)).io.getElements(src(2))
      }
    }
  }

  //  adders(1).io.getElements(2) := adders(0).io.out
  ////  adders(1).io.input_a := adders(0).io.out
  //  adders(1).io.input_b := 4.U
  //
  //  adders(0).io.input_a := io.input_0
  //  adders(0).io.input_b := io.input_1

  //io.out := adders(1).io.out

}

class TopModuleUnitTest(c: TopModule) extends PeekPokeTester(c) {
  poke(c.io.input_0,2)
  poke(c.io.input_1,3)
  expect(c.io.out, 13)
}