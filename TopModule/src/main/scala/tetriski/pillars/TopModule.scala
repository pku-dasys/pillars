package tetriski.pillars

import chisel3._
import chisel3.{Bundle, Input, Module, Output, UInt}


class Adder extends Module {
  val io = IO(new Bundle{
    //port sequnces: 0:out, 1:input_b, 2: input_a
    val input_a = Input(UInt(32.W))
    val input_b= Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  io.out := io.input_a + io.input_b
}

class Multiplier extends Module {
  val io = IO(new Bundle{
    //port sequnces: 0:out, 1:input_b, 2: input_a
    val input_a = Input(UInt(32.W))
    val input_b= Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  io.out := io.input_a * io.input_b
}

class TopModule (val moduleNums: List[Int], val types : Int, val connect: Map[List[Int] , List[List[Int]]] )  extends Module {
  val io = IO(new Bundle{
    //port sequnces: 0:out, 1:input_1, 2: input_0
    val input_0 = Input(UInt(32.W))
    val input_1= Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  //  println(io.getElements(0))
  val addNum = moduleNums(0)
  val adders = (0 until addNum).toArray.map( t => Module(new Adder))
  val mulNum = moduleNums(1)
  val muls = (0 until mulNum).toArray.map( t => Module(new Multiplier))

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
