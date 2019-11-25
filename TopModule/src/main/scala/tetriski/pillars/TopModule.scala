package tetriski.pillars

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.{Bundle, Input, Module, Output, UInt}
import chisel3.util._

object Alu_Op {
  val ALU_ADD = 0.U(4.W)
  val ALU_SUB = 1.U(4.W)
  val ALU_AND = 2.U(4.W)
  val ALU_OR = 3.U(4.W)
  val ALU_XOR = 4.U(4.W)
  val ALU_SLT = 5.U(4.W)
  val ALU_SLL = 6.U(4.W)
  val ALU_SLTU = 7.U(4.W)
  val ALU_SRL = 8.U(4.W)
  val ALU_SRA = 9.U(4.W)
  val ALU_COPY_A = 10.U(4.W)
  val ALU_COPY_B = 11.U(4.W)
  val ALU_XXX = 15.U(4.W)
}

import Alu_Op._

class Alu(w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces: 0:out, 1:input_b, 2: input_a, 3: select
    val select = Input(UInt(4.W))
    val input_a = Input(UInt(w.W))
    val input_b = Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })
  val shamt = io.input_b(4, 0).asUInt

  io.out := MuxLookup(io.select, io.input_b, Seq(
    ALU_ADD -> (io.input_a + io.input_b),
    ALU_SUB -> (io.input_a - io.input_b),
    ALU_AND -> (io.input_a & io.input_b),
    ALU_OR -> (io.input_a | io.input_b),
    ALU_XOR -> (io.input_a ^ io.input_b),
    ALU_SLT -> (io.input_a.asSInt < io.input_b.asSInt),
    ALU_SLL -> (io.input_a << shamt),
    ALU_SLTU -> (io.input_a < io.input_b),
    ALU_SRL -> (io.input_a >> shamt),
    ALU_SRA -> (io.input_a.asSInt >> shamt).asUInt,
    ALU_COPY_A -> io.input_a,
    ALU_COPY_B -> io.input_b))
}


class Adder(w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces: 0:out, 1:input_b, 2: input_a
    val input_a = Input(UInt(w.W))
    val input_b = Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })

  io.out := io.input_a + io.input_b
}

class Multiplier(w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces: 0:out, 1:input_b, 2: input_a
    val input_a = Input(UInt(w.W))
    val input_b = Input(UInt(w.W))
    val out = Output(UInt((2 * w).W))
  })

  io.out := io.input_a * io.input_b
}

//to be update
class Dispatch(w: Int) extends Module {
  val io = IO(new Bundle {
    val configuration = Input(UInt(w.W))
    val out = Output(Vec(1, UInt(w.W)))
  })
  io.out(0) := io.configuration

}

class TopModule(val moduleInfo: List[List[Int]], val connect: Map[List[Int], List[List[Int]]], w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces: 0:out, 1:input_1, 2: input_0, 3: configuration
    val configuration = Input(UInt(4.W))
    val input_0 = Input(UInt(w.W))
    val input_1 = Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })
  val moduleNums = moduleInfo(0)
  val types = moduleNums.size
  //  println(io.getElements(0))
  var currentNum = 0
  val addNum = moduleNums(0)
  val adders = (0 until addNum).toArray.map(t => Module(new Adder(moduleInfo(1)(t + currentNum))))
  currentNum += addNum

  val mulNum = moduleNums(1)
  val muls = (0 until mulNum).toArray.map(t => Module(new Multiplier(moduleInfo(1)(t + currentNum))))
  currentNum += mulNum

  val aluNum = moduleNums(2)
  val alus = (0 until aluNum).toArray.map(t => Module(new Alu(moduleInfo(1)(t + currentNum))))
  currentNum += aluNum

  //sent configuration to modules
  //to be update
  val dispatch = Module(new Dispatch(4))
  dispatch.io.configuration := io.configuration
  alus(0).io.select := dispatch.io.out(0)

  val modules = List(adders, muls, alus)

  //  val adder_one = Module(new Adder())
  //  val adder_two = Module(new Adder())

  for (i <- 0 until connect.keys.size) {
    println(connect.keys.toList(i))
  }

  for (i <- 0 until connect.keys.size) {
    val src = connect.keys.toList(i)
    val dsts = connect(src)
    for (j <- 0 until dsts.size) {
      val dst = dsts(j)
      println(dst, src)
      if (dst(0) == types) {
        io.getElements(dst(2)) := modules(src(0))(src(1)).io.getElements(src(2))
      } else if (src(0) == types) {
        modules(dst(0))(dst(1)).io.getElements(dst(2)) := io.getElements(src(2))
      } else {
        modules(dst(0))(dst(1)).io.getElements(dst(2)) := modules(src(0))(src(1)).io.getElements(src(2))
      }
    }
  }
}

class TopModuleUnitTest(c: TopModule) extends PeekPokeTester(c) {
  poke(c.io.input_0, 2)
  poke(c.io.input_1, 3)
  //add config
  poke(c.io.configuration, 0)
  expect(c.io.out, 15)
  //sub config
  poke(c.io.configuration, 1)
  expect(c.io.out, 11)
  //and config
  poke(c.io.configuration, 2)
  expect(c.io.out, 0)
  //or config
  poke(c.io.configuration, 3)
  expect(c.io.out, 15)
  //xor config
  poke(c.io.configuration, 4)
  expect(c.io.out, 15)
}