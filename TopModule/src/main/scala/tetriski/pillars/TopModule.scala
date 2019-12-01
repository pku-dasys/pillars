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


//to be replaced with SRAM
class RegisterFiles(log2Regs : Int, numIn : Int, numOut:Int, w :Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces: 0:outs, 1:inputs, 2: configuration, 3: configTest for test
    val configTest = Output(Vec(numOut+numIn, UInt(w.W)))
    val configuration = Input(UInt((log2Regs * (numIn + numOut)).W))
    val inputs = Input(Vec(numIn, UInt(w.W)))
    val outs = Output(Vec(numOut, UInt(w.W)))
  })
  val targets = (0 until numIn + numOut).toList.map(t => log2Regs)
  val dispatch = Module(new Dispatch((log2Regs * (numIn + numOut)), log2Regs, targets))
  dispatch.io.configuration := io.configuration
  val registers = SyncReadMem(Math.pow(2, log2Regs).toInt, UInt(w.W))
  for (i <- 0 until numIn){
    registers.write(dispatch.io.outs(i), io.inputs(i))
    io.configTest(i) := dispatch.io.outs(i)
  }
  for (i <- 0 until numOut){
    io.outs(i) := registers.read(dispatch.io.outs(i + numIn))
    io.configTest(i + numIn) := dispatch.io.outs(i + numIn)
  }


}

//to be update
class ADRESPE(w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces: 0:out, 1:input_3, 2:input_2, 3:input_1, 4:input_0, 5: configuration
    val configuration = Input(UInt(13.W))
    val input_0 = Input(UInt(w.W))
    val input_1 = Input(UInt(w.W))
    val input_2 = Input(UInt(w.W))
    val input_3 = Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })
  val rf = Module(new RegisterFiles(1, 1, 2, 32))
  val alu = Module(new Alu(32))
  val targets = List(3, 3, 4, 3)
  val dispatch = Module(new Dispatch(13, 4, targets))
  dispatch.io.configuration := io.configuration
  val muxIn0 = MuxLookup(dispatch.io.outs(0), rf.io.outs(0), Array(0.U -> io.input_0, 1.U -> io.input_1,
    2.U -> io.input_2, 3.U -> io.input_3, 4.U -> rf.io.outs(0)))
  val muxIn1 = MuxLookup(dispatch.io.outs(1), rf.io.outs(0), Array(0.U -> io.input_0, 1.U -> io.input_1,
    2.U -> io.input_2, 3.U -> io.input_3, 4.U -> rf.io.outs(0)))
  alu.io.input_a := muxIn0
  alu.io.input_b := muxIn1
  alu.io.select := dispatch.io.outs(2)
  rf.io.inputs(0) := alu.io.out
  rf.io.configuration := dispatch.io.outs(3)
  io.out := rf.io.outs(1)
}

//to be update
//wOut : Upper bound bits of out configuration
class Dispatch(wIn: Int, wOut : Int, targets : List[Int]) extends Module {
  val io = IO(new Bundle {
    val configuration = Input(UInt(wIn.W))
    val outs = Output(Vec(targets.size, UInt(wOut.W)))
  })
  var i = 0
  var offset : Int= 0
  for (elem <- targets){
    io.outs(i) := io.configuration(offset + elem - 1, offset)
    i += 1
    offset += elem
  }

}

class TopModule(val moduleInfo: List[List[Int]], val connect: Map[List[Int], List[List[Int]]], w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces: 0:out, 1:input_1, 2: input_0, 3: configuration
    val configTest = Output(Vec(2, UInt(w.W)))
    val configuration = Input(UInt(17.W))
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

  val PENum = moduleNums(3)
  val PEs = (0 until PENum).toArray.map(t => Module(new ADRESPE(moduleInfo(1)(t + currentNum))))
  currentNum += PENum

  val modules = List(adders, muls, alus, PEs)

//  val test = Module(new ADRESPE(32))
//  test.io.input_0 := io.input_0
//  test.io.input_1 := io.input_0
//  test.io.input_2 := io.input_1
//  test.io.input_3 := io.input_0

  //sent configuration to modules
  //to be update
  val dispatch = Module(new Dispatch(17, 13, List(4, 13)))
  dispatch.io.configuration := io.configuration
  alus(0).io.select := dispatch.io.outs(0)
  io.configTest(0) := dispatch.io.outs(0)
  if(PENum > 0){
    PEs(0).io.configuration := dispatch.io.outs(1)
  }
  io.configTest(1) := dispatch.io.outs(1)



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
        println(dst,src)
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

class TopModulePEUnitTest(c: TopModule) extends PeekPokeTester(c) {
  poke(c.io.input_0, 2)
  poke(c.io.input_1, 3)
  //1 0 0 0001 001 000 0000
  //save (((a+b)*a+b)+(a)) - (a+b) in rf(0), to next cycle //10
  poke(c.io.configuration, 66688)
  expect(c.io.out, 0)
  expect(c.io.configTest(0), 0)
  expect(c.io.configTest(1), 4168)
  step(1)
  expect(c.io.out, 0)
  //1 1 1 0000 011 101 0000
  // rf(1) = rf(0) + b // 10 + 3 =13
  poke(c.io.configuration, 115152)
  step(1)
  expect(c.io.out, 13)
}

class RegisterFilesUnitTest(c: RegisterFiles) extends PeekPokeTester(c) {
  poke(c.io.inputs(0), 666)
  poke(c.io.configuration, 7)
  expect(c.io.outs(0), 0)
  expect(c.io.outs(1), 0)
  expect(c.io.configTest(0), 1)
  expect(c.io.configTest(1), 1)
  expect(c.io.configTest(2), 1)

  step(1)

  expect(c.io.outs(0), 666)
  expect(c.io.outs(1), 666)
  poke(c.io.configuration, 4)
  poke(c.io.inputs(0), 233)


  step(1)
  expect(c.io.configTest(0), 0)
  expect(c.io.configTest(1), 0)
  expect(c.io.configTest(2), 1)

  expect(c.io.outs(1), 666)
  expect(c.io.outs(0), 233)
}