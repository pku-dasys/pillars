package tetriski.pillars

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.{Bundle, Input, Module, Output, UInt}
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

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
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_a, 1: input_b
    val configuration = Input(UInt(4.W))
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  val input_a = io.inputs(0)
  val input_b = io.inputs(1)
  val out = io.outs(0)
  val shamt = input_b(4, 0).asUInt

  out := MuxLookup(io.configuration, input_b, Seq(
    ALU_ADD -> (input_a + input_b),
    ALU_SUB -> (input_a - input_b),
    ALU_AND -> (input_a & input_b),
    ALU_OR -> (input_a | input_b),
    ALU_XOR -> (input_a ^ input_b),
    ALU_SLT -> (input_a.asSInt < input_b.asSInt),
    ALU_SLL -> (input_a << shamt),
    ALU_SLTU -> (input_a < input_b),
    ALU_SRL -> (input_a >> shamt),
    ALU_SRA -> (input_a.asSInt >> shamt).asUInt,
    ALU_COPY_A -> input_a,
    ALU_COPY_B -> input_b))
}


class Adder(w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_a, 1: input_b
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })
  val input_a = io.inputs(0)
  val input_b = io.inputs(1)
  val out = io.outs(0)

  out := input_a + input_b
}

class Multiplier(w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_a, 1: input_b
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt((2 * w).W))))
  })

  val input_a = io.inputs(0)
  val input_b = io.inputs(1)
  val out = io.outs(0)

  out := input_a * input_b
}


//to be replaced with SRAM
class RegisterFiles(log2Regs : Int, numIn : Int, numOut:Int, w :Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces: 0:outs, 1:inputs, 2: configuration, 3: configTest for test
    val configTest = Output(Vec(numOut+numIn, UInt(w.W)))
    val configuration = Input(UInt((log2Regs * (numIn + numOut)).W))
    val inputs = Input(MixedVec((1 to numIn) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to numOut) map { i => UInt(w.W) }))
  })
  val targets = (0 until numIn + numOut).toList.map(t => log2Regs)
  val dispatch = Module(new Dispatch((log2Regs * (numIn + numOut)), targets))
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

class Multiplexer(inNum : Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val configuration = Input(UInt(log2Up(inNum).W))
    val inputs = Input(MixedVec((1 to inNum) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to 1) map { i => UInt(w.W) }))
  })
  val selectArray = (0 to inNum - 1).map(i => i.U -> io.inputs(i))
  val muxIn0 = MuxLookup(io.configuration, io.inputs(0), selectArray)
  io.outs(0) := muxIn0
}

//to be update
class ADRESPE(w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_0, 1: input_1, 2: input_2, 3: input_3
    val configuration = Input(UInt(13.W))
    val inputs = Input(MixedVec((1 to 4) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to 1) map { i => UInt(w.W) }))
  })

  val input_0 = io.inputs(0)
  val input_1 = io.inputs(1)
  val input_2 = io.inputs(2)
  val input_3 = io.inputs(3)
  val out = io.outs(0)

  val rf = Module(new RegisterFiles(1, 1, 2, 32))
  val alu = Module(new Alu(32))
  val targets = List(3, 3, 4, 3)
  val dispatch = Module(new Dispatch(13,  targets))
  dispatch.io.configuration := io.configuration
  val muxIn0 = MuxLookup(dispatch.io.outs(0), rf.io.outs(0), Array(0.U -> input_0, 1.U -> input_1,
    2.U -> input_2, 3.U -> input_3, 4.U -> rf.io.outs(0)))
  val muxIn1 = MuxLookup(dispatch.io.outs(1), rf.io.outs(0), Array(0.U -> input_0, 1.U -> input_1,
    2.U -> input_2, 3.U -> input_3, 4.U -> rf.io.outs(0)))
  alu.io.inputs(0) := muxIn0
  alu.io.inputs(1) := muxIn1
  alu.io.configuration := dispatch.io.outs(2)
  rf.io.inputs(0) := alu.io.outs(0)
  rf.io.configuration := dispatch.io.outs(3)
  out := rf.io.outs(1)
}

//to be update
//wOut : Upper bound bits of out configuration
class Dispatch(wIn: Int, targets : List[Int]) extends Module {
  val io = IO(new Bundle {
    val configuration = Input(UInt(wIn.W))
    val outs = Output(MixedVec(targets.map{i => UInt(i.W)}))
  })
  var i = 0
  var offset : Int= 0
  for (elem <- targets){
    io.outs(i) := io.configuration(offset + elem - 1, offset)
    i += 1
    offset += elem
  }

}

class TopModule(val moduleInfo: List[List[Int]], val connect: Map[List[Int], List[List[Int]]],
                val configList : List[List[List[Int]]], w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_a, 1: input_b
   // val configTest = Output(Vec(2, UInt(w.W)))
    val configuration = Input(UInt(13.W))
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  def getConfigBit (typeID : Int): Int ={
    val ret = typeID match {
      case 0 => 4
      case 1 => 3
      case 2 => 3
    }
    ret
  }
  val input_0 = io.inputs(0)
  val input_1 = io.inputs(1)
  val out = io.outs(0)

  val moduleNums = moduleInfo(0)
  val types = moduleNums.size
  //  println(io.getElements(0))
  var currentNum = 0
//  val addNum = moduleNums(0)
//  val adders = (0 until addNum).toArray.map(t => Module(new Adder(moduleInfo(1)(t + currentNum))))
//  currentNum += addNum
//
//  val mulNum = moduleNums(1)
//  val muls = (0 until mulNum).toArray.map(t => Module(new Multiplier(moduleInfo(1)(t + currentNum))))
//  currentNum += mulNum
//
//  val PENum = moduleNums(3)
//  val PEs = (0 until PENum).toArray.map(t => Module(new ADRESPE(moduleInfo(1)(t + currentNum))))
//  currentNum += PENum
//
  val aluNum = moduleNums(0)
  val alus = (0 until aluNum).toArray.map(t => Module(new Alu(moduleInfo(1)(t + currentNum))))
  currentNum += aluNum



  val RFNum1_1_2 = moduleNums(1)
  val RFs1_1_2 = (0 until RFNum1_1_2).toArray
    .map(t => Module(new RegisterFiles(1, 1, 2, moduleInfo(1)(t + currentNum))))
  currentNum += RFNum1_1_2

  val MuxNum5 = moduleNums(2)
  val Muxs5 = (0 until MuxNum5).toArray
    .map(t => Module(new Multiplexer(5, moduleInfo(1)(t + currentNum))))
  currentNum += MuxNum5

  val modules = List(alus, RFs1_1_2, Muxs5)


  val outPorts = new ArrayBuffer[Array[List[Any]]]
  outPorts.append(alus.map(i => i.io.outs.toList))
  outPorts.append(RFs1_1_2.map(i => i.io.outs.toList))
  outPorts.append(Muxs5.map(i => i.io.outs.toList))

  val inPorts = new ArrayBuffer[Array[List[Any]]]
  inPorts.append(alus.map(i => i.io.inputs.toList))
  inPorts.append(RFs1_1_2.map(i => i.io.inputs.toList))
  inPorts.append(Muxs5.map(i => i.io.inputs.toList))


  var dispatchs = ArrayBuffer[Dispatch]()
  var regionConfigBits = List[Int]()
  for (region <- configList){
    var configBits = List[Int]()
    var configPorts = List[Data]()
    for(moduleList <- region){
      val typeID = moduleList(0)
      val moduleID = moduleList(1)
      configBits = configBits :+ getConfigBit(typeID)
      val configPort = typeID match {
        case 0 => alus(moduleID).io.configuration
        case 1 => RFs1_1_2(moduleID).io.configuration
        case 2 => Muxs5(moduleID).io.configuration
      }
      configPorts = configPorts :+ configPort
    }
    val regionTotalBits = configBits.reduce(_+_)
    regionConfigBits = regionConfigBits :+ regionTotalBits
    val dispatch = Module(new Dispatch(regionTotalBits, configBits))
    for (i <- 0 until configBits.size){
      configPorts(i) := dispatch.io.outs(i)
    }
    //io.configTest(0) := alus
    dispatchs.append(dispatch)
  }
  val totalBits = regionConfigBits.reduce(_+_)
  val topDispatch = Module(new Dispatch(totalBits, regionConfigBits))
  topDispatch.io.configuration := io.configuration
  for (i <- 0 until dispatchs.size){
    dispatchs(i).io.configuration := topDispatch.io.outs(i)
  }

  //sent configuration to modules
//  val dispatch = Module(new Dispatch(13, List(3, 3, 4, 3)))
//  dispatch.io.configuration := io.configuration
//
//  io.configTest(0) := dispatch.io.outs(0)
//  io.configTest(1) := dispatch.io.outs(1)
//
//  Muxs5(0).io.configuration := dispatch.io.outs(0)
//  Muxs5(1).io.configuration := dispatch.io.outs(1)
//  alus(0).io.configuration := dispatch.io.outs(2)
//  RFs1_1_2(0).io.configuration := dispatch.io.outs(3)


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
        io.outs(dst(2)) := outPorts(src(0))(src(1))(src(2)).asInstanceOf[Data]
      } else if (src(0) == types) {
        inPorts(dst(0))(dst(1))(dst(2)).asInstanceOf[Data] := io.inputs(src(2))
      } else {
        inPorts(dst(0))(dst(1))(dst(2)).asInstanceOf[Data] := outPorts(src(0))(src(1))(src(2)).asInstanceOf[Data]
      }
    }
  }

}

class TopModulePEUnitTest(c: TopModule) extends PeekPokeTester(c) {
  //MixedVec don't support c.io.inputs(0) in poke
  poke(c.input_0, 2)
  poke(c.input_1, 3)
  //010 000 100 0000
  //save (a+b) in rf(0), to next cycle //5
  poke(c.io.configuration, 2112)
  expect(c.out, 0)
//  expect(c.io.configTest(0), 0)
//  expect(c.io.configTest(1), 2)
  step(1)
  expect(c.out, 0)
  //001 100 111 0011
  // rf(1) = rf(0) or a // 5 or 2 =7
  poke(c.io.configuration, 1651)
//  expect(c.io.configTest(0), 4)
//  expect(c.io.configTest(1), 1)
  step(1)
  expect(c.out, 7)
}
