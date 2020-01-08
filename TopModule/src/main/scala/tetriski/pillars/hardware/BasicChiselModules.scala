package tetriski.pillars.hardware

import chisel3.util.{EnqIO, Enum, MixedVec, MuxLookup, log2Ceil, log2Up}
import chisel3.{Bundle, Input, Mem, Module, Output, UInt, Vec, _}
import tetriski.pillars.testers.EnqMemWrapper
import tetriski.pillars.util.{EnqMem, MemReadIO, MemWriteIO, SimpleDualPortSram}
import tetriski.pillars.hardware.PillarsConfig._

class ScheduleController extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val waitCycle = Input(UInt(LOG_SCHEDULE_SIZE))
    val valid = Output(Bool())
  })

  val s_wait :: s_valid :: Nil = Enum(2)
  val state = RegInit(s_wait)
  val cycleReg = Reg(UInt(LOG_SCHEDULE_SIZE))

  io.valid := (cycleReg === io.waitCycle) && io.en

  when(io.en){
    when(state === s_wait){
      when(cycleReg === io.waitCycle){
        state := s_valid
      }.otherwise{
        cycleReg := cycleReg + 1.U
      }
    }
  }.otherwise{
    state := s_wait
    cycleReg := 0.U
  }
}

class Alu(w: Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
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

  when(io.en){
    out := MuxLookup(io.configuration, input_b, Seq(
      ALU_ADD -> (input_a + input_b),
      ALU_SUB -> (input_a - input_b),
      ALU_AND -> (input_a & input_b),
      ALU_OR -> (input_a | input_b),
      ALU_XOR -> (input_a ^ input_b),
      ALU_MUL -> (input_a * input_b),
      ALU_SLT -> (input_a.asSInt < input_b.asSInt),
      ALU_SLL -> (input_a << shamt),
      ALU_SLTU -> (input_a < input_b),
      ALU_SRL -> (input_a >> shamt),
      ALU_SRA -> (input_a.asSInt >> shamt).asUInt,
      ALU_COPY_A -> input_a,
      ALU_COPY_B -> input_b))
  }.otherwise{
    for(out <- io.outs){
      out := 0.U
    }
  }
}


//class Adder(w: Int) extends Module {
//  val io = IO(new Bundle {
//    //port sequnces outs: 0: out
//    //port sequnces inputs: 0: input_a, 1: input_b
//    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
//    val outs = Output(MixedVec(Seq(UInt(w.W))))
//  })
//  val input_a = io.inputs(0)
//  val input_b = io.inputs(1)
//  val out = io.outs(0)
//
//  out := input_a + input_b
//}
//
//class Multiplier(w: Int) extends Module {
//  val io = IO(new Bundle {
//    //port sequnces outs: 0: out
//    //port sequnces inputs: 0: input_a, 1: input_b
//    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
//    val outs = Output(MixedVec(Seq(UInt((2 * w).W))))
//  })
//
//  val input_a = io.inputs(0)
//  val input_b = io.inputs(1)
//  val out = io.outs(0)
//
//  out := input_a * input_b
//}


class RegisterFiles(log2Regs : Int, numIn : Int, numOut:Int, w :Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    //port sequnces: 0:outs, 1:inputs, 2: configuration, 3: configTest for test
//    val configTest = Output(Vec(numOut+numIn, UInt(w.W)))
    val configuration = Input(UInt((log2Regs * (numIn + numOut)).W))
    val inputs = Input(MixedVec((1 to numIn) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to numOut) map { i => UInt(w.W) }))
  })
  val targets = (0 until numIn + numOut).toList.map(t => log2Regs)
  val dispatch = Module(new Dispatch((log2Regs * (numIn + numOut)), targets))
  dispatch.io.configuration := io.configuration
  dispatch.io.en <> io.en

  //val registers = SyncReadMem(Math.pow(2, log2Regs).toInt, UInt(w.W))
  //val registers = Mem(Math.pow(2, log2Regs).toInt, UInt(w.W))

  val regs = RegInit(VecInit(Seq.fill(Math.pow(2, log2Regs).toInt)(0.U(32.W))))

  when(io.en){
    for (i <- 0 until numIn){
      //registers.write(dispatch.io.outs(i), io.inputs(i))
      regs(dispatch.io.outs(i)) := io.inputs(i)
//      io.configTest(i) := dispatch.io.outs(i)
    }
    for (i <- 0 until numOut){
      //io.outs(i) := registers.read(dispatch.io.outs(i + numIn))
      io.outs(i) := regs(dispatch.io.outs(i + numIn))
//      io.configTest(i + numIn) := dispatch.io.outs(i + numIn)
    }
  }.otherwise{
    for(out <- io.outs){
      out := 0.U
    }
  }

}

class Multiplexer(inNum : Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())

    val configuration = Input(UInt(log2Up(inNum).W))
    val inputs = Input(MixedVec((1 to inNum) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to 1) map { i => UInt(w.W) }))
  })
  val input0 = io.inputs(0)
  val input1 = io.inputs(1)
  val out = io.outs(0)
  val selectArray = (0 to inNum - 1).map(i => i.U -> io.inputs(i))
  val muxIn0 = MuxLookup(io.configuration, io.inputs(0), selectArray)
  when(io.en){
    io.outs(0) := muxIn0
  }.otherwise{
    for(out <- io.outs){
      out := 0.U
    }
  }
}

//  object Common {
//    def constUnitBody(configuration: UInt, outs: MixedVec[UInt]): Unit = {
//      val const = Mem(1, UInt(w.W))
//      const.write(0.U, configuration)
//      outs(0) := const.read((0.U))
//    }
//  }

//class ConstUnit(w :Int, foo: (UInt, MixedVec[UInt]) => Unit) extends Module {
//  val io = IO(new Bundle {
//    val configuration = Input(UInt(w.W))
//    val outs = Output(MixedVec((1 to 1) map { i => UInt(w.W) }))
//  })
//  foo(io.configuration, io.outs)
//}

class ConstUnit(w :Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())

    val configuration = Input(UInt(w.W))
    val outs = Output(MixedVec((1 to 1) map { i => UInt(w.W) }))
  })
  val const = Mem(1, UInt(w.W))
  const.write(0.U, io.configuration)

  when(io.en){
    io.outs(0) := const.read((0.U))
  }.otherwise{
    for(out <- io.outs){
      out := 0.U
    }
  }
}

//unused currently
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
class Dispatch(wIn: Int, targets : List[Int]) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())

    val configuration = Input(UInt(wIn.W))
    val outs = Output(MixedVec(targets.map{i => UInt(i.W)}))
  })
  val outt =io.outs(targets.size - 2)
  var i = 0
  var offset : Int= 0
  when(io.en){
    for (elem <- targets){
      io.outs(i) := io.configuration(offset + elem - 1, offset)
      i += 1
      offset += elem
    }
  }.otherwise{
    for(out <- io.outs){
      out := 0.U
    }
  }
}

class DispatchT(wIn: Int, targets : List[Int]) extends Module {
  val io = IO(new Bundle {
    val configuration = Input(UInt(wIn.W))
    val outs = Output(MixedVec(targets.map{i => UInt(i.W)}))
  })
  val outt =io.outs(targets.size - 2)
  var i = 0
  var offset : Int= 0
  for (elem <- targets){
    io.outs(i) := io.configuration(offset + elem - 1, offset)
    i += 1
    offset += elem
  }

}

class LSMemWrapper(w : Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))

    val readMem = Flipped(new MemReadIO(MEM_DEPTH, w))
    val writeMem = Flipped(new MemWriteIO(MEM_DEPTH, w))

    val base = Input(UInt(readMem.addr.getWidth.W))
    val start = Input(Bool())
    val en = Input(Bool())
    val idle = Output(Bool())
  })

  val mem = Module(new SimpleDualPortSram(MEM_DEPTH, w))
  val enq_mem = Module(new EnqMem(mem.io.a, MEM_IN_WIDTH))

  mem.clock := clock
  enq_mem.clock := clock

  io.readMem <> mem.io.b
  when(io.en === true.B){
    enq_mem.io.mem <> mem.io.a
  }.otherwise{
    io.writeMem <> mem.io.a
  }

  enq_mem.io.en <> io.en
  enq_mem.io.in <> io.in

  enq_mem.io.base <> io.base
  enq_mem.io.start <> io.start
  enq_mem.io.idle <> io.idle
}

class LoadStoreUnit(w : Int) extends Module{
  val io = IO(new Bundle {
    //0 for load, 1 for store
    val configuration = Input(UInt(1.W))
    val en = Input(Bool())

    val in = Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))
    val base = Input(UInt(log2Ceil(MEM_DEPTH).W))
    val start = Input(Bool())
    val enqEn = Input(Bool())
    val idle = Output(Bool())

    val inputs = Input(MixedVec( UInt(log2Ceil(MEM_DEPTH).W), UInt(w.W)))
    val outs = Output(MixedVec((1 to 1) map { i => UInt(w.W) }))
  })
  val memWrapper = Module(new LSMemWrapper(w))
  memWrapper.io.base <> io.base
  memWrapper.io.start <> io.start
  memWrapper.io.idle <> io.idle
  memWrapper.io.en <> io.enqEn
  memWrapper.io.in <> io.in

  val addr = io.inputs(0)
  val dataIn = io.inputs(1)
  val out = io.outs(0)

  val readMem =  memWrapper.io.readMem
  val writeMem =  memWrapper.io.writeMem



  when(io.en){
    readMem.addr := addr
    writeMem.addr := addr
    writeMem.din := dataIn
    when(io.configuration === 0.U){
      readMem.en := true.B
      writeMem.en := false.B
      writeMem.we := false.B
    }.otherwise{
      readMem.en := false.B
      writeMem.en := true.B
      writeMem.we := true.B
    }
    io.outs(0) := readMem.dout
  }.otherwise{
    readMem.en := false.B
    writeMem.en := false.B
    writeMem.we := false.B
    readMem.addr := DontCare
    writeMem.addr := DontCare
    writeMem.din := DontCare
      for(out <- io.outs){
        out := 0.U
      }
    }
}


