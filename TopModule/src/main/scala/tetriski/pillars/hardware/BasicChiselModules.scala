package tetriski.pillars.hardware

import chisel3.util.{DeqIO, EnqIO, Enum, MixedVec, MuxLookup, is, log2Ceil, log2Up, switch}
import chisel3.{Bundle, Input, Mem, Module, Output, UInt, Vec, _}
import tetriski.pillars.testers.EnqMemWrapper
import tetriski.pillars.util.{DeqMem, EnqMem, MemReadIO, MemWriteIO, SimpleDualPortSram}
import tetriski.pillars.hardware.PillarsConfig._

import scala.collection.mutable.ArrayBuffer

class ConfigController(configWidth : Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val II = Input(UInt(LOG_II_UPPER_BOUND.W))
    val inConfig = Input(UInt(configWidth.W))
    val outConfig = Output(UInt(configWidth.W))
  })

  val s_read_write :: s_read_only :: Nil = Enum(2)
  val state = RegInit(s_read_write)
  val cycleReg = Reg(UInt(LOG_II_UPPER_BOUND.W))

  val configRegs = RegInit(VecInit(Seq.fill(II_UPPER_BOUND)(0.U(configWidth.W))))

  //io.outConfig := configRegs(cycleReg - 1.U(LOG_II_UPPER_BOUND.W))

  when(state === s_read_write){
    io.outConfig := io.inConfig
  }.otherwise{
    io.outConfig := configRegs(cycleReg)
  }

  when(io.en){
    when(state === s_read_write){
      configRegs(cycleReg) := io.inConfig
      when(cycleReg === io.II){
        state := s_read_only
        cycleReg := 0.U
      }.otherwise{
        cycleReg := cycleReg + 1.U
      }
    }.otherwise{
      when(cycleReg === io.II - 1.U){
        cycleReg := 0.U
      }.otherwise{
        cycleReg := cycleReg + 1.U
      }
    }
  }.otherwise{
    state := s_read_write
    cycleReg := 0.U
  }
}

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

class MultiIIScheduleController extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val waitCycles = Input(Vec(II_UPPER_BOUND, UInt(LOG_SCHEDULE_SIZE)))
    val II = Input(UInt(LOG_II_UPPER_BOUND.W))
    val valid = Output(Bool())
  })
  val scheduleControllers = (0 until II_UPPER_BOUND).toArray.map(t => Module(new ScheduleController))
  val validRegs = RegInit(VecInit(Seq.fill(II_UPPER_BOUND)(false.B)))
  val cycleReg = RegInit((II_UPPER_BOUND-1).U(LOG_II_UPPER_BOUND.W))

  for(i <- 0 until II_UPPER_BOUND){
    val scheduleController = scheduleControllers(i)
    scheduleController.io.en := io.en
    scheduleController.io.waitCycle := io.waitCycles(i)
    validRegs(i) := scheduleController.io.valid
  }

  io.valid := validRegs(cycleReg)

  when(io.en === true.B){
    when(cycleReg === io.II - 1.U){
      cycleReg := 0.U
    }.otherwise{
      cycleReg := cycleReg + 1.U
    }
  }

}

class Alu(funSelect: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_a, 1: input_b
    val configuration = Input(UInt(4.W))
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  def getFunSeq(shamt : UInt) : Seq[(UInt, UInt)] ={
    val funSeq = new ArrayBuffer[(UInt, UInt)]()
//    val funSelect = 0

    for (i <- 0 until ALU_FUN_NUM){
      if((funSelect & (1 << i))> 0){
        i match {
          case 0 => funSeq.append(ALU_ADD -> (input_a + input_b))
          case 1 => funSeq.append(ALU_SUB -> (input_a - input_b))
          case 2 => funSeq.append(ALU_AND -> (input_a & input_b))
          case 3 => funSeq.append(ALU_OR -> (input_a | input_b))
          case 4 => funSeq.append(ALU_XOR -> (input_a ^ input_b))
          case 5 => funSeq.append(ALU_MUL -> (input_a * input_b))
          case 6 => funSeq.append(ALU_SLT -> (input_a.asSInt < input_b.asSInt))
          case 7 => funSeq.append(ALU_SLL -> (input_a << shamt).asUInt())
          case 8 => funSeq.append(ALU_SLTU -> (input_a < input_b))
          case 9 => funSeq.append(ALU_SRL -> (input_a >> shamt).asUInt())
          case 10 => funSeq.append(ALU_SRA -> (input_a.asSInt >> shamt).asUInt)
          case 11 => funSeq.append(ALU_COPY_A -> input_a)
          case 12 => funSeq.append(ALU_COPY_B -> input_b)
        }
      }
    }
    funSeq.toSeq
  }

  val input_a = io.inputs(0)
  val input_b = io.inputs(1)
  val out = io.outs(0)
  val shamt = input_b(4, 0).asUInt

  val funSeq = getFunSeq(shamt)

  when(io.en){
    out := MuxLookup(io.configuration, input_b, funSeq)
//    out := MuxLookup(io.configuration, input_b, Seq(
//      ALU_ADD -> (input_a + input_b),
//      ALU_SUB -> (input_a - input_b),
//      ALU_AND -> (input_a & input_b),
//      ALU_OR -> (input_a | input_b),
//      ALU_XOR -> (input_a ^ input_b),
//      ALU_MUL -> (input_a * input_b),
//      ALU_SLT -> (input_a.asSInt < input_b.asSInt),
//      ALU_SLL -> (input_a << shamt),
//      ALU_SLTU -> (input_a < input_b),
//      ALU_SRL -> (input_a >> shamt),
//      ALU_SRA -> (input_a.asSInt >> shamt).asUInt,
//      ALU_COPY_A -> input_a,
//      ALU_COPY_B -> input_b))
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


class RegisterFiles(log2Regs : Int, numIn : Int, numOut : Int, w :Int) extends Module {
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
//  val const = Mem(1, UInt(w.W))
//  const.write(0.U, io.configuration)


  when(io.en){
    //io.outs(0) := const.read((0.U))
    io.outs(0) := io.configuration
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
  val alu = Module(new Alu(0, 32))
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
//  val outt =io.outs(targets.size - 2)
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

    val base = Input(UInt(writeMem.addr.getWidth.W))
    val len = Input(UInt(writeMem.addr.getWidth.W))
    val out = Flipped(DeqIO(UInt(MEM_OUT_WIDTH.W)))

    val start = Input(Bool())
    val enqEn = Input(Bool())
    val deqEn = Input(Bool())
    val idle = Output(Bool())
  })

  val s_noop :: s_write_only :: s_work :: s_read_only :: Nil = Enum(4)
  val state = RegInit(s_noop)

  val mem = Module(new SimpleDualPortSram(MEM_DEPTH, w))
  val enq_mem = Module(new EnqMem(mem.io.a, MEM_IN_WIDTH))
  val deq_mem = Module(new DeqMem(mem.io.b, MEM_OUT_WIDTH))

  when(state === s_noop){
    when(io.enqEn === true.B){
      state := s_write_only
      enq_mem.io.mem <> mem.io.a
    }.otherwise{
      io.writeMem <> mem.io.a
    }
    io.readMem <> mem.io.b
    deq_mem.io.mem.dout <> DontCare
    enq_mem.io.idle <> io.idle
  }.elsewhen(state === s_write_only) {
    when(io.enqEn === false.B){
      state := s_work
      io.writeMem <> mem.io.a
    }.otherwise{
      enq_mem.io.mem <> mem.io.a
    }
    io.readMem <> mem.io.b
    deq_mem.io.mem.dout <> DontCare
    enq_mem.io.idle <> io.idle
  }.elsewhen(state === s_work){
    when(io.deqEn === true.B){
      state := s_read_only
      deq_mem.io.mem <> mem.io.b
      io.readMem.dout <> DontCare
    }.otherwise{
      io.readMem <> mem.io.b
      deq_mem.io.mem.dout <> DontCare
    }
    io.writeMem <> mem.io.a
    deq_mem.io.idle <> io.idle
  }.otherwise{
    when(io.deqEn === false.B){
      state := s_noop
      io.readMem <> mem.io.b
      deq_mem.io.mem.dout <> DontCare
    }.otherwise{
      deq_mem.io.mem <> mem.io.b
      io.readMem.dout <> DontCare
    }
    io.writeMem <> mem.io.a
    deq_mem.io.idle <> io.idle
  }


//  deq_mem.io.mem <> mem.io.b
//  deq_mem.io.out <> io.out
//  deq_mem.io.len <> io.len


//  deq_mem.io.en := true.B

  mem.clock := clock
  enq_mem.clock := clock

  deq_mem.io.base <> io.base
  deq_mem.io.start <> io.start
  enq_mem.io.base <> io.base
  enq_mem.io.start <> io.start

//  io.readMem <> mem.io.b
//  when(io.enqEn === true.B){
//    enq_mem.io.mem <> mem.io.a
//  }.otherwise{
//    io.writeMem <> mem.io.a
//  }

  enq_mem.io.en <> io.enqEn
  enq_mem.io.in <> io.in

  deq_mem.io.en <> io.deqEn
  deq_mem.io.len <> io.len
  deq_mem.io.out <> io.out

//  enq_mem.io.base <> io.base
//  enq_mem.io.start <> io.start
//  enq_mem.io.idle <> io.idle
}

class LoadStoreUnit(w : Int) extends Module{
  val io = IO(new Bundle {
    //0 for load, 1 for store
    val configuration = Input(UInt(1.W))
    val en = Input(Bool())

    val streamIn = Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))
    val len = Input(UInt(log2Ceil(MEM_DEPTH).W))
    val streamOut = Flipped(DeqIO(UInt(MEM_OUT_WIDTH.W)))
    val base = Input(UInt(log2Ceil(MEM_DEPTH).W))

    val start = Input(Bool())
    val enqEn = Input(Bool())
    val deqEn = Input(Bool())
    val idle = Output(Bool())

    val inputs = Input(MixedVec( UInt(log2Ceil(MEM_DEPTH).W), UInt(w.W)))
    val outs = Output(MixedVec((1 to 1) map { i => UInt(w.W) }))
  })
  val memWrapper = Module(new LSMemWrapper(w))
  memWrapper.io.base <> io.base
  memWrapper.io.start <> io.start
  memWrapper.io.idle <> io.idle
  memWrapper.io.enqEn <> io.enqEn
  memWrapper.io.deqEn <> io.deqEn
  memWrapper.io.len <> io.len
  memWrapper.io.in <> io.streamIn
  memWrapper.io.out <> io.streamOut

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


