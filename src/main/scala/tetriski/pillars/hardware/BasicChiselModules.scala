package tetriski.pillars.hardware

import chisel3.util._
import chisel3.{Bundle, Input, Module, Output, UInt, Vec, _}
import tetriski.pillars.hardware.PillarsConfig._
import tetriski.pillars.util._

import scala.collection.mutable.ArrayBuffer


//Deprecated because bad frequency.
//class RegNextN(w: Int) extends Module {
//  val io = IO(new Bundle {
//    val latency = Input(UInt((LOG_SCHEDULE_SIZE).W))
//    val input = Input(UInt(w.W))
//    val out = Output(UInt(w.W))
//  })
//  val regArray = RegInit(VecInit(Seq.fill((1 << LOG_SCHEDULE_SIZE) - 1)(0.U(w.W))))
//  regArray(0) := io.input
//  for (i <- 1 until (1 << LOG_SCHEDULE_SIZE) - 1) {
//    regArray(i) := regArray(i - 1)
//  }
//  when(io.latency > 0.U) {
//    io.out := regArray(io.latency - 1.U)
//  }.otherwise {
//    io.out := io.input
//  }
//}

/** A module which postpones the input date for "latency" clock cycles.
 *
 * @param w the data width
 */
class RegNextN(w: Int) extends Module {
  val io = IO(new Bundle {
    val latency = Input(UInt((LOG_SKEW_LENGTH).W))
    val input = Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })
  val regArray = RegInit(VecInit(Seq.fill(SKEW_REGISTER_NUM)(0.U(w.W))))
  val posReg = RegInit(0.U(LOG_SKEW_LENGTH.W))

  when(io.latency > 0.U) {
    io.out := regArray(posReg - io.latency)
    regArray(posReg) := io.input
  }.otherwise {
    io.out := io.input
  }
  posReg := posReg + 1.U

}

/** A module which achieves synchronization of "input0" and "input1" with "skewing".
 *
 * @param w the data width
 */
class Synchronizer(w: Int) extends Module {
  val io = IO(new Bundle {
    val skewing = Input(UInt((LOG_SKEW_LENGTH + 1).W))
    val input0 = Input(UInt(w.W))
    val input1 = Input(UInt(w.W))
    val skewedInput0 = Output(UInt(w.W))
    val skewedInput1 = Output(UInt(w.W))
  })

  val regNextN = Module(new RegNextN(w))
  val latency = io.skewing(LOG_SKEW_LENGTH - 1, 0)
  val signal = io.skewing(LOG_SKEW_LENGTH, LOG_SKEW_LENGTH)
  regNextN.io.latency := latency

  when(signal === 1.U) {
    regNextN.io.input := io.input0
    io.skewedInput0 := regNextN.io.out
    io.skewedInput1 := io.input1
  }.otherwise {
    regNextN.io.input := io.input1
    io.skewedInput1 := regNextN.io.out
    io.skewedInput0 := io.input0
  }
}

/** A module which repeats stored configurations every II cycles,
 * and distributes configurations to modules driven by it.
 *
 * @param configWidth the width of the configuration in a reconfiguration cycle
 */
class ConfigController(configWidth: Int, name: String = "ConfigController") extends Module {
  override def desiredName = name

  val io = IO(new Bundle {
    val en = Input(Bool())
    val II = Input(UInt(LOG_II_UPPER_BOUND.W))
    val inConfig = Input(UInt(configWidth.W))
    val outConfig = Output(UInt(configWidth.W))
  })

  val s_read_write :: s_read_only :: Nil = Enum(2)
  val state = RegInit(s_read_write)
  val cycleReg = RegInit(0.U(LOG_II_UPPER_BOUND.W))

  val configRegs = RegInit(VecInit(Seq.fill(II_UPPER_BOUND)(0.U(configWidth.W))))

  when(state === s_read_write) {
    io.outConfig := 0.U
  }.otherwise {
    io.outConfig := configRegs(cycleReg)
  }

  when(io.en) {
    when(state === s_read_write) {
      configRegs(cycleReg) := io.inConfig
      when(cycleReg === io.II) {
        state := s_read_only
        cycleReg := 0.U
      }.otherwise {
        cycleReg := cycleReg + 1.U
      }
    }.otherwise {
      when(cycleReg === io.II - 1.U) {
        cycleReg := 0.U
      }.otherwise {
        cycleReg := cycleReg + 1.U
      }
    }
  }.otherwise {
    state := s_read_write
    cycleReg := 0.U
  }
}

class ScheduleController extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val waitCycle = Input(UInt(LOG_SCHEDULE_SIZE.W))
    val valid = Output(Bool())
  })

  val cycleReg = Reg(UInt(LOG_SCHEDULE_SIZE.W))

  io.valid := (cycleReg >= (io.waitCycle + 1.U)) && io.en

  /** If waitCycle == (1 << LOG_SCHEDULE_SIZE) - 1,
   * it means the "valid" signal should be always false.
   */
  when(io.waitCycle === ((1 << LOG_SCHEDULE_SIZE) - 1).U){
    io.valid := false.B
  }

  when(io.en) {
    when(cycleReg + 1.U =/= 0.U) {
      cycleReg := cycleReg + 1.U
    }
  }.otherwise {
    cycleReg := 0.U
  }
}

/** A module which controls when modules should fire.
 * It also dispatchs "skewing" to modules.
 *
 */
class MultiIIScheduleController(DualInput: Boolean = true) extends Module {
  val skewWidth = if (DualInput) {
    SKEW_WIDTH
  } else {
    0
  }

  val SWidth = LOG_SCHEDULE_SIZE + skewWidth

  val io = IO(new Bundle {
    val en = Input(Bool())
    val enConfig = Input(Bool())
    //    val schedules = Input(Vec(II_UPPER_BOUND, UInt(SWidth.W)))
    val schedule = Input(UInt(SWidth.W))
    val II = Input(UInt(LOG_II_UPPER_BOUND.W))
    val valid = Output(Bool())
    val skewing = Output(UInt(skewWidth.W))
  })

  val cycleReg = RegInit((II_UPPER_BOUND - 1).U(LOG_II_UPPER_BOUND.W))

  val SConfigController = Module(new ConfigController(SWidth, "SConfigController"))

  SConfigController.io.en := io.enConfig
  SConfigController.io.inConfig := io.schedule
  SConfigController.io.II := io.II
  val SConfig = SConfigController.io.outConfig

  if (LOG_SCHEDULE_SIZE > 0) {
    val waitCycle = SConfig(LOG_SCHEDULE_SIZE - 1, 0)
    val scheduleController = Module(new ScheduleController)
    scheduleController.io.en := io.en
    scheduleController.io.waitCycle := waitCycle
    io.valid := scheduleController.io.valid

    //    val scheduleControllers = (0 until II_UPPER_BOUND).toArray.map(_ => Module(new ScheduleController))
    //    val validRegs = RegInit(VecInit(Seq.fill(II_UPPER_BOUND)(false.B)))
    //    for (i <- 0 until II_UPPER_BOUND) {
    //      val scheduleController = scheduleControllers(i)
    //      scheduleController.io.en := io.en
    //      scheduleController.io.waitCycle := io.schedules(i)(LOG_SCHEDULE_SIZE - 1, 0)
    //      validRegs(i) := scheduleController.io.valid
    //    }
    //    io.valid := validRegs(cycleReg)
  } else {
    io.valid := io.en
  }


  if (skewWidth > 0) {
    io.skewing := SConfig(LOG_SCHEDULE_SIZE + SKEW_WIDTH - 1, LOG_SCHEDULE_SIZE)
    //    io.skewing := io.schedules(cycleReg)(LOG_SCHEDULE_SIZE + SKEW_WIDTH - 1, LOG_SCHEDULE_SIZE)
  } else {
    io.skewing := DontCare
  }

  when(io.en === true.B) {
    when(cycleReg === io.II - 1.U) {
      cycleReg := 0.U
    }.otherwise {
      cycleReg := cycleReg + 1.U
    }
  }

}

/** An arithmetic logical unit which can perform an arbitrary subset of optional operations.
 *
 * @param funSelect the subset of optional operations
 * @param w         the data width
 */
class Alu(funSelect: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val skewing = Input(UInt((SKEW_WIDTH).W))
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_a, 1: input_b
    val configuration = Input(UInt(4.W))
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  /** Translates the subset of optional operations into hardware.
   *
   * @param shamt the bottom log2Up(w) bits of "input_b"
   */
  def getFunSeq(shamt: UInt = null): Seq[(UInt, UInt)] = {
    val funSeq = new ArrayBuffer[(UInt, UInt)]()

    for (i <- 0 until ALU_FUN_NUM) {
      if ((funSelect & (1 << i)) > 0) {
        i match {
          case 0 => funSeq.append(ALU_ADD -> (input_a + input_b))
          case 1 => funSeq.append(ALU_SUB -> (input_a - input_b))
          case 2 => funSeq.append(ALU_AND -> (input_a & input_b))
          case 3 => funSeq.append(ALU_OR -> (input_a | input_b))
          case 4 => funSeq.append(ALU_XOR -> (input_a ^ input_b))
          case 5 => funSeq.append(ALU_MUL -> (input_a * input_b))
          case 6 => funSeq.append(ALU_SLT -> (input_a.asSInt < input_b.asSInt))
          case 7 => funSeq.append(ALU_SHLL -> (input_a << shamt).asUInt())
          //          case 7 => funSeq.append(ALU_SHLL -> (input_a << input_b).asUInt())
          case 8 => funSeq.append(ALU_SHLA -> (input_a.asSInt << shamt).asUInt)
          case 9 => funSeq.append(ALU_SHRL -> (input_a >> shamt).asUInt())
          //          case 9 => funSeq.append(ALU_SHRL -> (input_a >> input_b).asUInt())
          case 10 => funSeq.append(ALU_SHRA -> (input_a.asSInt >> shamt).asUInt)
          case 11 => funSeq.append(ALU_DIV -> input_a / input_b)
          case 12 => funSeq.append(ALU_COPY_A -> input_a)
          case 13 => funSeq.append(ALU_COPY_B -> input_b)
        }
      }
    }
    funSeq
  }

  var input_a = io.inputs(0)
  var input_b = io.inputs(1)

  if (LOG_SKEW_LENGTH > 0) {
    if (USE_RELATIVE_SKEW) {
      val synchronizer = Module(new Synchronizer(w))
      synchronizer.io.input0 := input_a
      synchronizer.io.input1 := input_b

      synchronizer.io.skewing := io.skewing

      input_a = synchronizer.io.skewedInput0
      input_b = synchronizer.io.skewedInput1
    } else {
      val regNextNa = Module(new RegNextN(w))
      val regNextNb = Module(new RegNextN(w))
      regNextNa.io.input := input_a
      regNextNa.io.latency := io.skewing(LOG_SKEW_LENGTH - 1, 0)
      regNextNb.io.input := input_b
      regNextNb.io.latency := io.skewing(2 * LOG_SKEW_LENGTH - 1, LOG_SKEW_LENGTH)

      input_a = regNextNa.io.out
      input_b = regNextNb.io.out
    }
  }

  val out = io.outs(0)
  val shamt = input_b(log2Up(w), 0).asUInt

  val funSeq = getFunSeq(shamt)

  when(io.en) {
    out := MuxLookup(io.configuration, input_b, funSeq)
  }.otherwise {
    for (out <- io.outs) {
      out := 0.U
    }
  }
}

class DefaultBasicModule(w: Int, configBits: Int, numIn: Int, numOut: Int) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val configuration = Input(UInt(configBits.W))
    val inputs = Input(MixedVec((1 to numIn) map { _ => UInt(w.W) }))
    val outs = Output(MixedVec((1 to numOut) map { _ => UInt(w.W) }))
  })
}

/** An reconfigurable counter.
 * The configuration consists of freq (interval cycles of value change), end (stop value),
 * step (value changes per interval), and init (initial value).
 *
 * @example If freq = 2, end = 13, step = 2, and init = 3,
 *          the output should be 3, 0, 5, 0, 7, 0, 9, 0, 11, 0, 0, 0.....
 * @param w the data width
 */
class Counter(w: Int) extends DefaultBasicModule(w, 4 * w, 0, 1) {

  val freq = io.configuration(4 * w - 1, 3 * w)
  val end = io.configuration(3 * w - 1, 2 * w)
  val step = io.configuration(2 * w - 1, w)
  val init = io.configuration(w - 1, 0)

  val reg = RegInit(0.U(w.W))
  val firing = RegInit(false.B)
  val finish = RegInit(false.B)

  val intervalReg = RegInit(0.U(w.W))
  val intervalStart = RegInit(false.B)

  io.outs(0) := 0.U
  when(io.en) {
    when(!firing) {
      reg := init
      firing := true.B
      io.outs(0) := init
      when(freq === 1.U) {
        intervalStart := true.B
      }
    }.otherwise {
      when(intervalReg + 1.U === freq) {
        intervalReg := 0.U
      }.otherwise {
        intervalReg := intervalReg + 1.U
        intervalStart := true.B
      }

      when(intervalReg + 1.U === freq && intervalStart) {
        when(reg + step =/= end) {
          reg := reg + step
          io.outs(0) := reg + step
        }.otherwise {
          finish := true.B
        }
      }

    }
  }

}

/** A register file which can perform an arbitrary subset of optional operations.
 *
 * @param log2Regs log2(number of registers)
 * @param numIn    the number of input ports
 * @param numOut   the number of output ports
 * @param w        the data width
 */
class RegisterFile(log2Regs: Int, numIn: Int, numOut: Int, w: Int)
  extends DefaultBasicModule(w, log2Regs * (numIn + numOut) + 1, numIn, numOut) {
  if (log2Regs == 0 && numIn == 1 && numOut == 1) {
    //single register
    val reg = RegInit(0.U(w.W))
    reg := io.inputs(0)
    when(io.configuration === 1.U) {
      io.outs(0) := reg
    }.otherwise {
      io.outs(0) := 0.U
    }
  } else {
    //register files
    val targets = (0 until numIn + numOut).toList.map(t => log2Regs)
    val dispatch = Module(new Dispatch((log2Regs * (numIn + numOut)), targets))
    val configSize = log2Regs * (numIn + numOut)
    dispatch.io.configuration := io.configuration(configSize - 1, 0)
    dispatch.io.en <> io.en
    val forbidden = io.configuration(configSize, configSize)

    val regs = RegInit(VecInit(Seq.fill(Math.pow(2, log2Regs).toInt)(0.U(w.W))))

    when(forbidden === false.B) {
      for (i <- 0 until numIn) {
        regs(dispatch.io.outs(i)) := io.inputs(i)
      }
    }
    for (i <- 0 until numOut) {
      io.outs(i) := regs(dispatch.io.outs(i + numIn))
    }
  }
}

/** A single output multiplexer.
 *
 * @param numIn the number of input ports
 * @param w     the data width
 */
class Multiplexer(numIn: Int, w: Int) extends DefaultBasicModule(w, log2Up(numIn), numIn, 1) {
  //  val input0 = io.inputs(0)
  //  val input1 = io.inputs(1)
  val out = io.outs(0)
  val selectArray = (0 to numIn - 1).map(i => i.U -> io.inputs(i))
  val muxIn0 = MuxLookup(io.configuration, io.inputs(0), selectArray)

  io.outs(0) := muxIn0
}

//Deprecated
//  object Common {
//    def constUnitBody(configuration: UInt, outs: MixedVec[UInt]): Unit = {
//      val const = Mem(1, UInt(w.W))
//      const.write(0.U, configuration)
//      outs(0) := const.read((0.U))
//    }
//  }

//Deprecated
//class ConstUnit(w :Int, foo: (UInt, MixedVec[UInt]) => Unit) extends Module {
//  val io = IO(new Bundle {
//    val configuration = Input(UInt(w.W))
//    val outs = Output(MixedVec((1 to 1) map { i => UInt(w.W) }))
//  })
//  foo(io.configuration, io.outs)
//}

/** A simple const unit.
 *
 * @param w the data width
 */
class ConstUnit(w: Int) extends DefaultBasicModule(w, w, 0, 1) {
  io.outs(0) := io.configuration
}

/** A Chisel ADRES PE.
 *
 * @deprecated
 * @param w the data width
 */
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

  val rf = Module(new RegisterFile(1, 1, 2, 32))
  val alu = Module(new Alu(0, 32))
  val targets = List(3, 3, 4, 3)
  val dispatch = Module(new Dispatch(13, targets))
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

/** A module which can dispatch the input configuration to several modules.
 *
 * @param wIn     the data width of the input configuration
 * @param targets a list of data width of the output ports
 * @param regOut  a parameter indicating whether there should be registers before the output ports
 */
class Dispatch(wIn: Int, targets: List[Int], regOut: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())

    val configuration = Input(UInt(wIn.W))
    val outs = Output(MixedVec(targets.map { i => UInt(i.W) }))
  })

  var offset: Int = 0
  var i = 0
  for (elem <- targets) {
    if (regOut) {
      io.outs(i) := RegNext(io.configuration(offset + elem - 1, offset))
    } else {
      io.outs(i) := io.configuration(offset + elem - 1, offset)
    }
    i += 1
    offset += elem
  }
}

/** A dispatch for testing.
 *
 * @deprecated
 * @param  wIn    the data width of the input configuration
 * @param targets a list of data width of the output ports
 */
class DispatchT(wIn: Int, targets: List[Int]) extends Module {
  val io = IO(new Bundle {
    val configuration = Input(UInt(wIn.W))
    val outs = Output(MixedVec(targets.map { i => UInt(i.W) }))
  })
  val outt = io.outs(targets.size - 2)
  var i = 0
  var offset: Int = 0
  for (elem <- targets) {
    io.outs(i) := io.configuration(offset + elem - 1, offset)
    i += 1
    offset += elem
  }

}

/** A module which is the kernel of load/store unit.
 * There are a state machine in this module,
 * where {@code s_write_only}, {@code s_work} and {@code s_read_only}
 * are corresponding to pre-process, activating process and post-process.
 *
 * @param w the data width
 */
class LSMemWrapper(w: Int) extends Module {
  val io = IO(new Bundle {
    val workEn = Input(Bool())

    val in = Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))
    val memReset = Input(Bool())

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
  MEM_IN_WIDTH = w
  MEM_OUT_WIDTH = w

  val s_noop :: s_write_only :: s_work :: s_read_only :: Nil = Enum(4)
  val state = RegInit(s_noop)


  val mem = Module(new SimpleDualPortSram(MEM_DEPTH, w))
  mem.io.memReset := io.memReset

  val enq_mem = Module(new EnqMem(mem.io.a, MEM_IN_WIDTH))
  val deq_mem = Module(new DeqMem(mem.io.b, MEM_OUT_WIDTH))

  when(state === s_noop) {
    when(io.workEn === true.B) {
      state := s_work
      io.writeMem <> mem.io.a
    }.otherwise {
      when(io.enqEn === true.B) {
        state := s_write_only
        enq_mem.io.mem <> mem.io.a
      }.otherwise {
        io.writeMem <> mem.io.a
      }
    }
    io.readMem <> mem.io.b
    deq_mem.io.mem.dout <> DontCare
    enq_mem.io.idle <> io.idle
  }.elsewhen(state === s_write_only) {
    when(io.enqEn === false.B && io.workEn === true.B) {
      state := s_work
      io.writeMem <> mem.io.a
    }.otherwise {
      enq_mem.io.mem <> mem.io.a
    }
    io.readMem <> mem.io.b
    deq_mem.io.mem.dout <> DontCare
    enq_mem.io.idle <> io.idle
  }.elsewhen(state === s_work) {
    when(io.deqEn === true.B) {
      state := s_read_only
      deq_mem.io.mem <> mem.io.b
      io.readMem.dout <> DontCare
    }.otherwise {
      io.readMem <> mem.io.b
      deq_mem.io.mem.dout <> DontCare
    }
    io.writeMem <> mem.io.a
    deq_mem.io.idle <> io.idle
  }.otherwise {
    when(io.deqEn === false.B) {
      state := s_noop
      io.readMem <> mem.io.b
      deq_mem.io.mem.dout <> DontCare
    }.otherwise {
      deq_mem.io.mem <> mem.io.b
      io.readMem.dout <> DontCare
    }
    io.writeMem <> mem.io.a
    deq_mem.io.idle <> io.idle
  }

  mem.clock := clock
  enq_mem.clock := clock

  deq_mem.io.base <> io.base
  deq_mem.io.start <> io.start
  enq_mem.io.base <> io.base
  enq_mem.io.start <> io.start

  enq_mem.io.en <> io.enqEn
  enq_mem.io.in <> io.in

  deq_mem.io.en <> io.deqEn
  deq_mem.io.len <> io.len
  deq_mem.io.out <> io.out

}

/** A load/store unit which can perform load/store during the runtime of CGRA,
 * and direct memory access (DMA) for transferring data during pre-process and post-process.
 *
 * @param w the data width
 */
class LoadStoreUnit(w: Int) extends Module {
  val io = IO(new Bundle {
    //0 for load, 1 for store
    val configuration = Input(UInt(1.W))
    val en = Input(Bool())
    val skewing = Input(UInt((SKEW_WIDTH).W))

    val memReset = Input(Bool())

    val streamIn = Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))
    val len = Input(UInt(log2Ceil(MEM_DEPTH).W))
    val streamOut = Flipped(DeqIO(UInt(MEM_OUT_WIDTH.W)))
    val base = Input(UInt(log2Ceil(MEM_DEPTH).W))

    val start = Input(Bool())
    val enqEn = Input(Bool())
    val deqEn = Input(Bool())
    val idle = Output(Bool())

    val inputs = Input(MixedVec(UInt(log2Ceil(MEM_DEPTH).W), UInt(w.W)))
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
  memWrapper.io.workEn <> io.en
  memWrapper.io.memReset <> io.memReset

  /** The address where to load/store data.
   */
  var addr = io.inputs(0)
  /** The input data which is only used for storing.
   */
  var dataIn = io.inputs(1)

  if (LOG_SKEW_LENGTH > 0) {
    if (USE_RELATIVE_SKEW) {
      val synchronizer = Module(new Synchronizer(w))
      synchronizer.io.input0 := addr
      synchronizer.io.input1 := dataIn

      synchronizer.io.skewing := io.skewing

      addr = synchronizer.io.skewedInput0
      dataIn = synchronizer.io.skewedInput1
    } else {
      val regNextNaddr = Module(new RegNextN(w))
      val regNextNdataIn = Module(new RegNextN(w))
      regNextNaddr.io.input := addr
      regNextNaddr.io.latency := io.skewing(LOG_SKEW_LENGTH - 1, 0)
      regNextNdataIn.io.input := dataIn
      regNextNdataIn.io.latency := io.skewing(2 * LOG_SKEW_LENGTH - 1, LOG_SKEW_LENGTH)

      addr = regNextNaddr.io.out
      dataIn = regNextNdataIn.io.out
    }
  }
  val out = io.outs(0)

  val readMem = memWrapper.io.readMem
  val writeMem = memWrapper.io.writeMem

  io.outs(0) := readMem.dout

  when(io.en) {
    readMem.addr := addr
    writeMem.addr := addr
    writeMem.din := dataIn
    when(io.configuration === 0.U) {
      readMem.en := true.B
      writeMem.en := false.B
      writeMem.we := false.B
    }.otherwise {
      readMem.en := false.B
      writeMem.en := true.B
      writeMem.we := true.B
    }
  }.otherwise {
    readMem.en := false.B
    writeMem.en := false.B
    writeMem.we := false.B
    readMem.addr := DontCare
    writeMem.addr := DontCare
    writeMem.din := DontCare
  }
}


