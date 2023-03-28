package pillars.util

import chisel3._
import chisel3.util._

abstract class EnqMemProcess {
  def in: DecoupledIO[UInt]
  def idle(): Bool
  def start(base: UInt): Unit
  def run(): Unit
}

object EnqMem {
  def apply(mem: TraitMemWriteIO, in_width: Int): EnqMemProcess = new EnqMemProcess {
    val self = Module(new EnqMem(mem, in_width))

    self.io.in.noenq()
    self.io.en := false.B
    self.io.start := false.B
    self.io.base := 0.U

    def in = self.io.in

    def idle() = self.io.idle

    def start(base: UInt) {
      connect()
      self.io.en := true.B
      self.io.start := true.B
      self.io.base := base
    }

    def run() {
      connect()
      self.io.en := true.B
    }

    def connect() {
      self.io.mem.en <> mem.en
      self.io.mem.we <> mem.we
      self.io.mem.addr <> mem.addr
      self.io.mem.din <> mem.din
      //printf("[EnqMem] in vr %d%d\n", in.valid, in.ready)
    }
  }

  def apply(mem: TraitMemWriteIO): EnqMemProcess = apply(mem, mem.din.getWidth)
}

class EnqMem(mem_io: TraitMemWriteIO, in_width: Int) extends Module {
  def this(mem_io: TraitMemWriteIO) = this(mem_io, mem_io.din.getWidth)

  val io = IO(new Bundle {
    val in = Flipped(EnqIO(UInt(in_width.W)))
    val mem = new MemWriteIO(mem_io.mem_depth, mem_io.mem_width)
    val base = Input(UInt(mem_io.addr.getWidth.W))

    val en = Input(Bool())
    val start = Input(Bool())
    val idle = Output(Bool())
  })

  val s_fetch :: s_exec :: Nil = Enum(2)
  val state = RegInit(s_fetch)

  val manip = SplitOrConcat(mem_io.din.getWidth, io.in.bits.getWidth)

  val mem_index = Reg(UInt(io.mem.addr.getWidth.W))
  val data_in = Reg(UInt(io.in.bits.getWidth.W))
  val word_index = Reg(UInt(manip.factor.W))

  io.in.nodeq()
  io.mem.disable()

  io.idle := (state === s_fetch) && (! io.in.valid)

  when (io.en) {

    when (io.idle && io.start) {
      state := s_fetch
      mem_index := io.base
      word_index := 0.U
    }

    when (state === s_fetch) {
      pull_data()
    }

    manip.mode match {
      case SplitOrConcat.Normal =>
        when (state === s_exec) {
          io.mem.enable()
          io.mem.addr := mem_index
          io.mem.din := data_in
          mem_index := mem_index + 1.U
          pull_data()
        }

      case SplitOrConcat.Split =>
        val multi_word = Wire(Vec(manip.factor, UInt(io.mem.din.getWidth.W)))
        multi_word := data_in.asTypeOf(multi_word)

        when (state === s_exec) {
          //printf("[EnqMem] s_exec mem_idx=%d word_idx=%d\n", mem_index, word_index)
          io.mem.enable()
          io.mem.addr := mem_index
          io.mem.din := multi_word(word_index)
          mem_index := mem_index + 1.U
          word_index := word_index + 1.U
          when (word_index === (manip.factor-1).U) {
            word_index := 0.U
            pull_data()
          }
        }

      case SplitOrConcat.Concat =>
        val multi_word = Reg(Vec(manip.factor, UInt(data_in.getWidth.W)))

        when (state === s_exec) {
          multi_word(word_index) := data_in
          when (word_index === (manip.factor-1).U) {
            io.mem.enable()
            io.mem.addr := mem_index
            // data_in not stored in multi_word yet; use the values at the wires
            val next_multi_word = WireInit(multi_word)
            next_multi_word(word_index) := data_in
            io.mem.din := next_multi_word.asUInt
            mem_index := mem_index + 1.U
            word_index := 0.U
          } .otherwise {
            word_index := word_index + 1.U
          }
          pull_data()
        }
    }
  }

  def pull_data() {
    data_in := io.in.deq()
    when (io.in.fire()) {
      state := s_exec
    } .otherwise {
      state := s_fetch
    }
  }
}
