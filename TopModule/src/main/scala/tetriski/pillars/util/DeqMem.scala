package tetriski.pillars.util


import chisel3._
import chisel3.util._

abstract class DeqMemProcess {
  def out: DecoupledIO[UInt]
  def idle(): Bool
  def start(base: UInt, len: UInt): Unit
  def run(): Unit
}

object DeqMem {
  def apply(mem: TraitMemReadIO, out_width: Int): DeqMemProcess = new DeqMemProcess {
    val self = Module(new DeqMem(mem, out_width))

    self.io.out.nodeq()
    self.io.mem.dout := DontCare
    self.io.en := false.B
    self.io.start := false.B
    self.io.base := 0.U
    self.io.len := 0.U

    def out = self.io.out

    def idle() = self.io.idle

    def start(base: UInt, len: UInt) {
      connect()
      self.io.en := true.B
      self.io.start := true.B
      self.io.base := base
      self.io.len := len
    }

    def run() {
      connect()
      self.io.en := true.B
    }

    def connect() {
      self.io.mem.en <> mem.en
      self.io.mem.addr <> mem.addr
      self.io.mem.dout <> mem.dout
    }
  }

  def apply(mem: TraitMemReadIO): DeqMemProcess = apply(mem, mem.dout.getWidth)
}

object EnqAddrDeqMem {
  def apply(enq_io: => DecoupledIO[UInt], mem_io: => TraitMemReadIO, deq_io: => DecoupledIO[UInt]) = new {
    val self = Module(new EnqAddrDeqMem(mem_io))

    self.io.iaddr.noenq()
    self.io.mem.dout := DontCare
    self.io.odata.nodeq()

    def idle() = self.io.idle

    def run() {
      connect()
    }

    def connect() {
      self.io.iaddr <> enq_io
      self.io.mem.en <> mem_io.en
      self.io.mem.addr <> mem_io.addr
      self.io.mem.dout <> mem_io.dout
      self.io.odata <> deq_io
    }
  }
}

class DeqMem(mem_io: TraitMemReadIO, out_width: Int) extends Module {
  def this(mem_io: TraitMemReadIO) = this(mem_io, mem_io.dout.getWidth)

  val io = IO(new Bundle {
    val mem = new MemReadIO(mem_io.mem_depth, mem_io.mem_width)
    val out = Flipped(DeqIO(UInt(out_width.W)))
    val base = Input(UInt(mem_io.addr.getWidth.W))
    val len = Input(UInt(mem_io.addr.getWidth.W))

    val en = Input(Bool())
    val start = Input(Bool())
    val idle = Output(Bool())
  })

  val s_idle :: s_fetch :: s_exec :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val manip = SplitOrConcat(io.out.bits.getWidth, io.mem.dout.getWidth)

  val mem_index = Reg(UInt(io.mem.addr.getWidth.W))
  val mem_data = Reg(UInt(io.mem.dout.getWidth.W))
  val word_index = Reg(UInt(manip.factor.W))
  val remain = Reg(UInt(io.mem.addr.getWidth.W))

  val iaddr_hs = Module(new Handshake(UInt(io.mem.addr.getWidth.W)))
  def iaddr = iaddr_hs.io.enq

  val odata_hs = Module(new Handshake(UInt(io.mem.dout.getWidth.W)))
  def odata = odata_hs.io.deq

  val imo = EnqAddrDeqMem(iaddr_hs.io.deq, io.mem, odata_hs.io.enq)

  odata_hs.io.enq.noenq()
  odata_hs.io.deq.nodeq()
  iaddr_hs.io.enq.noenq()
  iaddr_hs.io.deq.nodeq()

  io.mem.disable()
  io.out.noenq()
  io.idle := (state === s_idle)

  imo.run()

  when (io.en) {

    when (io.idle && io.start) {
      state := s_fetch
      mem_index := io.base
      word_index := 0.U
      remain := io.len
    }

    when (state === s_fetch) {
      fetch()
    }

    manip.mode match {
      case SplitOrConcat.Normal =>
        when (state === s_exec) {
          io.out.enq(mem_data)
          when (io.out.fire()) {
            fetch()
          }
        }

      case SplitOrConcat.Split =>
        val multi_word = Wire(Vec(manip.factor, UInt(io.out.bits.getWidth.W)))
        multi_word := mem_data.asTypeOf(multi_word)

        when (state === s_exec) {
          io.out.enq(multi_word(word_index))
          when (io.out.fire()) {
            val next_word_index = word_index + 1.U
            word_index := next_word_index
            when (next_word_index === manip.factor.U) {
              fetch()
              word_index := 0.U
            }
          }
        }

      case SplitOrConcat.Concat =>
        val multi_word = Reg(Vec(manip.factor, UInt(io.mem.dout.getWidth.W)))

        when (state === s_exec) {
          val next_word_index = word_index + 1.U
          when (next_word_index === manip.factor.U) {
            val next_multi_word = WireInit(multi_word)
            next_multi_word(word_index) := mem_data
            io.out.enq(next_multi_word.asUInt)
            when (io.out.fire()) {
              printf("[DeqMem] %x %x = mem[%d] %d\n", next_multi_word.asUInt, multi_word.asUInt, mem_index-manip.factor.U, manip.factor.U)
              fetch()
              word_index := 0.U
            }
          } .otherwise {
            multi_word(word_index) := mem_data
            word_index := next_word_index
            fetch()
          }
        }
    }

  }

  def fetch() {
    // enq addr
    when (remain > 0.U) {
      iaddr.enq(mem_index)
      when (iaddr.fire()) {
        mem_index := mem_index + 1.U
        remain := remain - 1.U
      }
    }
    // deq data
    mem_data := odata.deq()
    when (odata.fire()) {
      state := s_exec
    } .elsewhen (imo.idle() === false.B) {
      state := s_fetch
    } .otherwise {
      state := s_idle
    }
  }
}

class EnqAddrDeqMem(mem_io: TraitMemReadIO) extends Module {
  val io = IO(new Bundle {
    val iaddr = Flipped(EnqIO(UInt(mem_io.addr.getWidth.W)))
    val mem = new MemReadIO(mem_io.mem_depth, mem_io.mem_width)
    val odata = Flipped(DeqIO(UInt(mem_io.dout.getWidth.W)))
    val idle = Output(Bool())
  })

  val token = RegInit(false.B)
  val next_token = Wire(Bool())

  io.iaddr.nodeq()
  io.mem.disable()
  io.odata.noenq()

  io.idle := (token === false.B) && (io.iaddr.valid === false.B)

  next_token := token

  //printf("[imo] %x (%x %x %x) (%x %x %x)\n", token, io.iaddr.valid, io.iaddr.ready, io.iaddr.bits, io.odata.valid, io.odata.ready, io.odata.bits)

  when (token) {
    io.odata.enq(io.mem.dout)
    when (io.odata.fire()) {
      token := false.B
      next_token := false.B
    }
  }

  when (next_token === false.B) {
    val addr = io.iaddr.deq()
    when (io.iaddr.fire()) {
      token := true.B
      io.mem.enable()
      io.mem.addr := addr
    }
  }
}

