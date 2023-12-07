package pillars.util

import chisel3._
import chisel3.util._

class WriteRequest[T <: Data](addr_gen: T, data_gen: T) extends Bundle {
  val addr = addr_gen
  val data = data_gen

  //override def cloneType: this.type = new WriteRequest(addr_gen, data_gen).asInstanceOf[this.type]
}

trait TraitMemIO extends TraitMemWriteIO with TraitMemReadIO {
  override def enable() {
    super[TraitMemWriteIO].disable()
    super[TraitMemReadIO].enable()
  }

  def wenable() {
    super[TraitMemReadIO].disable()
    super[TraitMemWriteIO].enable()
  }

  override def disable() {
    super[TraitMemReadIO].disable()
    super[TraitMemWriteIO].disable()
  }

  override def <>(that: TraitMemIO) {
    super[TraitMemReadIO].<>(that)
    super[TraitMemWriteIO].<>(that)
  }
}

trait TraitMemReadIO extends Bundle {
  val mem_depth: Int
  val mem_width: Int

  val en: Bool
  val addr: UInt
  val dout: UInt

  def enable() {
    en := true.B
  }

  def disable() {
    en := false.B
    addr := DontCare
  }

  def <>(that: TraitMemIO) {
    this.en <> that.en
    this.addr <> that.addr
    this.dout <> that.dout
  }

  def <>(that: TraitMemReadIO) {
    this.en <> that.en
    this.addr <> that.addr
    this.dout <> that.dout
  }
}

trait TraitMemWriteIO extends Bundle {
  val mem_depth: Int
  val mem_width: Int

  val en: Bool
  val we: Bool
  val addr: UInt
  val din: UInt

  def enable() {
    en := true.B
    we := true.B
  }

  def disable() {
    en := false.B
    we := false.B
    addr := DontCare
    din := DontCare
  }

  def write(addr: UInt, data: UInt) {
    this.addr := addr
    this.din := data
  }

  def <>(that: TraitMemIO) {
    this.en <> that.en
    this.we <> that.we
    this.addr <> that.addr
    this.din <> that.din
  }

  def <>(that: TraitMemWriteIO) {
    this.en <> that.en
    this.we <> that.we
    this.addr <> that.addr
    this.din <> that.din
  }
}


class MemReadIO(val mem_depth: Int, val mem_width: Int) extends TraitMemReadIO {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(mem_depth).W))
  val dout = Input(UInt(mem_width.W))

  //override def cloneType: this.type = new MemReadIO(mem_depth, mem_width).asInstanceOf[this.type]
}

class MemWriteIO(val mem_depth: Int, val mem_width: Int) extends TraitMemWriteIO {
  val en = Output(Bool())
  val we = Output(Bool())
  val addr = Output(UInt(log2Ceil(mem_depth).W))
  val din = Output(UInt(mem_width.W))

  //override def cloneType: this.type = new MemWriteIO(mem_depth, mem_width).asInstanceOf[this.type]
}

class MemIO(val mem_depth: Int, val mem_width: Int) extends TraitMemIO {
  val en = Output(Bool())
  val we = Output(Bool())
  val addr = Output(UInt(log2Ceil(mem_depth).W))
  val din = Output(UInt(mem_width.W))
  val dout = Input(UInt(mem_width.W))

  //override def cloneType: this.type = new MemIO(mem_depth, mem_width).asInstanceOf[this.type]
}

// exclusive read or write
@deprecated("use SinglePortSram instead", "commit f5a2e57")
class SinglePortMem(WIDTH: Int, DEPTH: Int) extends Module {
  val io = IO(
    Flipped(new MemIO(DEPTH, WIDTH))
  )

  //val mem = SyncReadMem(DEPTH, UInt(WIDTH.W))
  val mem = Mem(DEPTH, UInt(WIDTH.W))

  when(io.en) {
    when(io.we) {
      mem.write(io.addr, io.din)
      io.dout := DontCare
      //io.dout_valid := false.B
    }.otherwise {
      io.dout := mem.read(io.addr)
      //io.dout_valid := true.B
    }
  }.otherwise {
    io.dout := DontCare
    //io.dout_valid := false.B
  }
}

// write port A + read port B
@deprecated("use SimpleDualPortSram instead", "commit f5a2e57")
class SimpleDualPortMem(WIDTH: Int, DEPTH: Int) extends Module {
  val io = IO(new Bundle {
    val a = Flipped(new MemWriteIO(DEPTH, WIDTH))
    val b = Flipped(new MemReadIO(DEPTH, WIDTH))
  })

  //val mem = SyncReadMem(DEPTH, UInt(WIDTH.W))
  val mem = Mem(DEPTH, UInt(WIDTH.W))

  when(io.a.en && io.a.we) {
    mem.write(io.a.addr, io.a.din)
  }

  when(io.b.en) {
    io.b.dout := mem.read(io.b.addr)
  }.otherwise {
    io.b.dout := DontCare
  }
}

// exclusive read or write
class SinglePortSram(mem_depth: Int, mem_width: Int) extends Module {
  val io = IO(
    Flipped(new MemIO(mem_depth, mem_width))
  )

  val mem = Mem(mem_depth, UInt(mem_width.W))
  val dout = Reg(UInt(io.dout.getWidth.W))

  io.dout := dout

  when(io.en) {
    when(io.we) {
      mem.write(io.addr, io.din)
    }.otherwise {
      dout := mem.read(io.addr) // buffered; io.dout available in next cycle
    }
  }

  //printf("[SinglePortMem] %d %d %x %x %x\n", io.en, io.we, io.addr, io.din, io.dout)
}

// write port A + read port B
class SimpleDualPortSram(mem_depth: Int, mem_width: Int, useBlackBox: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val a = Flipped(new MemWriteIO(mem_depth, mem_width))
    val b = Flipped(new MemReadIO(mem_depth, mem_width))
  })

  if (useBlackBox) {
    val bb = Module(new sram_dp_32bit_freepdk45(mem_depth, mem_width))
    bb.io.clk0 := clock
    bb.io.clk1 := clock
    bb.io.addr0 := io.a.addr
    bb.io.csb0 := !(io.a.en && io.a.we)
    bb.io.din0 := io.a.din
    bb.io.addr1 := io.b.addr
    bb.io.csb1 := !io.b.en
    io.b.dout := bb.io.dout1
  } else {
    val mem = Mem(mem_depth, UInt(mem_width.W))
    val dout = Reg(UInt(io.b.dout.getWidth.W))

    io.b.dout := dout

    when(io.a.en && io.a.we) {
      mem.write(io.a.addr, io.a.din)
    }

    when(io.b.en) {
      dout := mem.read(io.b.addr) // buffered; io.b.dout available in next cycle
    }
  }

}

class sram_dp_32bit_freepdk45(mem_depth: Int, mem_width: Int)
  extends BlackBox(Map("DATA_WIDTH" -> mem_width,
    "ADDR_WIDTH" -> log2Ceil(mem_depth),
    "DELAY" -> 0,
    "VERBOSE" -> 0)) with HasBlackBoxResource {
  val ADDR_WIDTH = log2Ceil(mem_depth)
  val io = IO(new Bundle() {
    val clk0 = Input(Clock())
    val csb0 = Input(Bool())
    val addr0 = Input(UInt(ADDR_WIDTH.W))
    val din0 = Input(UInt(mem_width.W))
    val clk1 = Input(Clock())
    val csb1 = Input(Bool())
    val addr1 = Input(UInt(ADDR_WIDTH.W))
    val dout1 = Output(UInt(mem_width.W))
  })

  addResource("/sram_dp_32bit_freepdk45.v")
}
