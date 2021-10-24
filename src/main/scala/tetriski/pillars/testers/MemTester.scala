package tetriski.pillars.testers

import chisel3._
import chisel3.util._
import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import tetriski.pillars.hardware.LoadStoreUnit
import tetriski.pillars.util._

class SinglePortSramUnitTester(c: SinglePortSram) extends PeekPokeTester(c) {
  val addr = Array(0x0000, 0xffff, 0xcafe)
  val data = Array(BigInt("cccccccc",16), BigInt("deadbeef",16), BigInt("cdcdcdcd",16))

  poke(c.io.en, 1) // enable mem

  // first round of write

  poke(c.io.we, 1) // write mode
  for (x <- addr) {
    poke(c.io.addr, x)
    poke(c.io.din, data(0))
    step(1)
  }

  // first round of read

  poke(c.io.we, 0) // read mode
  for (x <- addr) {
    poke(c.io.addr, x)
    step(1)
    expect(c.io.dout, data(0))
  }

  // second round of write

  poke(c.io.we, 1) // write mode
  for (x <- addr) {
    poke(c.io.addr, x)
    poke(c.io.din, data(1))
    step(1)
  }

  // second round of read

  poke(c.io.we, 0) // read mode
  for (x <- addr) {
    poke(c.io.addr, x)
    step(1)
    expect(c.io.dout, data(1))
  }

  // third round of write

  poke(c.io.we, 1) // write mode

  poke(c.io.addr, addr(0))
  poke(c.io.din, data(2))
  step(1)

  poke(c.io.en, 0) // temporally disabled

  poke(c.io.addr, addr(1))
  poke(c.io.din, data(2))
  step(1)

  poke(c.io.en, 1) // re-enabled

  poke(c.io.addr, addr(2))
  poke(c.io.din, data(2))
  step(1)

  // third round of read

  poke(c.io.we, 0) // read mode

  poke(c.io.addr, addr(0))
  step(1)
  expect(c.io.dout, data(2))

  poke(c.io.addr, addr(1))
  step(1)
  expect(c.io.dout, data(1))

  poke(c.io.addr, addr(2))
  step(1)
  expect(c.io.dout, data(2))
}

class SimpleDualPortSramUnitTester(c: SimpleDualPortSram) extends PeekPokeTester(c) {
  val data = Array(BigInt("cccccccc",16), BigInt("deadbeef",16), BigInt("cdcdcdcd",16))

  poke(c.io.a.en, 1)
  poke(c.io.a.we, 1)
  poke(c.io.b.en, 1)

  poke(c.io.a.addr, 0xc0de)
  poke(c.io.a.din, data(0))
  step(1)

  poke(c.io.a.addr, 0xc0dd)
  poke(c.io.a.din, data(1))
  poke(c.io.b.addr, 0xc0de)
  step(1)
  expect(c.io.b.dout, data(0))

  poke(c.io.a.we, 0)
  poke(c.io.b.addr, 0xc0dd)
  step(1)
  expect(c.io.b.dout, data(1))
}

class EnqMemWrapper(in_width: Int, mem_width: Int, mem_depth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(EnqIO(UInt(in_width.W)))
    val mem = Flipped(new MemReadIO(mem_depth, mem_width))
    val base = Input(UInt(mem.addr.getWidth.W))
    val start = Input(Bool())
    val idle = Output(Bool())
  })

  val mem = Module(new SimpleDualPortSram(mem_depth, mem_width))
  val enq_mem = Module(new EnqMem(mem.io.a, in_width))

  io.mem <> mem.io.b
  enq_mem.io.en := true.B
  enq_mem.io.in <> io.in
  enq_mem.io.mem <> mem.io.a
  enq_mem.io.base <> io.base
  enq_mem.io.start <> io.start
  enq_mem.io.idle <> io.idle
}
class EnqMemUnitTester(c: EnqMemWrapper) extends PeekPokeTester(c) {
  val idata = c.enq_mem.manip.mode match {
    case SplitOrConcat.Normal =>
      Array(BigInt("cccccccc",16), BigInt("deadbeef",16), BigInt("cdcdcdcd",16))
    case SplitOrConcat.Split =>
      assert(c.enq_mem.manip.factor == 4)
      Array(BigInt("deadc0de" + "cdcdcdcd" + "deadbeef" + "cccccccc",16)) // little endian
    case SplitOrConcat.Concat =>
      assert(c.enq_mem.manip.factor == 4)
      Array(
        BigInt("cc",16), BigInt("cc",16), BigInt("cc",16), BigInt("cc",16),
        BigInt("ef",16), BigInt("be",16), BigInt("ad",16), BigInt("de",16),
        BigInt("cd",16), BigInt("cd",16), BigInt("cd",16), BigInt("cd",16)
      ) // little endian
  }
  val odata = Array(BigInt("cccccccc",16), BigInt("deadbeef",16), BigInt("cdcdcdcd",16))

  poke(c.io.start, 1)
  poke(c.io.base, 1023)
  step(1)

  // push
  for (x <- idata) {
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits, x)
    if (peek(c.io.in.ready) == 0) {
      while (peek(c.io.in.ready) == 0) {
        step(1)
      }
    } else {
      step(1)
    } // exit condition: (c.io.in.ready === true.B) and step()
  }
  poke(c.io.in.valid, 0)

  // exec
  while (peek(c.io.idle) == 0) {
    step(1)
  }

  // read
  poke(c.io.mem.en, 1)
  poke(c.io.mem.addr, 1023)
  step(1)
  expect(c.io.mem.dout, odata(0))

  poke(c.io.mem.en, 1)
  poke(c.io.mem.addr, 1025)
  step(1)
  expect(c.io.mem.dout, odata(2))

  poke(c.io.mem.en, 1)
  poke(c.io.mem.addr, 1024)
  step(1)
  expect(c.io.mem.dout, odata(1))
}

class EnqAddrDeqMemWrapper(mem_io: TraitMemReadIO) extends Module {
  val io = IO(new Bundle {
    val iaddr = Flipped(EnqIO(UInt(mem_io.addr.getWidth.W)))
    val mem = Flipped(new MemWriteIO(math.pow(2,mem_io.addr.getWidth).toInt, mem_io.dout.getWidth))
    val odata = Flipped(DeqIO(UInt(mem_io.dout.getWidth.W)))
  })

  val mem = Module(new SimpleDualPortSram(math.pow(2,mem_io.addr.getWidth).toInt, mem_io.dout.getWidth))
  val imo = EnqAddrDeqMem(io.iaddr, mem.io.b, io.odata)

  mem.io.a <> io.mem

  imo.run()
}
class EnqAddrDeqMemUnitTester(c: EnqAddrDeqMemWrapper) extends PeekPokeTester(c) {
  val addr = Array(0x0000, 0xffff, 0xcafe)
  val data = Array(BigInt("cccccccc",16), BigInt("deadbeef",16), BigInt("cdcdcdcd",16))

  poke(c.io.mem.en, 1)
  poke(c.io.mem.we, 1)

  for (i <- 0 to 2) {
    poke(c.io.mem.addr, addr(i))
    poke(c.io.mem.din, data(i))
    step(1)
  }

  poke(c.io.mem.we, 0)

  var i = 0
  var j = 0
  while (j < 3) {
    push_addr()
    pull_data()
  }

  i = 0; j = 0
  while (j < 3) {
    push_addr()
    pull_data()
    pull_data()
  }

  i = 0; j = 0
  while (j < 3) {
    push_addr()
    push_addr()
    pull_data()
  }

  i = 0; j = 0
  while (j < 3) {
    push_addr()
    push_addr()
    pull_data()
    pull_data()
  }

  def push_addr() {
    if(i <= 2 && peek(c.io.iaddr.ready) == 1) {
      poke(c.io.iaddr.valid, 1)
      poke(c.io.iaddr.bits, addr(i))
      //println(s"push addr($i)")
      i = i + 1
    }
    step(1)
    poke(c.io.iaddr.valid, 0)
  }

  def pull_data() {
    poke(c.io.odata.ready, 1)
    if(peek(c.io.odata.valid) == 1) {
      expect(c.io.odata.bits, data(j))
      //println(s"pull data($j)")
      j = j + 1
    }
    step(1)
    poke(c.io.odata.ready, 0)
  }

}

class DeqMemWrapper(out_width: Int, mem_width: Int, mem_depth: Int) extends Module {
  val io = IO(new Bundle {
    val mem = Flipped(new MemWriteIO(mem_depth, mem_width))
    val out = Flipped(DeqIO(UInt(out_width.W)))
    val base = Input(UInt(mem.addr.getWidth.W))
    val len = Input(UInt(mem.addr.getWidth.W))
    val start = Input(Bool())
    val idle = Output(Bool())
  })

  val mem = Module(new SimpleDualPortSram(mem_depth, mem_width))
  val deq_mem = Module(new DeqMem(mem.io.b, io.out.bits.getWidth))

  io.mem <> mem.io.a
  deq_mem.io.mem <> mem.io.b
  deq_mem.io.out <> io.out
  deq_mem.io.base <> io.base
  deq_mem.io.len <> io.len
  deq_mem.io.start <> io.start
  deq_mem.io.idle <> io.idle
  deq_mem.io.en := true.B
}
class DeqMemUnitTester(c: DeqMemWrapper) extends PeekPokeTester(c) {
  val idata = Array(BigInt("cccccccc",16), BigInt("deadbeef",16), BigInt("cdcdcdcd",16), BigInt("deadc0de",16))
  val odata = c.deq_mem.manip.mode match {
    case SplitOrConcat.Normal =>
      Array(BigInt("cccccccc",16), BigInt("deadbeef",16), BigInt("cdcdcdcd",16))
    case SplitOrConcat.Split =>
      assert(c.deq_mem.manip.factor == 4)
      Array(
        BigInt("cc",16), BigInt("cc",16), BigInt("cc",16), BigInt("cc",16),
        BigInt("ef",16), BigInt("be",16), BigInt("ad",16), BigInt("de",16),
        BigInt("cd",16), BigInt("cd",16), BigInt("cd",16), BigInt("cd",16)
      ) // little endian
    case SplitOrConcat.Concat =>
      assert(c.deq_mem.manip.factor == 4)
      Array(BigInt("deadc0de" + "cdcdcdcd" + "deadbeef" + "cccccccc",16)) // little endian
  }

  val base = 1023

  // write
  for (i <- 0 until idata.length) {
    poke(c.io.mem.en, 1)
    poke(c.io.mem.we, 1)
    poke(c.io.mem.addr, base + i)
    poke(c.io.mem.din, idata(i))
    step(1)
  }
  poke(c.io.mem.we, 0)

  // exec
  poke(c.io.start, 1)
  poke(c.io.base, base)
  poke(c.io.len, idata.length)
  step(1)
  poke(c.io.start, 0)

  // pull
  for (i <- 0 until odata.length) {
    poke(c.io.out.ready, 1)
    if (peek(c.io.out.valid) == 0) {
      while (peek(c.io.out.valid) == 0) {
        poke(c.io.out.ready, 1)
        step(1)
      }
    }
    expect(c.io.out.bits, odata(i))
    step(1)
  }
}

object MemTest extends App {
  //iotesters.Driver.execute(args, () => new SinglePortMem(32, 10000)) { c => new SinglePortMemUnitTester(c) }
  //iotesters.Driver.execute(args, () => new SimpleDualPortMem(32, 10000)) { c => new SimpleDualPortMemUnitTester(c) }

  iotesters.Driver.execute(args, () => new SinglePortSram(10000, 32)) { c => new SinglePortSramUnitTester(c) }
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"),
    () => new SimpleDualPortSram(10000, 32, true)) { c => new SimpleDualPortSramUnitTester(c) }

  iotesters.Driver.execute(args, () => new EnqMemWrapper(32, 32, 10000)) { c => new EnqMemUnitTester(c) }
  iotesters.Driver.execute(args, () => new EnqMemWrapper(128, 32, 10000)) { c => new EnqMemUnitTester(c) }
  iotesters.Driver.execute(args, () => new EnqMemWrapper(8, 32, 10000)) { c => new EnqMemUnitTester(c) }

  iotesters.Driver.execute(args, () =>
    new EnqAddrDeqMemWrapper(new MemReadIO(10000, 32))) { c => new EnqAddrDeqMemUnitTester(c) }

  iotesters.Driver.execute(args, () => new DeqMemWrapper(32, 32, 10000)) { c => new DeqMemUnitTester(c) }
  iotesters.Driver.execute(args, () => new DeqMemWrapper(8, 32, 10000)) { c => new DeqMemUnitTester(c) }
  iotesters.Driver.execute(args, () => new DeqMemWrapper(128, 32, 10000)) { c => new DeqMemUnitTester(c) }
}

object SinglePortSramVerilog extends App {
  chisel3.Driver.execute(args, () => new SinglePortSram(10000, 32))
}
object SimpleDualPortSramVerilog extends App {
  chisel3.Driver.execute(args, () => new SimpleDualPortSram(10000, 32))
}



