package tetriski.pillars.util

import chisel3._
import chisel3.util._

class Handshake[T<:Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(EnqIO(gen))
    val deq = Flipped(DeqIO(gen))
  })
  io.enq <> io.deq
}
object Handshake {
  def apply[T<:Data](gen: T): Handshake[T] = {
    val self = Module(new Handshake(gen))
    val io = self.io
    io.enq.noenq()
    io.deq.nodeq()
    return self
  }
}

@deprecated("use DeqBus or EnqBus instead","")
object DecoupledBus {

  def apply(bus: AxiMasterReadIO, enq: DecoupledIO[UInt]) = new {

    val len_width = bus.read_addr.len.getWidth
    val data_width = bus.read_data.bits.getWidth

    protected val s_start :: s_read :: s_idle :: Nil = Enum(3)
    protected val state = RegInit(s_idle)

    def idle() = (state === s_idle)

    def start() {
      state := s_start
    }

    def read(base: UInt, len: UInt) {

      val counter = RegInit(0.U(len_width))
      when (state === s_start) {

        bus.read_addr.setLen(len)
        bus.read_addr.setSize(data_width/8)
        bus.read_addr.enq(base)

        when (bus.read_addr.fire()) {

          state := s_read
          counter := len-1.U
          assert(bus.read_addr.len === len-1.U)
        }
      }

      when (state === s_read) {
        connect()
        when (bus.read_data.fire()) {
          counter := counter - 1.U
          when (bus.read_data.last) {
            state := s_idle
            assert(counter === 0.U)
          }
        }
      }

      when (state === s_idle) {
      }
    }
    def connect() {
      bus.read_data.ready <> enq.ready
      bus.read_data.valid <> enq.valid
      bus.read_data.bits <> enq.bits
    }
  }



  def apply(deq: DecoupledIO[UInt], bus: AxiMasterWriteIO) = new {

    val len_width = bus.write_addr.len.getWidth
    val data_width = bus.write_data.bits.getWidth

    protected val s_start :: s_write :: s_resp :: s_idle :: Nil = Enum(4)
    protected val state = RegInit(s_start)

    def idle() = (state === s_idle)

    def start() { state := s_start }

    def write(base: UInt, len: UInt) {
      val counter = RegInit(0.U(len_width.W))

      when (state === s_start) {
        bus.write_addr.setLen(len)
        bus.write_addr.setSize(data_width/8)
        bus.write_addr.enq(base)
        when (bus.write_addr.fire()) {
          state := s_write
          counter := bus.write_addr.len
          assert(bus.write_addr.len === len-1.U)
        }
      }

      when (state === s_write) {
        connect()
        when (bus.write_data.fire()) {
          when (counter === 0.U) {
            state := s_resp
            bus.write_data.last := true.B
          } .otherwise {
            counter := counter - 1.U
          }
        }
      }

      when (state === s_resp) {
        val resp = bus.write_resp.deq()
        when (bus.write_resp.fire()) {
          state := s_idle
          assert(counter === 0.U)
          assert(resp === Axi4Config.resp.OKAY)
        }
      }

      when (state === idle) {
      }
    }

    def connect() {
      deq.ready <> bus.write_data.ready
      deq.valid <> bus.write_data.valid
      deq.bits <> bus.write_data.bits
      bus.write_resp.ready := true.B
    }
  }
}

@deprecated("use DeqMem and EnqBus instead","")
object DecoupledBusFromMemByWrite { // unbuffered

  def apply(mem_io: MemReadIO, bus: AxiMasterWriteIO) = new {

    val len_width = bus.write_addr.len.getWidth
    val data_width = bus.write_data.bits.getWidth

    protected val s_start :: s_write :: s_resp :: s_idle :: Nil = Enum(4)
    protected val state = RegInit(s_start)

    //*************************************************************************************
    def show_decoupstate(): UInt = {
      state
    }

    //*************************************************************************************

    def idle() = (state === s_idle)

    def start() { state := s_start }

    def write(base: UInt, len: UInt, mem_base:UInt) {
      val counter = RegInit(0.U(len_width.W))

      when (state === s_start) {
        bus.write_addr.setLen(len)
        bus.write_addr.setSize(data_width/8)
        bus.write_addr.enq(base)
        when (bus.write_addr.fire()) {
          state := s_write
        }
      }

      when (state === s_write) {
        mem_io.en := true.B
        mem_io.addr := mem_base + counter
        bus.write_data.enq(mem_io.dout)
        when (bus.write_data.fire()) {
          when (counter === len-1.U) {
            state := s_resp
            bus.write_data.last := true.B
          } .otherwise {
            counter := counter + 1.U
          }
        }
      }

      when (state === s_resp) {
        val resp = bus.write_resp.deq()
        when (bus.write_resp.fire()) {
          state := s_idle
          assert(counter === len-1.U)
          assert(resp === Axi4Config.resp.OKAY)
        }
      }

      when (state === idle) {

      }
    }

  }
}

//
//object DecoupledBusFromMem { // unbuffered
//
//  def apply(mem_io: MemReadIO, bus: AxiMasterIO) = new {
//
//    val len_width = bus.write_addr.len.getWidth
//    val data_width = bus.write_data.bits.getWidth
//
//    protected val s_start :: s_write :: s_resp :: s_idle :: Nil = Enum(4)
//    protected val state = RegInit(s_start)
//
//    //*************************************************************************************
//    def show_decoupstate(): UInt = {
//      state
//    }
//
//    //*************************************************************************************
//
//    def idle() = (state === s_idle)
//
//    def start() { state := s_start }
//
//    def write(base: UInt, len: UInt, mem_base:UInt) {
//      val counter = RegInit(0.U(len_width.W))
//
//      when (state === s_start) {
//        bus.write_addr.len := Axi4Config.len(len)
//        bus.write_addr.size := Axi4Config.size(data_width/8)
//        bus.write_addr.enq(base)
//        when (bus.write_addr.fire()) {
//          state := s_write
//        }
//      }
//
//      when (state === s_write) {
//        mem_io.en := true.B
//        mem_io.addr := mem_base + counter
//        bus.write_data.enq(mem_io.dout)
//        when (bus.write_data.fire()) {
//          when (counter === Axi4Config.len(len)) {
//            state := s_resp
//            bus.write_data.last := true.B
//          } .otherwise {
//            counter := counter + 1.U
//          }
//        }
//      }
//
//      when (state === s_resp) {
//        val resp = bus.write_resp.deq()
//        when (bus.write_resp.fire()) {
//          state := s_idle
//          assert(counter === Axi4Config.len(len))
//          assert(resp === Axi4Config.resp.OKAY)
//        }
//      }
//
//      when (state === idle) {
//      }
//    }
//
//  }
//}

@deprecated("use DeqBus and EnqMem instead","")
object DecoupledBusToMem { // unbuffered

  def apply(bus: TraitAxiMasterReadIO, mem: MemIO) = new {

    val len_width = bus.read_addr.len.getWidth //8
    val data_width = bus.read_data.bits.getWidth //512

    protected val s_start :: s_read :: s_idle :: Nil = Enum(3)
    protected val state = RegInit(s_idle)

    def idle() = (state === s_idle)

    //********************************************************************
    def show_bustate(): UInt = {
      state
    }

    val show_count = WireInit(0.U(len_width.W))
    val reg_tmp = WireInit(0.U(len_width.W))

    def show_counter(): UInt = {
      show_count
    }

    val tmp_result = RegInit(0.U(64.W))

    val wire_readaddrlen = WireInit(0.U(64.W))
    def show_readaddrlen(): UInt = {
      reg_tmp
    }

    val wire_innerlen = WireInit(0.U(64.W))
    def vshow_innerlen(): UInt = {
      wire_innerlen
    }

    //********************************************************************

    def start() {
      state := s_start
    }

    def read(base: UInt, len: UInt, mem_base: UInt) {

      val counter = RegInit(0.U(len_width.W))
      reg_tmp := counter
      when (state === s_start) {
        bus.read_addr.setLen(len)
        bus.read_addr.setSize(data_width/8)
        bus.read_addr.enq(base)

        when(bus.read_addr.fire()) {
          state := s_read
        }
      }

      when (state === s_read) {
        bus.read_data.deq()
        when (bus.read_data.fire()) {

          mem.enable()
          mem.addr := mem_base + counter
          mem.din := bus.read_data.bits
          counter := counter + 1.U

          when (bus.read_data.last) {
            state := s_idle
            counter := 0.U
          }
        }
      }

      when (state === s_idle) {

      }

    }
  }
}
