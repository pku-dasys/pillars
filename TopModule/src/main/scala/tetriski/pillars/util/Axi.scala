package tetriski.pillars.util

import chisel3._
import chisel3.util._

object XilinxAxiConfig {
  val M_AXI_ADDR_WIDTH = 64
  val S_AXI_ADDR_WIDTH = 7
}

import XilinxAxiConfig._ // TO-DO: fix this quick-and-dirty workaround

class AxiMasterIO(val addr_width: Int, val data_width: Int) extends TraitAxiMasterIO {
  val read_addr = new AxiAddressIO(UInt(addr_width.W))
  val read_data = Flipped(new AxiReadDataIO(UInt(data_width.W)))
  val write_addr = new AxiAddressIO(UInt(addr_width.W))
  val write_data = new AxiWriteDataIO(UInt(data_width.W))
  val write_resp = Flipped(new AxiWriteResponseIO(UInt(2.W)))
}

class AxiMasterReadIO(val addr_width: Int, val data_width: Int) extends TraitAxiMasterReadIO {
  val read_addr = new AxiAddressIO(UInt(addr_width.W))
  val read_data = Flipped(new AxiReadDataIO(UInt(data_width.W)))

  def this(parent: AxiMasterIO) {
    this(parent.addr_width, parent.data_width)
  }

  override def cloneType: this.type = new AxiMasterReadIO(addr_width, data_width).asInstanceOf[this.type]
}

class AxiMasterWriteIO(val addr_width: Int, val data_width: Int) extends TraitAxiMasterWriteIO {
  val write_addr = new AxiAddressIO(UInt(addr_width.W))
  val write_data = new AxiWriteDataIO(UInt(data_width.W))
  val write_resp = Flipped(new AxiWriteResponseIO(UInt(2.W)))

  def this(parent: AxiMasterIO) {
    this(parent.addr_width, parent.data_width)
  }
}

trait TraitAxiMasterIO extends Bundle with TraitAxiMasterWriteIO with TraitAxiMasterReadIO {
  override def init() {
    super[TraitAxiMasterReadIO].init()
    super[TraitAxiMasterWriteIO].init()
  }
  override def s_init() {
    super[TraitAxiMasterReadIO].s_init()
    super[TraitAxiMasterWriteIO].s_init()
  }

  override def <>(that: TraitAxiMasterIO) {
    super[TraitAxiMasterReadIO].<>(that)
    super[TraitAxiMasterWriteIO].<>(that)
  }
}

trait TraitAxiMasterReadIO extends Bundle {
  val addr_width: Int
  val data_width: Int
  val read_addr: AxiAddressIO
  val read_data: AxiReadDataIO

  def init() {
    read_addr.m_init()
    read_data.m_init()
  }

  def s_init() {
    read_addr.s_init()
    read_data.s_init()
  }

  def <>(that: TraitAxiMasterIO) {
    this.read_addr <> that.read_addr
    this.read_data <> that.read_data
  }
  def <>(that: TraitAxiMasterReadIO) {
    this.read_addr <> that.read_addr
    this.read_data <> that.read_data
  }
}

trait TraitAxiMasterWriteIO extends Bundle {
  val addr_width: Int
  val data_width: Int
  val write_addr: AxiAddressIO
  val write_data: AxiWriteDataIO
  val write_resp: AxiWriteResponseIO

  def init() {
    write_addr.m_init()
    write_data.m_init()
    write_resp.m_init()
  }

  def s_init() {
    write_addr.s_init()
    write_data.s_init()
    write_resp.s_init()
  }

  def <>(that: TraitAxiMasterIO) {
    this.write_addr <> that.write_addr
    this.write_data <> that.write_data
    this.write_resp <> that.write_resp
  }
  def <>(that: TraitAxiMasterWriteIO) {
    this.write_addr <> that.write_addr
    this.write_data <> that.write_data
    this.write_resp <> that.write_resp
  }
}

class AxiLiteSlaveIO(AXI_DATA_WIDTH: Int) extends Bundle {
  val read_addr = Flipped(new AxiLiteAddressIO(UInt(S_AXI_ADDR_WIDTH.W)))
  val read_data = new AxiLiteReadDataIO(UInt(AXI_DATA_WIDTH.W))
  val write_addr = Flipped(new AxiLiteAddressIO(UInt(S_AXI_ADDR_WIDTH.W)))
  val write_data = Flipped(new AxiLiteWriteDataIO(UInt(AXI_DATA_WIDTH.W)))
  val write_resp = new AxiLiteWriteResponseIO(UInt(2.W))

  def init() {
    read_addr.s_init()
    read_data.s_init()
    write_addr.s_init()
    write_data.s_init()
    write_resp.s_init()
  }
}

// Encoding in ARM IHI 0022F.b (ID122117) //////////////////////////////////////
//   AMBA® AXI and ACE Protocol Specification (AXI3, AXI4, AXI5, ACE and ACE5)
//   at https://developer.arm.com/docs/ihi0022/fb
object Axi4Config {
  object len {
    // A3.4.1 Address structure: Burst length
    val width = 8
    def apply(length: UInt): UInt = {
      val code = WireInit(0.U(width.W))
      code := length - 1.U
      return code
    }
  }
  object size {
    // Table A3-2 Burst size encoding
    val width = 3
    def apply(bytes: Int): UInt = bytes match {
      case 1 => "b000".U(3.W)
      case 2 => "b001".U(3.W)
      case 4 => "b010".U(3.W)
      case 8 => "b011".U(3.W)
      case 16 => "b100".U(3.W)
      case 32 => "b101".U(3.W)
      case 64 => "b110".U(3.W)
      case 128 => "b111".U(3.W)
    }
  }
  object burst {
    // Table A3-3 Burst type encoding
    val width = 2
    val FIXED = "b00".U(2.W)
    val INCR = "b01".U(2.W)
    val WRAP = "b10".U(2.W)
    val RESERVED = "b11".U(2.W)
  }
  object resp {
    // Table A3-4 RRESP and BRESP encoding
    val width = 2
    val OKAY = "b00".U(2.W)
    val EXOKAY = "b01".U(2.W)
    val SLVERR = "b10".U(2.W)
    val DECERR = "b11".U(2.W)
  }
  trait CacheAx {
    // Table A4-5 Memory type encoding
    val width = 4
    def DEVICE_NON_BUFFERABLE = "b0000".U(4.W)
    def DEVICE_BUFFERABLE = "b0001".U(4.W)
    def NORMAL_NON_CACHEABLE_NON_BUFFERABLE = "b0010".U(4.W)
    def NORMAL_NON_CACHEABLE_BUFFERABLE = "b0011".U(4.W)
  }
  object cache extends CacheAx {
  }
  object cache_ar extends CacheAx {
    val WRITE_THROUGH_NO_ALLOCATE = "b1010".U(4.W)
    val WRITE_THROUGH_READ_ALLOCATE = "b1110".U(4.W)
    val WRITE_THROUGH_WRITE_ALLOCATE = "b1010".U(4.W)
    val WRITE_THROUGH_READ_AND_WRITE_ALLOCATE = "b1110".U(4.W)
    val WRITE_BACK_NO_ALLOCATE = "b1011".U(4.W)
    val WRITE_BACK_READ_ALLOCATE = "b1111".U(4.W)
    val WRITE_BACK_WRITE_ALLOCATE = "b1011".U(4.W)
    val WRITE_BACK_READ_AND_WRITE_ALLOCATE = "b1111".U(4.W)
  }
  object cache_aw extends CacheAx {
    val WRITE_THROUGH_NO_ALLOCATE = "b0110".U(4.W)
    val WRITE_THROUGH_READ_ALLOCATE = "b0110".U(4.W)
    val WRITE_THROUGH_WRITE_ALLOCATE = "b1110".U(4.W)
    val WRITE_THROUGH_READ_AND_WRITE_ALLOCATE = "b1110".U(4.W)
    val WRITE_BACK_NO_ALLOCATE = "b0111".U(4.W)
    val WRITE_BACK_READ_ALLOCATE = "b0111".U(4.W)
    val WRITE_BACK_WRITE_ALLOCATE = "b1111".U(4.W)
    val WRITE_BACK_READ_AND_WRITE_ALLOCATE = "b1111".U(4.W)
  }
  object prot {
    val width = 3
    // Table A4-6 Protection encoding
    // bit mask for prot[0]
    val UNPRIVILEGED = "b0".U(3.W)
    val PRIVILEGED = "b1".U(3.W)
    // bit mask for prot[1]
    val SECURE = "b00".U(3.W)
    val NONSECURE = "b10".U(3.W)
    // bit mask for prot[2]
    val DATA = "b000".U(3.W)
    val INSTRUCTION = "b100".U(3.W)
  }
  object lock {
    // Table A7-2 AXI4 atomic access encoding
    val width = 1
    val NORMAL = false.B
    val EXECLUSIVE = true.B

    object axi3 {
      // Table A7-1 AXI3 atomic access encoding
      val width = 2
      val NORMAL = "b00".U(2.W)
      val EXECLUSIVE = "b01".U(2.W)
      val LOCKDED = "b10".U(2.W)
      val RESERVED = "b11".U(2.W)
    }
  }
}

// Table B1-1 shows the required signals on an AXI4-Lite interface. ////////////

trait TraitAxiLiteAddressIO extends DecoupledIO[UInt] {
  // directions in the perspective of master (address source)
  def addr = bits
  val prot = Output(UInt(Axi4Config.prot.width.W))

  def m_init() {
    this.noenq()
    prot := {import Axi4Config.prot._; UNPRIVILEGED|SECURE|DATA}
  }
  def s_init() {
    this.nodeq()
  }
}

class AxiLiteAddressIO(gen: UInt) extends DecoupledIO(gen) with TraitAxiLiteAddressIO {
  override def cloneType: this.type = new AxiLiteAddressIO(gen).asInstanceOf[this.type]
}

trait TraitAxiLiteReadDataIO extends DecoupledIO[UInt] {
  // directions in the perspective of slave (read data source)
  def data = bits
  val resp = Output(UInt(Axi4Config.resp.width.W))

  def m_init() {
    this.nodeq()
  }
  def s_init() {
    this.noenq()
    resp := DontCare
  }
}
class AxiLiteReadDataIO(gen: UInt) extends DecoupledIO(gen) with TraitAxiLiteReadDataIO {
  override def cloneType: this.type = new AxiLiteReadDataIO(gen).asInstanceOf[this.type]
}

trait TraitAxiLiteWriteDataIO extends DecoupledIO[UInt] {
  // directions in the perspective of master (write data source)
  private val strb_width = bits.getWidth/8
  def data = bits
  val strb = Output(UInt(strb_width.W))

  def m_init() {
    this.noenq()
    strb := ~0.U(strb_width.W) // bit-wise not
  }
  def s_init() {
    this.nodeq()
  }
}
class AxiLiteWriteDataIO(gen: UInt) extends DecoupledIO(gen) with TraitAxiLiteWriteDataIO {
  override def cloneType: this.type = new AxiLiteWriteDataIO(gen).asInstanceOf[this.type]
}

trait TraitAxiLiteWriteResponseIO extends DecoupledIO[UInt] {
  // directions in the perspective of slave (write response source)
  def resp = bits

  def m_init() {
    this.nodeq()
  }
  def s_init() {
    this.noenq()
  }
}
class AxiLiteWriteResponseIO(gen: UInt) extends DecoupledIO(gen) with TraitAxiLiteWriteResponseIO {
  override def cloneType: this.type = new AxiLiteWriteResponseIO(gen).asInstanceOf[this.type]
}

// Implementations of the AXI IOs //////////////////////////////////////////////

trait AxiIdentifier {
  // directions in the perspective of message source
  val id = Output(UInt(1.W))
  val user = Output(UInt(1.W))

  def init() {
    id := 0.U
    user := 0.U
  }
}

class AxiAddressIO(gen: UInt) extends DecoupledIO(gen) with AxiIdentifier with TraitAxiLiteAddressIO {
  // Table A2-2 Write address channel signals
  // Table A2-5 Read address channel signals
  val len = Output(UInt(Axi4Config.len.width.W)) // === #transfers minus one
  val size = Output(UInt(Axi4Config.size.width.W)) // === log(bytes) per transfer
  val burst = Output(UInt(Axi4Config.burst.width.W))
  val lock = Output(UInt(Axi4Config.lock.axi3.width.W))
  val cache = Output(UInt(Axi4Config.cache.width.W))

  val qos = Output(UInt(4.W))
  val region = Output(UInt(4.W))

  override def m_init() {
    super[AxiIdentifier].init()
    super[TraitAxiLiteAddressIO].m_init()

    len := 0.U
    size := Axi4Config.size(2)
    burst := Axi4Config.burst.INCR
    lock := Axi4Config.lock.axi3.NORMAL
    cache := Axi4Config.cache.NORMAL_NON_CACHEABLE_BUFFERABLE

    qos := 0.U
    region := 0.U
  }

  // master mode
  def setLen(len: UInt) {
    this.len := len - 1.U
  }
  def setSize(size: Int) {
    this.size := Axi4Config.size(size)
  }

  // slave mode
  def getLen() = (len + 1.U((Axi4Config.len.width+1).W))
  def getSize() = (1.U << size)

  override def cloneType: this.type = new AxiAddressIO(gen).asInstanceOf[this.type]
}

class AxiReadDataIO(gen: UInt) extends DecoupledIO(gen) with AxiIdentifier with TraitAxiLiteReadDataIO {
  // Table A2-6 Read data channel signals
  val last = Output(Bool())

  override def s_init() {
    super[AxiIdentifier].init()
    super[TraitAxiLiteReadDataIO].s_init()
    last := false.B
  }
  override def cloneType: this.type = new AxiReadDataIO(gen).asInstanceOf[this.type]
}

class AxiWriteDataIO(gen: UInt) extends DecoupledIO(gen) with AxiIdentifier with TraitAxiLiteWriteDataIO {
  // Table A2-3 Write data channel signals
  val last = Output(Bool())

  override def m_init() {
    super[AxiIdentifier].init()
    super[TraitAxiLiteWriteDataIO].m_init()
    last := false.B
  }
  override def cloneType: this.type = new AxiWriteDataIO(gen).asInstanceOf[this.type]
}

class AxiWriteResponseIO(gen: UInt) extends DecoupledIO(gen) with AxiIdentifier with TraitAxiLiteWriteResponseIO {
  // Table A2-4 Write response channel signals

  override def s_init() {
    super[AxiIdentifier].init()
    super[TraitAxiLiteWriteResponseIO].s_init()
  }
  override def cloneType: this.type = new AxiWriteResponseIO(gen).asInstanceOf[this.type]
}

