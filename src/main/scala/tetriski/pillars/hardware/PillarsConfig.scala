package tetriski.pillars.hardware

import chisel3._
import chisel3.util.{Log2, log2Up}

/** Hardware config in Pillars including the depth of RAM in LSU,
 * limitation of II and the schedule length.
 */
object PillarsConfig {
  /** The limitation of the initiation interval.
   * 1 for TopModuleWrapper generation and 2 for verilator test of examples.
   *
   * @example If LOG_II_UPPER_BOUND = 2, the compliant II of the CGRA architecture is 1 ~ 3.
   *
   */
  var LOG_II_UPPER_BOUND = 2
  val II_UPPER_BOUND = Math.pow(2, LOG_II_UPPER_BOUND).toInt

  /** Memory depth in Load/Store unit.
   */
  val MEM_DEPTH = 256
  var MEM_IN_WIDTH = 32
  var MEM_OUT_WIDTH = 32

  /** The limitation of the schedule length.
   * 3 for TopModuleWrapper generation and 5 for verilator test of examples.
   *
   * @example If LOG_SCHEDULE_SIZE = 5, the compliant schedule of the CGRA architecture is 0 ~ 31.
   * @example If LOG_SCHEDULE_SIZE = 0, the schedule controller will not drive modules.
   */
  var LOG_SCHEDULE_SIZE = 5

  /** The number of registers used for skewing.
   */
  var SKEW_REGISTER_NUM = 16

  /** Log2 of the limitation of the skew.
   *
   * @example If LOG_SCHEDULE_SIZE = 4, the compliant skew of the CGRA architecture is 0 ~ 15.
   * @example If LOG_SCHEDULE_SIZE = -1 and USE_RELATIVE_SKEW = true, which means SKEW_WIDTH = 0,
   *          the Synchronizers will not be employed in the architecture.
   */
  var LOG_SKEW_LENGTH = log2Up(SKEW_REGISTER_NUM + 1)

  /** A parameter indicating we use relative skew or wait skew in the schedule.
   *
   * @example If USE_RELATIVE_SKEW = true, the relative skew will be used to perform synchronization,
   *          and Synchronizers will be employed for synchronization of 2-input module.
   * @example If USE_RELATIVE_SKEW = false, the wait skew will be used to perform synchronization,
   *          and RegNextN will be employed for postponing each input date of 2-input module.
   *
   */
  var USE_RELATIVE_SKEW = true

  var SKEW_WIDTH = if (USE_RELATIVE_SKEW) {
    LOG_SKEW_LENGTH + 1
  } else {
    LOG_SKEW_LENGTH * 2
  }

  var USE_AUXILIARY_SCHEDULER = (SKEW_WIDTH != 0) || (LOG_SCHEDULE_SIZE != 0)

  var USE_TOKEN = false

  def getClassIO(w: Int) = if (USE_TOKEN) {
    new TokenIO(w)
  } else {
    UInt(w.W)
  }

  def setIO(out: Data, r: Data, valid: Bool = false.B, latch: Int = 0) = {
    if (USE_TOKEN) {
      out.asInstanceOf[TokenIO].data := r
      out.asInstanceOf[TokenIO].token := valid
      if(latch != 0){
        out.asInstanceOf[TokenIO].token := latchToken(valid, latch)
      }
    } else {
      out := r
    }
  }

  def latchToken(token: Bool, latch: Int): Data ={
    val regs = (0 until latch).map(_ => RegInit(false.B))
    regs(0) := token
    for(i <- 0 until latch - 1){
      regs(i + 1) := regs(i)
    }
    regs(latch - 1)
  }

  def getData(data: Data) = {
    if (USE_TOKEN) {
      data.asInstanceOf[TokenIO].data
    } else {
      data.asInstanceOf[UInt]
    }
  }

  def getToken(data: Data) = {
    if (USE_TOKEN) {
      data.asInstanceOf[TokenIO].token
    } else {
      true.B
    }
  }

  val ALU_ADD = 0.U(4.W)
  val ALU_SUB = 1.U(4.W)
  val ALU_AND = 2.U(4.W)
  val ALU_OR = 3.U(4.W)
  val ALU_XOR = 4.U(4.W)
  val ALU_MUL = 5.U(4.W)
  val ALU_SLT = 6.U(4.W)
  val ALU_SHLL = 7.U(4.W)
  val ALU_SHLA = 8.U(4.W)
  val ALU_SHRL = 9.U(4.W)
  val ALU_SHRA = 10.U(4.W)
  val ALU_DIV = 11.U(4.W)
  val ALU_COPY_A = 12.U(4.W)
  val ALU_COPY_B = 13.U(4.W)


  val ALU_FUN_NUM = 14

  val COUNTER_WIDTH = 8
  val CONST_WIDTH = 8
}

class TokenIO(val w: Int) extends Bundle {
  val token = Bool()
  val data = UInt(w.W)

  def :=(r: TokenIO): Unit = {
    token := r.token
    data := r.data
  }

  def <>(r: TokenIO): Unit = {
    token <> r.token
    data <> r.data
  }
}