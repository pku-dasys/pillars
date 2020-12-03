package tetriski.pillars.hardware

import chisel3._

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
  val LOG_II_UPPER_BOUND = 2
  val II_UPPER_BOUND = Math.pow(2, LOG_II_UPPER_BOUND).toInt

  /** Memory depth in Load/Store unit.
   */
  val MEM_DEPTH = 256
  val MEM_IN_WIDTH = 32
  val MEM_OUT_WIDTH = 32

  /** The limitation of the schedule length.
   * 3 for TopModuleWrapper generation and 5 for verilator test of examples.
   *
   * @example If LOG_SCHEDULE_SIZE = 5, the compliant schedule of the CGRA architecture is 0 ~ 31.
   * @example If LOG_SCHEDULE_SIZE = 0, the schedule controller will not drive modules.
   */
  var LOG_SCHEDULE_SIZE = 5

  /** The limitation of the skew.
   * 2 for TopModuleWrapper generation and 4 for verilator test of examples.
   *
   * @example If LOG_SCHEDULE_SIZE = 4, the compliant skew of the CGRA architecture is 0 ~ 16.
   * @example If LOG_SCHEDULE_SIZE = -1 and USE_RELATIVE_SKEW = true, which means SKEW_WIDTH = 0,
   *          the Synchronizers will not be employed in the architecture.
   *
   */
  val LOG_SKEW_LENGTH = 4

  /** A parameter indicating we use relative skew or wait skew in the schedule.
   *
   * @example If USE_RELATIVE_SKEW = true, the relative skew will be used to perform synchronization,
   *          and Synchronizers will be employed for synchronization of 2-input module.
   * @example If USE_RELATIVE_SKEW = false, the wait skew will be used to perform synchronization,
   *          and RegNextN will be employed for postponing each input date of 2-input module.
   *
   */
  val USE_RELATIVE_SKEW = true

  var SKEW_WIDTH = if (USE_RELATIVE_SKEW) {
    LOG_SKEW_LENGTH + 1
  } else {
    LOG_SKEW_LENGTH * 2
  }

  val USE_AUXILIARY_SCHEDULER = (SKEW_WIDTH != 0) || (LOG_SCHEDULE_SIZE != 0)

  val ALU_ADD = 0.U(4.W)
  val ALU_SUB = 1.U(4.W)
  val ALU_AND = 2.U(4.W)
  val ALU_OR = 3.U(4.W)
  val ALU_XOR = 4.U(4.W)
  val ALU_MUL = 5.U(4.W)
  val ALU_SLT = 6.U(4.W)
  val ALU_SHLL = 7.U(4.W)
  val ALU_LS = 7.U(4.W)
  val ALU_SLTU = 8.U(4.W)
  val ALU_CLT = 8.U(4.W)
  val ALU_SHRL = 9.U(4.W)
  val ALU_RS = 9.U(4.W)
  val ALU_SHRA = 10.U(4.W)
  val ALU_MOVC = 10.U(4.W)
  val ALU_DIV = 11.U(4.W)
  val ALU_COPY_A = 12.U(4.W)
  val ALU_COPY_B = 13.U(4.W)
  val ALU_CMP = 12.U(4.W)
  val ALU_CGT = 13.U(4.W)
  val ALU_SELECT = 14.U(4.W)
  val ALU_CMERGE = 15.U(4.W)


//  val ALU_FUN_NUM = 14
  val ALU_FUN_NUM = 16

  val LSU_LOAD = 0.U(3.W)
  val LSU_STORE = 1.U(3.W)
  val LSU_LOADH = 2.U(3.W)
  val LSU_STOREH = 3.U(3.W)
  val LSU_LOADB = 4.U(3.W)
  val LSU_STOREB = 5.U(3.W)

  val USE_PREDICATE = true
  val LSU_WITH_6_OP = true
}
