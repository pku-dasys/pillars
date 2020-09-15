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
   *
   */
  val LOG_SCHEDULE_SIZE = 5

  /** The limitation of the skew.
   * 2 for TopModuleWrapper generation and 4 for verilator test of examples.
   *
   * @example If LOG_SCHEDULE_SIZE = 4, the compliant skew of the CGRA architecture is 0 ~ 16.
   * @example If LOG_SCHEDULE_SIZE = -1, the Synchronizers will not be employed in the architecture.
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

  val SKEW_WIDTH = if (USE_RELATIVE_SKEW) {
    LOG_SKEW_LENGTH + 1
  } else {
    LOG_SKEW_LENGTH * 2
  }

  val ALU_ADD = 0.U(4.W)
  val ALU_SUB = 1.U(4.W)
  val ALU_AND = 2.U(4.W)
  val ALU_OR = 3.U(4.W)
  val ALU_XOR = 4.U(4.W)
  val ALU_MUL = 5.U(4.W)
  val ALU_SLT = 6.U(4.W)
  val ALU_SHLL = 7.U(4.W)
  val ALU_SLTU = 8.U(4.W)
  val ALU_SHRL = 9.U(4.W)
  val ALU_SHRA = 10.U(4.W)
  val ALU_DIV = 11.U(4.W)
  val ALU_COPY_A = 12.U(4.W)
  val ALU_COPY_B = 13.U(4.W)


  val ALU_FUN_NUM = 14
}
