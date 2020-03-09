package tetriski.pillars.hardware
import chisel3._

object PillarsConfig {
  //for TopModuleWrapper generation
//  val LOG_II_UPPER_BOUND = 1
  //for verilator test
  val LOG_II_UPPER_BOUND = 2
  val II_UPPER_BOUND =  Math.pow(2, LOG_II_UPPER_BOUND).toInt

  val MEM_DEPTH = 256
  val MEM_IN_WIDTH = 32
  val MEM_OUT_WIDTH = 32

  //for TopModuleWrapper generation
//  val LOG_SCHEDULE_SIZE = 3
  //for verilator test
  val LOG_SCHEDULE_SIZE = 5

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
