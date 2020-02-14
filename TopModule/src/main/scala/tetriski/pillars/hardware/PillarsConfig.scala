package tetriski.pillars.hardware
import chisel3._

object PillarsConfig {
  val LOG_II_UPPER_BOUND = 4
  val II_UPPER_BOUND =  Math.pow(2, LOG_II_UPPER_BOUND).toInt

  val MEM_DEPTH = 256
  val MEM_IN_WIDTH = 32
  val MEM_OUT_WIDTH = 32

  val LOG_SCHEDULE_SIZE = 5.W

  val ALU_ADD = 0.U(4.W)
  val ALU_SUB = 1.U(4.W)
  val ALU_AND = 2.U(4.W)
  val ALU_OR = 3.U(4.W)
  val ALU_XOR = 4.U(4.W)
  val ALU_MUL = 5.U(4.W)
  val ALU_SLT = 6.U(4.W)
  val ALU_SLL = 7.U(4.W)
  val ALU_SLTU = 8.U(4.W)
  val ALU_SRL = 9.U(4.W)
  val ALU_SRA = 10.U(4.W)
  val ALU_COPY_A = 11.U(4.W)
  val ALU_COPY_B = 12.U(4.W)

  val ALU_FUN_NUM = 13
}
