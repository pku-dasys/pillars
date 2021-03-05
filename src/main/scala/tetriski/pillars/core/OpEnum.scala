package tetriski.pillars.core

import tetriski.pillars.core.OpEnum.{OpEnum, Value}

/** A object enumerating opcodes.
 * Not all opcodes can be performed by modules in our current hardware libs.
 */
object OpEnum extends Enumeration {
  type OpEnum = Value
  val NOP = Value("0")
  val SEXT = Value("1")
  val ZEXT = Value("2")
  val TRUNC = Value("3")
  val INPUT = Value("4")
  val OUTPUT = Value("5")
  val PHI = Value("6")
  val CONST = Value("7")
  val ADD = Value("8")
  val SUB = Value("9")
  val MUL = Value("10")
  val DIV = Value("11")
  val AND = Value("12")
  val OR = Value("13")
  val XOR = Value("14")
  val SHLL = Value("15")
  val LS = Value("15")
  val SHRA = Value("16")
  val CLT = Value("16")
  val SHRL = Value("17")
  val RS = Value("17")
  val LOAD = Value("18")
  val STORE = Value("19")
  val GEP = Value("20")
  val ICMP = Value("21")
  val CMP = Value("21")
  val SHR = Value("22")
  val MOVC = Value("22")
  val SLT = Value("23")
  val CGT = Value("24")
  val SLTU = Value("25")
  val SELECT = Value("25")
  val SHLA = Value("26")
  val CMERGE = Value("26")
  val LOADH = Value("27")
  val STOREH = Value("28")
  val LOADB = Value("29")
  val STOREB = Value("30")
}



/** A object translating opcodes in elements into the set of configuration of modules.
 */
object OpcodeTranslator {

  val aluOpcodeList = List(OpEnum.ADD,
    OpEnum.SUB,
    OpEnum.AND,
    OpEnum.OR,
    OpEnum.XOR,
    OpEnum.MUL,
    OpEnum.SLT,
    OpEnum.LS,
    OpEnum.CLT,
    OpEnum.RS,
    OpEnum.MOVC,
    OpEnum.DIV,
    OpEnum.CMP,
    OpEnum.CGT,
    OpEnum.SELECT,
    OpEnum.CMERGE)
//    OpEnum.SLT,
//    OpEnum.SHLL,
//    OpEnum.SLTU,
//    OpEnum.SHRL,
//    OpEnum.SHRA,
//    OpEnum.DIV)

  /** Get the subset of optional operations of an ALU with a list of opcodes.
   *
   * @param opEnums   a list of opcodes
   * @param supBypass a parameter indicating whether the ALU should support bypass
   * @return the subset of optional operations of an ALU as Int
   */
  def getAluFunSelect(opEnums: List[OpEnum], supBypass: Boolean): Int = {
    var ret = 0
    opEnums.foreach(op => ret = ret | (1 << getModuleConfig(op)))
    if (supBypass) {
      ret = ret | (1 << 12)
      ret = ret | (1 << 13)
    }
    ret
  }

  /** Get the configuration of a module with a opcode.
   *
   * @param opEnum a opcode
   * @return the configuration
   */
  def getModuleConfig(opEnum: OpEnum): Int = {
    val ret = opEnum match {
      //ALU
      case OpEnum.ADD => 0
      case OpEnum.SUB => 1
      case OpEnum.AND => 2
      case OpEnum.OR => 3
      case OpEnum.XOR => 4
      case OpEnum.MUL => 5
      case OpEnum.SLT => 6
      case OpEnum.LS => 7
      case OpEnum.CLT => 8
      case OpEnum.RS => 9
      case OpEnum.MOVC => 10
      case OpEnum.SLT => 6
      case OpEnum.SHLL => 7
      case OpEnum.SLTU => 8
      case OpEnum.SHRL => 9
      case OpEnum.SHRA => 10
      case OpEnum.DIV => 11
      case OpEnum.CMP => 12
      case OpEnum.CGT => 13
      case OpEnum.SELECT => 14
      case OpEnum.CMERGE => 15


      //LSU
      case OpEnum.LOAD => 0
      case OpEnum.STORE => 1
      case OpEnum.LOADH => 2
      case OpEnum.STOREH => 3
      case OpEnum.LOADB => 4
      case OpEnum.STOREB => 5

      case OpEnum.CONST => 0
    }
    ret
  }

}


