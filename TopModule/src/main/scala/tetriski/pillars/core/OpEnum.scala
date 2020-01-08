package tetriski.pillars.core

import tetriski.pillars.core.OpEnum.OpEnum

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
  val SHL = Value("15")
  val SHRA = Value("16")
  val SHRL = Value("17")
  val LOAD = Value("18")
  val STORE = Value("19")
  val GEP = Value("20")
  val ICMP = Value("21")
  val SHR = Value("22")
  val SLT = Value("23")
  val SLTU = Value("24")
  val SHLL = Value("25")

}

  object OpcodeTranslator {
    def getAluFunSelect(opEnums : List[OpEnum], supBypass : Boolean): Int ={
      var ret = 0
      opEnums.foreach(op => ret = ret | (1 << getModuleOpcode(op)))
      if(supBypass){
        ret = ret | (1 << 11)
        ret = ret | (1 << 12)
      }
      ret
    }

    def getModuleOpcode(opEnum : OpEnum):Int ={
      val ret = opEnum match {
          //ALU
        case OpEnum.ADD => 0
        case OpEnum.SUB => 1
        case OpEnum.AND => 2
        case OpEnum.OR => 3
        case OpEnum.XOR => 4
        case OpEnum.MUL => 5
        case OpEnum.SLT => 6
        case OpEnum.SHLL => 7
        case OpEnum.SLTU => 8
        case OpEnum.SHRL => 9
        case OpEnum.SHRA => 10

          //LSU
        case OpEnum.LOAD => 0
        case OpEnum.STORE => 1

        case OpEnum.CONST => 0
      }
      ret
    }

  }


