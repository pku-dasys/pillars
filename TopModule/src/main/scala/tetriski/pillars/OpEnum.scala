package tetriski.pillars

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


}
