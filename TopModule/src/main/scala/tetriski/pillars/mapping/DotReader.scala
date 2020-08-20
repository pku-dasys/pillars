package tetriski.pillars.mapping

import tetriski.pillars.core.OpEnum

/** Class to load DFG from a DOT file.
 *
 * It uses {@code loadDot} to read Dot file.
 */
object DotReader {
  /** Generate opcode of an operation.
   *
   * @param op the name of the operation
   */
  def genOpCode(op: String): Int = {
    if (op == "nop") return 0
    if (op == "sext") return 1
    if (op == "zext") return 2
    if (op == "trunc") return 3
    if (op == "input") return 4
    if (op == "output") return 5
    if (op == "phi") return 6
    if (op == "const") return 7
    if (op == "add") return 8
    if (op == "sub") return 9
    if (op == "mul") return 10
    if (op == "div") return 11
    if (op == "and") return 12
    if (op == "or") return 13
    if (op == "xor") return 14
    if (op == "shll") return 15
    if (op == "shra") return 16
    if (op == "shrl") return 17
    if (op == "load") return 18
    if (op == "store") return 19
    if (op == "gep") return 20
    if (op == "icmp") return 21
    if (op == "shr") return 22
    if (op == "slt") return 23
    if (op == "sltu") return 24
    if (op == "shla") return 25
    -1
  }

  /** Load DFG(IR) from a Dot file.
   *
   * @param filename   the name of DOT file
   * @param targetedII the targeted II
   */
  def loadDot(filename: String, targetedII: Int = 1): DFG = {
    import scala.io.Source

    val buffer = Source.fromFile(filename)
    val file = buffer.getLines().toArray
    val name = file(0).substring(8, file(0).length - 2)
    val dfg = new DFG(name)
    val lines = file.length

    dfg.II = targetedII

    for (i <- 1 until lines) {
      val index0 = file(i).indexOf("->")
      val index1 = file(i).indexOf('[')
      val index2 = file(i).indexOf('=')
      val index3 = file(i).indexOf(']')
      if (index1 != -1) {
        if (index0 == -1) {
          val nodeName = file(i).substring(0, index1)
          val opcode = genOpCode(file(i).substring(index2 + 1, index3))
          dfg.addOpNode(new OpNode(nodeName))
          dfg.applyOp(nodeName).opcode = OpEnum(opcode)
        }
        else {
          val from = file(i).substring(0, index0)
          if (dfg.applyOp(from).output == null) {
            dfg.addValNode(new ValNode(from + "OUT"))
            dfg.applyOp(from).output = dfg.applyVal(from + "OUT")
          }
          val sink = file(i).substring(index0 + 2, index1)
          val operand = Integer.parseInt(file(i).substring(index2 + 1, index3))
          dfg.applyOp(from).output.output.append(dfg.applyOp(sink))
          dfg.applyOp(from).output.outputOperand.append(operand)
          dfg.applyOp(sink).input += (operand -> (dfg.applyOp(from)))
          dfg.applyOp(sink).inputLatency.append(0)
        }
      }
    }
    dfg.checkPrimaryInput()
    dfg
  }
}
