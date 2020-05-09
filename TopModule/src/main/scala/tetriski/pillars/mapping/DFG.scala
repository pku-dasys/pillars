package tetriski.pillars.mapping

import tetriski.pillars.core.OpEnum
import tetriski.pillars.core.OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer

/** An abstract class of nodes in DFG（IR).
 *
 */
class NodeDFG() {
}

/** Class describing opNodes in DFG.
 * The opNodes represent operators and primary I/O in a DFG.
 *
 * @constructor create a opNode
 * @param name the name of this opNode
 */
class OpNode(var name: String) extends NodeDFG {
  /** The output valNode of this opNode.
   */
  var output: ValNode = null

  /** The opcode of this opNode.
   */
  var opcode: OpEnum = null

  /** A map between the serial number of inputs and corresponding opNode.
   */
  var input = Map[Int, OpNode]()

  /** The latency of this opNode.
   */
  var latency = 0

  /** The annulate latency of this opNode.
   * This variable is only used for annulated DFG.
   */
  var annulateLatency = 0

  /** A variable indicating whether this opNode has a const input.
   */
  var constInput = false

  /** A variable indicating whether this opNode has been visited.
   */
  var visited = false

  /** The skew of this opNode.
   */
  var skew = 0

  /** The latencies between the inputs of this opNode and itself.
   */
  var inputLatency = ArrayBuffer[Int]()

  /** Set latency of an opNode.
   *
   * @param arg the latency of current opNode
   */
  def setLatency(arg: Int): Unit = {
    latency = arg
  }
}

/** Class describing valNodes in DFG.
 * The valNodes represent output values of operators in a DFG.
 *
 * @constructor create a valNode
 * @param name the name of this valNode
 */
class ValNode(var name: String) extends NodeDFG {
  /** Fan-outs of a valNode.
   */
  var output = ArrayBuffer[OpNode]()

  /** Corresponding operands in each fan-out.
   */
  var outputOperand = ArrayBuffer[Int]()
}

/** Class describing DFGs in Pillars.
 *
 * @constructor create a DFG
 * @param name the name of this DFG
 */
class DFG(var name: String) {
  /** Operators and primary I/O in a DFG.
   */
  var opNodes = ArrayBuffer[OpNode]()

  /** Output values of operators in a DFG.
   */
  var valNodes = ArrayBuffer[ValNode]()

  /** A map between opNodes and a serial number.
   */
  var opNodesMap = Map[String, Int]()

  /** A map between valNodes and a serial number.
   */
  var valNodesMap = Map[String, Int]()

  /** Get the num of opNodes.
   */
  def getOpSize(): Int = {
    opNodes.size
  }

  /** Get the num of valNodes.
   */
  def getValSize(): Int = {
    valNodes.size
  }

  /** Add an opNode into DFG.
   *
   * @param node the opNode being added
   */
  def addOpNode(node: OpNode): Unit = {
    opNodes.append(node)
    opNodesMap = opNodesMap + (node.name -> (getOpSize() - 1))
  }

  /** Add a valNode into DFG.
   *
   * @param node the valNode being added
   */
  def addValNode(node: ValNode): Unit = {
    valNodes.append(node)
    valNodesMap = valNodesMap + (node.name -> (getValSize() - 1))
  }

  /** Get a valNode from DFG using its name.
   *
   * @param name the name of valNode
   */
  def applyVal(name: String) = {
    valNodes(valNodesMap(name))
  }

  /** Get an opNode from DFG using its name.
   *
   * @param name the name of opNode
   */
  def applyOp(name: String) = {
    opNodes(opNodesMap(name))
  }

  /** Load a DFG from a TXT file, not used in real process.
   *
   * @param Filename the file name of TXT file
   */
  def loadTXT(Filename: String): Unit = {
    import scala.io.Source

    val buffer = Source.fromFile(Filename)
    val file = buffer.getLines().toArray
    var now: Int = 0
    val valSize: Int = Integer.parseInt(file(now))

    for (i <- 0 until valSize) {
      now += 1
      val name: String = file(now).substring(1, file(now).length - 1)
      addValNode(new ValNode(name))
      now += 1
      val outputSize = Integer.parseInt(file(now))
      now += (outputSize + 1)
      for (j <- 0 until outputSize) {
        now += 1
        valNodes(i).outputOperand.append(Integer.parseInt(file(now)))
      }
    }

    now += 1
    val opSize: Int = Integer.parseInt(file(now))

    for (i <- 0 until opSize) {
      now += 1
      val name: String = file(now).substring(1, file(now).length - 1)
      addOpNode(new OpNode(name))
      now += 1
      if (file(now) != "----") {
        opNodes(i).output = applyVal(file(now))
      }
      now += 1
      opNodes(i).opcode = OpEnum(Integer.parseInt(file(now)));
    }

    now = 1
    for (i <- 0 until valSize) {
      now += 1
      val outputSize = Integer.parseInt(file(now))
      for (j <- 0 until outputSize) {
        now += 1
        valNodes(i).output.append(applyOp(file(now)))
      }
      now += (outputSize + 2)
    }
  }

  /** Debug Function to print DFG into screen.
   *
   */
  def printDFG(): Unit = {
    println(valNodes.size)
    for (_val <- valNodes) {
      println("<" + _val.name + ">")
      println(_val.output.size)
      for (output <- _val.output) {
        println(output.name)
      }
      println(_val.outputOperand.size)
      for (operand <- _val.outputOperand) {
        println(operand)
      }
    }
    println(opNodes.size)
    for (op <- opNodes) {
      println("<" + op.name + ">")
      println(op.output.name)
      println(op.opcode.id)
    }
  }
}