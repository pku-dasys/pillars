package tetriski.pillars.mapping

import java.io.FileWriter
import java.util
import java.util.{List}

import tetriski.pillars.core.OpEnum
import tetriski.pillars.core.OpEnum.OpEnum

//import scala.collection
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

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

  /** A map between the identification number of inputs and corresponding opNode.
   */
  var input = Map[Int, OpNode]()

  /** The latency of this opNode.
   */
  var latency = 0

  /** The annulate latency of this opNode.
   * This variable is only used for annulated DFG.
   */
  var annulateLatency = 0

  /** A variable indicating whether this opNode has a primary input.
   */
  var primaryInput = false

  /** A variable indicating whether this opNode has been visited.
   */
  var visited = false

  /** The skew of this opNode.
   */
  var skew = 0

  /** The latencies between the inputs of this opNode and itself.
   */
  var inputLatency = ArrayBuffer[Int]()

  /** A variable indicating whether the operand0 and operand1 of this opNode is exchanged.
   */
  var commutated = false

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
  /** The the targeted II.
   */
  var II = 1

  /** Operators and primary I/O in a DFG.
   */
  var opNodes = ArrayBuffer[OpNode]()

  /** Output values of operators in a DFG.
   */
  var valNodes = ArrayBuffer[ValNode]()

  /** A map between opNodes and a identification number.
   */
  var opNodesMap = Map[String, Int]()

  /** A map between valNodes and a identification number.
   */
  var valNodesMap = Map[String, Int]()

  /** A map between the ID of a register and the IDs of its fan-out registers.
   */
  var regConnect = scala.collection.mutable.Map[Integer, List[Integer]]()

  /** A map between the ID of the module corresponding a funcNode and the IDs of its fan-out registers.
   */
  var func2regMap = scala.collection.mutable.Map[Integer, List[Integer]]()

  /** A map between the ID of a register and the IDs of its fan-out modules corresponding funcNodes.
   */
  var reg2funcMap = scala.collection.mutable.Map[Integer, List[List[Integer]]]()

  /** A map between the ID of the module corresponding a funcNode
   * and the IDs of its fan-out modules corresponding funcNodes.
   */
  var funcDirect2funcMap = scala.collection.mutable.Map[Integer, List[List[Integer]]]()

  /** The number of registers.
   */
  var regNum = -1

  /** A parameter indicating whether this DFG is synthesizable.
   */
  var synthesizable = false

  /** A map between some funcNodes and the fixed SRAM ID belonging to them
   */
  var fixedMapSRAM = Map[OpNode, Int]()

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
   * @param filename   the file name of TXT file
   * @param targetedII the targeted II
   */
  def loadTXT(filename: String, targetedII: Int = 1): Unit = {
    import scala.io.Source

    II = targetedII

    val buffer = Source.fromFile(filename)
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
      opNodes(i).opcode = OpEnum(Integer.parseInt(file(now)))
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

    checkPrimaryInput()
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

  /** Check whether a node has primary input.
   *
   */
  def checkPrimaryInput(): Unit = {
    for (node <- opNodes) {
      for (input <- node.input.values) {
        if (input.name.indexOf("const") != -1 ||
          input.name.indexOf("input") != -1) {
          node.primaryInput = true
        }
      }
    }
  }

  /** Update a unscheduled result TXT file with latency and skew.
   *
   * @param filename    the file name of result TXT file
   * @param latencyMap  a map between the name of a opNode and its latency
   * @param skewMap     a map between the name of a opNode and its skew
   * @param outFilename the file name of output file
   */
  def updateSchedule(filename: String, latencyMap: util.Map[String, Integer] = null,
                     skewMap: util.Map[String, Integer] = null, outFilename: String = null): Unit = {
    import scala.collection.JavaConverters


    if (latencyMap != null) {
      val scalaLatencyMap = JavaConverters.mapAsScalaMap(latencyMap)
      for (item <- scalaLatencyMap) {
        applyOp(item._1).latency = item._2
      }
    }
    if (skewMap != null) {
      val scalaSkewMap = JavaConverters.mapAsScalaMap(skewMap)
      for (item <- scalaSkewMap) {
        applyOp(item._1).skew = item._2
      }
      for (node <- opNodes) {
        if (node.output != null) {
          if (node.output.output.contains(node)) {
            if (node.input(0) == node) {
              node.annulateLatency = node.skew + II
            } else if (node.input(1) == node) {
              node.annulateLatency = node.skew - II
            }
          }
        }
      }
    }

    val unscheduledArray = Source.fromFile(filename).getLines().toArray
    var scheduleFilename = outFilename
    if (scheduleFilename == null) {
      scheduleFilename = filename
    }
    val scheduleFile = new FileWriter(scheduleFilename)
    var i = 0
    var beginCycle = 0
    val pattern = "[0-9]+:".r

    val minLatency = opNodes.map(op => op.latency).min
    opNodes.foreach(op => op.setLatency(op.latency - minLatency))

    /** Calculate fire time of each opNodes in DFG.
     */
    for (j <- 0 until opNodes.size) {
      val op = opNodes(j)
      val tempResult = unscheduledArray(j).split(" ").toList
      val mrrgName = tempResult(1)
      val tempStr = (pattern findFirstIn mrrgName).toArray
      val rc = tempStr(0).replace(":", "").toInt
      if (op.latency == 0 && op.annulateLatency == 0) {
        beginCycle = rc
      }
    }

    for (op <- opNodes) {
      var outLatency = op.latency + beginCycle
      if (op.annulateLatency != 0 && op.primaryInput) {
        outLatency = outLatency + II
      }
      println(op.name + " " + outLatency + " " + op.skew)
      scheduleFile.write(unscheduledArray(i) + " " + outLatency + " " + op.skew + "\n")
      i = i + 1
    }
    scheduleFile.flush()
    scheduleFile.close()
  }
}