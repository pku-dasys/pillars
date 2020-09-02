package tetriski.pillars.core

import OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer
import MRRGMode._
import tetriski.pillars.mapping.NodeDFG

import scala.collection.mutable

/** A node of MRRG.
 * The function clone is overridden for MRRG unrolling.
 *
 * @constructor create a node of MRRG
 * @param name the name of the node
 */
class NodeMRRG(var name: String) extends Cloneable {
  /** The fan-ins of this MRRG node.
   */
  var fanIn = ArrayBuffer[NodeMRRG]()

  /** The fan-outs of this MRRG node.
   */
  var fanOut = ArrayBuffer[NodeMRRG]()

  /** The opcodes this MRRG node can support.
   */
  var ops = ArrayBuffer[OpEnum]()

  /** The DFG node mapped onto this MRRG node.
   */
  var mapNode: NodeDFG = null

  /** The mode of MRRG generation.
   * The default mode is normal mode.
   */
  var mode = NORMAL_MODE

  /** Get the name of this MRRG node.
   *
   * @return the name of this MRRG node
   */
  def getName(): String = name

  /** Set the name of this MRRG node.
   *
   * @param newName a new name
   */
  def setName(newName: String): Unit = {
    name = newName
  }

  /** Set the mode of MRRG generation of this MRRG node.
   *
   * @param newMode a new mode
   */
  def setMode(newMode: Int): Unit = {
    mode = newMode
  }

  /** The function clone is overridden for MRRG unrolling.
   */
  override def clone: NodeMRRG = {
    val clone = super.clone.asInstanceOf[NodeMRRG]
    clone.fanIn = ArrayBuffer[NodeMRRG]()
    clone.fanOut = ArrayBuffer[NodeMRRG]()
    clone.ops = ops.clone
    clone.mode = mode
    clone
  }
}

/** An MRRG (modulo routing resource graph).
 * The function clone is overridden for MRRG unrolling.
 *
 * @constructor create an MRRG
 */
class MRRG extends Cloneable {
  /** Nodes in this MRRG.
   */
  var nodes = ArrayBuffer[NodeMRRG]()

  /** A map between the name of a node and itself.
   */
  var nodeMap = Map[String, Int]()

  /** A list of undetermined connections between internal nodes and nodes representing the output ports.
   * When II > 1, it may be connected across cycles.
   */
  var undeterminedOutConnects = List[List[NodeMRRG]]()

  /** A list of undetermined connections between nodes representing the input ports and internal nodes.
   * When II > 1, it may be connected across cycles.
   */
  var undeterminedInConnects = List[List[NodeMRRG]]()

  /** A map between a pair of source/sink opNode and their distance.
   */
  var shortestDistanceMap = scala.collection.mutable.Map[(String, String), Int]()

  /** The function clone is overridden for MRRG unrolling.
   */
  override def clone: MRRG = {
    val clone = super.clone.asInstanceOf[MRRG]
    clone.nodes = ArrayBuffer[NodeMRRG]()
    clone.undeterminedOutConnects = List[List[NodeMRRG]]()
    clone.undeterminedInConnects = List[List[NodeMRRG]]()
    for (node <- nodes) {
      clone.nodes.append(node.clone)
    }
    for (node <- nodes) {
      val sourceName = node.getName()
      for (out <- node.fanOut) {
        val sinkName = out.getName()
        clone.addConnect(sourceName, sinkName)
      }
    }
    for (undeterminedConnect <- undeterminedOutConnects) {
      val sourceName = undeterminedConnect(0).getName()
      val sinkName = undeterminedConnect(1).getName()
      clone.addUndeterminedOutConnect(sourceName, sinkName)
    }
    for (undeterminedConnect <- undeterminedInConnects) {
      val sourceName = undeterminedConnect(0).getName()
      val sinkName = undeterminedConnect(1).getName()
      clone.addUndeterminedInConnect(sourceName, sinkName)
    }
    clone
  }

  /** Get the size of this MRRG.
   *
   * @return the size of this MRRG
   */
  def getSize(): Int = {
    nodes.size
  }

  /** Add a new MRRG node in this MRRG.
   *
   * @param node the new MRRG node
   */
  def addNode(node: NodeMRRG): Unit = {
    nodes.append(node)
    nodeMap = nodeMap + (node.getName() -> (getSize() - 1))
  }

  /** Get a MRRG node with its name.
   *
   * @param name the name of this MRRG node
   * @return this MRRG node
   */
  def apply(name: String): NodeMRRG = nodes(nodeMap(name))

  /** Update the name of a MRRG node.
   *
   * @param oldName the old name of this MRRG node
   * @param newName the new name of this MRRG node
   */
  def update(oldName: String, newName: String): Unit = {
    val num = nodeMap(oldName)
    val node = apply(oldName)
    nodeMap = nodeMap - oldName

    node.setName(newName)
    nodeMap = nodeMap + (newName -> num)

  }

  /** Merge this MRRG with another.
   *
   * @param arg another MRRG
   * @return the merged MRRG
   */
  def merge(arg: MRRG): Unit = {
    for (node <- arg.nodes) {
      nodes.append(node)
      nodeMap = nodeMap + (node.getName() -> (getSize() - 1))
    }
    undeterminedOutConnects = undeterminedOutConnects ::: arg.undeterminedOutConnects
    undeterminedInConnects = undeterminedInConnects ::: arg.undeterminedInConnects
  }

  /** Add connections between a source node and several sink nodes.
   *
   * @param source the name of the source node
   * @param sinks  a list of names of the sink nodes
   */
  def addConnect(source: String, sinks: List[String]): Unit = {
    val sourceNode = apply(source)
    for (sink <- sinks) {
      val sinkNode = apply(sink)
      sourceNode.fanOut.append(sinkNode)
      sinkNode.fanIn.append(sourceNode)
    }
  }

  /** Add a connection between a source node and a sink node.
   *
   * @param source the name of the source node
   * @param sink   the name of the sink node
   */
  def addConnect(source: String, sink: String): Unit = {
    val sourceNode = apply(source)
    val sinkNode = apply(sink)
    sourceNode.fanOut.append(sinkNode)
    sinkNode.fanIn.append(sourceNode)
  }

  /** Add a undetermined connection between an internal node and a node representing the output port.
   *
   * @param source the name of the source node
   * @param sink   the name of the sink node
   */
  def addUndeterminedOutConnect(source: String, sink: String): Unit = {
    undeterminedOutConnects = undeterminedOutConnects :+ List(apply(source), apply(sink))
  }

  /** Add a undetermined connection between a node representing the input port and an internal node.
   *
   * @param source the name of the source node
   * @param sink   the name of the sink node
   */
  def addUndeterminedInConnect(source: String, sink: String): Unit = {
    undeterminedInConnects = undeterminedInConnects :+ List(apply(source), apply(sink))
  }

  /** Get the set of MRRG nodes in this MRRG which cannot support any opcode.
   *
   * @return the set of MRRG nodes in this MRRG which cannot support any opcode
   */
  def getNoOpSet(): Set[NodeMRRG] = {
    var ret = Set[NodeMRRG]()
    for (node <- nodes) {
      if (node.ops.size == 0) {
        ret = ret + node
      }
    }
    ret
  }

  /** Load an MRRG from a text file.
   * Currently, the mode of MRRG node is not stored in the text file,
   * so all nodes are in the default normal mode.
   *
   * @param filename the name of this file
   */
  def loadTXT(filename: String): Unit = {
    import scala.io.Source

    val buffer = Source.fromFile(filename)
    val file = buffer.getLines().toArray

    var now: Int = 0
    val numRoutingNodes: Int = Integer.parseInt(file(now))

    //Initialize routing nodes.
    for (i <- 0 until numRoutingNodes) {
      now += 1
      val name: String = file(now).substring(1, file(now).length - 1)
      addNode(new NodeMRRG(name))
      if((name.indexOf("rf0.internalNode") != -1) ||
        (name.indexOf("global_rf.internalNode") != -1)){
        nodes(i).mode == REG_MODE
      }
      now += 1
      val fanInSize = Integer.parseInt(file(now))
      now += (fanInSize + 1)
      val fanOutSize = Integer.parseInt(file(now))
      now += fanOutSize
    }

    now += 1
    val numFuncNodes: Int = Integer.parseInt(file(now))

    //Initialize functional nodes.
    for (i <- 0 until numFuncNodes) {
      now += 1
      val name: String = file(now).substring(1, file(now).length - 1)
      addNode(new NodeMRRG(name))
      if(name.indexOf("loadStoreUnit.internalNode") != -1){
        nodes(i + numRoutingNodes).mode = MEM_MODE
      }
      now += 1
      val fanInSize = Integer.parseInt(file(now))
      now += (fanInSize + 1)
      val fanOutSize = Integer.parseInt(file(now))
      now += (fanOutSize + 1)
      val opSize = Integer.parseInt(file(now))
      for (j <- 0 until opSize) {
        now += 1
        nodes(i + numRoutingNodes).ops.append(OpEnum(Integer.parseInt(file(now))))
      }
    }

    now = 0

    //Add connections of routing nodes.
    for (i <- 0 until numRoutingNodes) {
      now += 1
      val name: String = file(now).substring(1, file(now).length - 1)
      now += 1
      val fanInSize = Integer.parseInt(file(now))
      for (j <- 0 until fanInSize) {
        now += 1
        apply(name).fanIn.append(apply(file(now)))
      }
      now += 1
      val fanOutSize = Integer.parseInt(file(now))
      for (j <- 0 until fanOutSize) {
        now += 1
        apply(name).fanOut.append(apply(file(now)))
      }
    }

    now += 1
    //Add connections of functional nodes.
    for (i <- 0 until numFuncNodes) {
      now += 1
      val name: String = file(now).substring(1, file(now).length - 1)
      now += 1
      val fanInSize = Integer.parseInt(file(now))
      for (j <- 0 until fanInSize) {
        now += 1
        addConnect(file(now), name)
      }
      now += 1
      val fanOutSize = Integer.parseInt(file(now))
      for (j <- 0 until fanOutSize) {
        now += 1
        apply(name).fanOut.append(apply(file(now)))
      }
      now += 1
      var opSize = Integer.parseInt(file(now))
      now += opSize
    }
  }

  /** Floyd-Warshall shortest path algorithm.
   *
   * @param neighboringDistance initial search depth between nodes supporting opcodes
   */
  def shortestPath(neighboringDistance: Int): Map[(String, String), Int] = {
    val opNodes = (nodes.toSet -- getNoOpSet()).toArray
    val nodeNum = opNodes.size
    val MAX_DISTENCE = 9999999
    for (i <- 0 until nodeNum) {
      for (j <- i until nodeNum) {
        val sourceName = opNodes(i).name
        val sinkName = opNodes(j).name
        if (i == j) {
          shortestDistanceMap += ((sourceName, sinkName) -> 0)
        } else {
          shortestDistanceMap += ((sourceName, sinkName) -> MAX_DISTENCE)
          shortestDistanceMap += ((sinkName, sourceName) -> MAX_DISTENCE)
        }
      }
    }

    for (i <- 0 until nodeNum) {
      val sourceNode = opNodes(i)
      val tempNodesArray = new ArrayBuffer[Set[NodeMRRG]]()
      tempNodesArray.append(Set[NodeMRRG](sourceNode))
      for (depth <- 0 until neighboringDistance) {
        var nextDepthNodeSet = Set[NodeMRRG]()
        for (tempNode <- tempNodesArray(depth)) {
          for (fanout <- tempNode.fanOut) {
            nextDepthNodeSet += fanout
          }
        }
        tempNodesArray.append(nextDepthNodeSet)
      }
      for (depth <- 1 to neighboringDistance) {
        for (sinkNode <- tempNodesArray(depth)) {
          if (sinkNode.ops.size > 0) {
            shortestDistanceMap((sourceNode.name, sinkNode.name)) = Math.min(depth,
              shortestDistanceMap((sourceNode.name, sinkNode.name)))
          }
        }
      }
    }

    for (k <- 0 until nodeNum) {
      for (i <- 0 until nodeNum) {
        for (j <- 0 until nodeNum) {
          val sourceName = opNodes(i).name
          val sinkName = opNodes(j).name
          val tempName = opNodes(k).name
          shortestDistanceMap((sourceName, sinkName)) = Math.min(shortestDistanceMap(sourceName, sinkName),
            shortestDistanceMap((sourceName, tempName)) + shortestDistanceMap((tempName, sinkName)))
        }
      }
    }

    shortestDistanceMap.toMap
  }
}

/** MRRG generation mode which keeps the major consistency requirement, modeling consistency.
 * The modeling consistency ensures the latency between an I/O port and an inner
 * routing or functional unit in a module keeps consistency with
 * the corresponding latency in generated MRRG. (BlockTrait.graphUnroll)
 *
 * It also guarantees correct correspondence of two entity pairs:
 * (1) the number of internal routing nodes and the routing capabilities of a module.
 * (ElementTrait.addInternalNodesNum)
 * (2) the opcodes owned by a functional node and the functional capabilities of a module.
 * (BasicTrait.setSupOps)
 */
object MRRGMode {
  /** Normal mode.
   * Nodes belonging to the same copy of
   * unrolled MRRG generated by an element in normal mode are
   * all in the same reconfiguration cycle (RC).
   */
  val NORMAL_MODE = 0

  /** Register mode.
   * In addition to the delay between the routing node representing
   * the input port and the functional node, the distinction of register
   * mode is the edges between copies of internal routing nodes,
   * which modeling the storage function of registers.
   */
  val REG_MODE = 1

  /** Memory mode.
   * There is a delay between the functional node
   * and routing node representing the output port.
   */
  val MEM_MODE = 2
}
