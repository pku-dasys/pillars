package tetriski.pillars.Mapping

import tetriski.pillars.core.OpEnum
import tetriski.pillars.core.OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer

class NodeDFG() {
  var cycles: Int = 0
}

class OpNode(var name: String) extends NodeDFG {
  var output: ValNode = null
  var opcode: OpEnum = null
  var input = Map[Int, OpNode]()
  var latency = 0
  var annulateLatency = 0
  var constInput = false
  var visited = false
  var skew = 0
  var inputLatency = ArrayBuffer[Int]()

  def setLatency(arg: Int): Unit = {
    latency = arg
  }
}

class ValNode(var name: String) extends NodeDFG {
  var output = ArrayBuffer[OpNode]()
  var output_operand = ArrayBuffer[Int]()
}

class DFG(var name: String) {
  var op_nodes = ArrayBuffer[OpNode]()
  var val_nodes = ArrayBuffer[ValNode]()
  var op_nodes_map = Map[String, Int]()
  var val_nodes_map = Map[String, Int]()

  def getOpSize(): Int = {
    op_nodes.size
  }

  def getValSize(): Int = {
    val_nodes.size
  }

  def addOpNode(node: OpNode): Unit = {
    op_nodes.append(node)
    op_nodes_map = op_nodes_map + (node.name -> (getOpSize() - 1))
  }

  def addValNode(node: ValNode): Unit = {
    val_nodes.append(node)
    val_nodes_map = val_nodes_map + (node.name -> (getValSize() - 1))
  }

  def applyVal(name: String) = {
    val_nodes(val_nodes_map(name))
  }

  def applyOp(name: String) = {
    op_nodes(op_nodes_map(name))
  }

  def loadTXT(Filename: String): Unit = {
    import scala.io.Source

    val buffer = Source.fromFile(Filename)
    val file = buffer.getLines().toArray
    var now: Int = 0
    val valsize: Int = Integer.parseInt(file(now))

    for (i <- 0 until valsize) {
      now += 1
      val name: String = file(now).substring(1, file(now).length - 1)
      //println(name)
      addValNode(new ValNode(name))
      now += 1
      val outputsize = Integer.parseInt(file(now))
      now += (outputsize + 1)
      for (j <- 0 until outputsize) {
        now += 1
        val_nodes(i).output_operand.append(Integer.parseInt(file(now)))
      }
    }

    now += 1
    val opsize: Int = Integer.parseInt(file(now))

    for (i <- 0 until opsize) {
      now += 1
      val name: String = file(now).substring(1, file(now).length - 1)
      addOpNode(new OpNode(name))
      now += 1
      if (file(now) != "----") {
        op_nodes(i).output = applyVal(file(now))
      }
      now += 1
      op_nodes(i).opcode = OpEnum(Integer.parseInt(file(now)));
    }

    now = 1
    for (i <- 0 until valsize) {
      now += 1
      val outputsize = Integer.parseInt(file(now))
      for (j <- 0 until outputsize) {
        now += 1
        val_nodes(i).output.append(applyOp(file(now)))
      }
      now += (outputsize + 2)
    }
  }

  def printDFG(): Unit = {
    println(val_nodes.size)
    for (_val <- val_nodes) {
      println("<" + _val.name + ">")
      println(_val.output.size)
      for (output <- _val.output) {
        println(output.name)
      }
      println(_val.output_operand.size)
      for (operand <- _val.output_operand) {
        println(operand)
      }
    }
    println(op_nodes.size)
    for (op <- op_nodes) {
      println("<" + op.name + ">")
      println(op.output.name)
      println(op.opcode.id)
    }
  }
}