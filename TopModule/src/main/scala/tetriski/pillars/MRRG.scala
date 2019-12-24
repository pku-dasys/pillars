package tetriski.pillars

import tetriski.pillars.OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer

class NodeMRRG(var name : String) {
  var fanIn = ArrayBuffer[NodeMRRG]()
  var fanOut = ArrayBuffer[NodeMRRG]()
  var ops = ArrayBuffer[OpEnum]()

  def getName() : String = name
  def setName(newName : String): Unit = {
    name = newName
  }
}

class MRRG {
  var nodes = ArrayBuffer[NodeMRRG]()
  var nodeMap = Map[String, Int]()

  def getSize(): Int ={
    nodes.size
  }
  def addNode(node : NodeMRRG): Unit ={
    nodes.append(node)
    nodeMap = nodeMap + (node.getName() -> (getSize()-1))
  }

  def apply(name : String) = nodes(nodeMap(name))

  def update(oldName : String, newName : String): Unit ={
    val num = nodeMap(oldName)
    val node = apply(oldName)
    nodeMap = nodeMap - oldName

    node.setName(newName)
    nodeMap = nodeMap + (newName -> num)

  }

  def mergy(arg : MRRG): Unit ={
    for(node <- arg.nodes){
      nodes.append(node)
      nodeMap = nodeMap + (node.getName() -> (getSize()-1))
    }
  }

  def addConnect(source : String, sinks : List[String]): Unit ={
    val sourceNode = apply(source)
    for (sink <- sinks){
      val sinkNode = apply(sink)
      sourceNode.fanOut.append(sinkNode)
      sinkNode.fanIn.append(sourceNode)
    }
  }

  def addConnect(source : String, sink : String): Unit ={
    val sourceNode = apply(source)
      val sinkNode = apply(sink)
      sourceNode.fanOut.append(sinkNode)
      sinkNode.fanIn.append(sourceNode)
  }

  def getNoOpSet() : Set[NodeMRRG] = {
    var ret = Set[NodeMRRG]()
    for(node <- nodes){
      if(node.ops.size == 0){
        ret = ret + node
      }
    }
    ret
  }
}
