package tetriski.pillars

import tetriski.pillars.OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer

class Node(var name : String) {
  var fanIn = ArrayBuffer[Node]()
  var fanOut = ArrayBuffer[Node]()
  var ops = ArrayBuffer[OpEnum]()

  def getName() : String = name
  def setName(newName : String): Unit = {
    name = newName
  }
}

class MRRG {
  var nodes = ArrayBuffer[Node]()
  var nodeMap = Map[String, Int]()

  def getSize(): Int ={
    nodes.size
  }
  def addNode(node : Node): Unit ={
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
}
