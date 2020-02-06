package tetriski.pillars.core

import OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer
import MRRGMode._

class NodeMRRG(var name : String) extends Cloneable {
  var fanIn = ArrayBuffer[NodeMRRG]()
  var fanOut = ArrayBuffer[NodeMRRG]()
  var ops = ArrayBuffer[OpEnum]()
  var mode = NORMAL_MODE

  def getName() : String = name
  def setName(newName : String): Unit = {
    name = newName
  }
  def setMode(newMode : Int): Unit ={
    mode = newMode
  }

  override def clone = {
    val clone = super.clone.asInstanceOf[NodeMRRG]
    clone.fanIn = ArrayBuffer[NodeMRRG]()
    clone.fanOut = ArrayBuffer[NodeMRRG]()
    clone.ops = ops.clone
    clone.mode = mode
//    if(mode == REG_MODE){
//      println("test")
//    }
    clone
  }
}

class MRRG extends Cloneable {
  var nodes = ArrayBuffer[NodeMRRG]()
  var nodeMap = Map[String, Int]()
  var undeterminedOutConnects = List[List[NodeMRRG]]()
  var undeterminedInConnects = List[List[NodeMRRG]]()

  override def clone = {
    val clone = super.clone.asInstanceOf[MRRG]
    clone.nodes = ArrayBuffer[NodeMRRG]()
    clone.undeterminedOutConnects = List[List[NodeMRRG]]()
    clone.undeterminedInConnects = List[List[NodeMRRG]]()
    for(node <- nodes){
      clone.nodes.append(node.clone)
    }
    for(node <- nodes){
      val sourceName = node.getName()
      for(out <- node.fanOut){
        val sinkName = out.getName()
        clone.addConnect(sourceName, sinkName)
      }
    }
    for(undeterminedConnect <- undeterminedOutConnects){
      val sourceName = undeterminedConnect(0).getName()
      val sinkName = undeterminedConnect(1).getName()
      clone.addUndeterminedOutConnect(sourceName, sinkName)
    }
    for(undeterminedConnect <- undeterminedInConnects){
      val sourceName = undeterminedConnect(0).getName()
      val sinkName = undeterminedConnect(1).getName()
      clone.addUndeterminedInConnect(sourceName, sinkName)
    }
    clone
  }

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
    undeterminedOutConnects = undeterminedOutConnects ::: arg.undeterminedOutConnects
    undeterminedInConnects = undeterminedInConnects ::: arg.undeterminedInConnects
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

  def addUndeterminedOutConnect(source : String, sink : String): Unit ={
    undeterminedOutConnects = undeterminedOutConnects :+ List(apply(source), apply(sink))
  }

  def addUndeterminedInConnect(source : String, sink : String): Unit ={
    undeterminedInConnects = undeterminedInConnects :+ List(apply(source), apply(sink))
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

object MRRGMode{
  val NORMAL_MODE = 0
  val REG_MODE = 1
  val MEM_MODE = 2
}
