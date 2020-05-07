package tetriski.pillars.core

import OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer
import MRRGMode._
import tetriski.pillars.mapping.NodeDFG

class NodeMRRG(var name : String) extends Cloneable {
  var fanIn = ArrayBuffer[NodeMRRG]()
  var fanOut = ArrayBuffer[NodeMRRG]()
  var ops = ArrayBuffer[OpEnum]()
  var mapnode : NodeDFG = null
  var mode = NORMAL_MODE

  def getName() : String = name
  def setName(newName : String): Unit = {
    name = newName
  }
  def setMode(newMode : Int): Unit ={
    mode = newMode
  }

  override def clone: NodeMRRG = {
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

  override def clone: MRRG = {
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

  def apply(name : String):NodeMRRG = nodes(nodeMap(name))

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

  def loadtxt(filename : String) : Unit = {
    import scala.io.Source

    val buffer = Source.fromFile(filename)
    val file = buffer.getLines().toArray

    var now : Int = 0
    var rsize : Int = Integer.parseInt(file(now))
    var i : Int = 0

    for (i <- 0 until rsize) {
      now += 1
      var name : String = file(now).substring(1, file(now).length-1)
      addNode(new NodeMRRG(name))
      now += 1
      var faninsize = Integer.parseInt(file(now))
      now += (faninsize + 1)
      var fanoutsize = Integer.parseInt(file(now))
      now += fanoutsize
    }

    now += 1
    var fsize : Int = Integer.parseInt(file(now))
    for(i <- 0 until fsize) {
      now += 1
      var name : String = file(now).substring(1, file(now).length-1)
      addNode(new NodeMRRG(name))
      now += 1
      var faninsize = Integer.parseInt(file(now))
      now += (faninsize + 1)
      var fanoutsize = Integer.parseInt(file(now))
      now += (fanoutsize + 1)
      var opsize = Integer.parseInt(file(now))
      var j = 0
      for (j <- 0 until opsize) {
        now += 1
        nodes(i + rsize).ops.append(OpEnum(Integer.parseInt(file(now))))
      }
    }

    now = 0
    for(i <- 0 until rsize) {
      now += 1
      var name : String = file(now).substring(1, file(now).length-1)
      now += 1
      var faninsize = Integer.parseInt(file(now))
      var j = 0
      for (j <- 0 until faninsize) {
        now += 1
        apply(name).fanIn.append(apply(file(now)))
      }
      now += 1
      var fanoutsize = Integer.parseInt(file(now))
      for (j <- 0 until fanoutsize) {
        now += 1
        apply(name).fanOut.append(apply(file(now)))
      }
    }

    now += 1
    for(i <- 0 until fsize) {
      now += 1
      var name : String = file(now).substring(1, file(now).length-1)
      now += 1
      var faninsize = Integer.parseInt(file(now))
      var j = 0
      for (j <- 0 until faninsize) {
        now += 1
        addConnect(file(now), name)
      }
      now += 1
      var fanoutsize = Integer.parseInt(file(now))
      for (j <- 0 until fanoutsize) {
        now += 1
        apply(name).fanOut.append(apply(file(now)))
      }
      now += 1
      var opsize = Integer.parseInt(file(now))
      now += opsize
    }
  }
}

object MRRGMode{
  val NORMAL_MODE = 0
  val REG_MODE = 1
  val MEM_MODE = 2
}
