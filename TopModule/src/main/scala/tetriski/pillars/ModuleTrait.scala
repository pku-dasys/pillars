package tetriski.pillars

import scala.collection.mutable.ArrayBuffer


//Important note:
//Each module should possess this trait, with the help of which
//we can translate a string representation of a port into a list of integer.

//For example，in our minimal case, ["cgra/b_0:out/b_0_1:out/mul0:out"] can be
//translated into [1, 0, 0], where 1 (TypeID) means this port belongs to a multiplier
//and the first 0 (ModuleID) means this multiplier has index 0 in global,
//while the second 0 (portID) means the taget port is "out".

trait ModuleTrait extends Ports with ModuleBasic {
  var mrrg = new MRRG()
  var internalNodes = List[String]()


  //to be update
  def updateConfig(fanInNum : Int, fanOutNum : Int, internalNum : Int): Unit ={
    updateConfigArray(fanInNum)
  }

  def updateConfig(opcode : Int): Unit ={
    for (i <- 0 until supOps.size){
      if(supOps(i).toString.toInt == opcode){
        updateConfigArray(i)
      }
    }
  }

  def updateConfigArray(newConfig : Int): Unit ={
    configArray.clear()
    var t = newConfig
    val configSize = getConfigBit()
    for(i <- 0 until configSize){
      val bit = t & 1
      configArray.append(bit)
      t = t >> 1
    }
  }

  def addInternalNodes(arg : List[String]): Unit ={
    internalNodes = internalNodes ::: arg
  }

  def addInternalNodesNum(num : Int): Unit ={
    val size = internalNodes.length
    val newNodes = (0 to num-1).map(i => "internalNode_"+(i + size).toString).toList
    addInternalNodes(newNodes)
  }

  def initMRRG() : MRRG = {

    for(inPort <- inPorts){
      val node = new NodeMRRG(inPort)
      mrrg.addNode(node)
    }
    for(outPort <- outPorts){
      val node = new NodeMRRG(outPort)
      mrrg.addNode(node)
    }
    if(outPorts.size > 1){
      for(internalNode <- internalNodes){
        val nodeIn = new NodeMRRG(internalNode+"_in")
        val nodeOut = new NodeMRRG(internalNode+"_out")
        nodeIn.fanOut.append(nodeOut)
        nodeOut.fanIn.append(nodeIn)
        if(supOps.size > 0){
          nodeIn.ops.appendAll(supOps)
        }
        mrrg.addNode(nodeIn)
        mrrg.addNode(nodeOut)
        for(inPort <- inPorts){
          mrrg(inPort).fanOut.append(mrrg(internalNode+"_in"))
          mrrg(internalNode+"_in").fanIn.append(mrrg(inPort))
        }
        for(outPort <- outPorts){
          mrrg(internalNode+"_out").fanOut.append(mrrg(outPort))
          mrrg(outPort).fanIn.append(mrrg(internalNode+"_out"))
        }
      }
    }else{
      for(internalNode <- internalNodes){
        val node = new NodeMRRG(internalNode)
        if(supOps.size > 0){
          node.ops.appendAll(supOps)
        }
        mrrg.addNode(node)
        for(inPort <- inPorts){
          mrrg(inPort).fanOut.append(mrrg(internalNode))
          mrrg(internalNode).fanIn.append(mrrg(inPort))
        }
        for(outPort <- outPorts){
          mrrg(internalNode).fanOut.append(mrrg(outPort))
          mrrg(outPort).fanIn.append(mrrg(internalNode))
        }
      }
    }
    mrrg
  }

}





