package tetriski.pillars.core

import chisel3.util.log2Up
//import tetriski.pillars.core.NodeMRRG

trait ModuleTrait extends Ports with ModuleBasic {
  var mrrg = new MRRG()
  var internalNodes = List[String]()


  //to be update
  def updateConfig(fanInNum : Int, fanOutNum : Int, internalNum : Int): Unit ={
    if(internalNum > 0 ){
      //register files
      val configSize = getConfigBit()
      val inPortNum = getInPorts().size
      val outPortNum = getOutPorts().size
      val internalNodeNum = internalNodes.size
      val singleConfigSize = log2Up(internalNodeNum)
      val oldConfig = getBigIntConfig()

      var newConfig = (oldConfig & ~(1 << (singleConfigSize * fanInNum))) | (internalNum << (singleConfigSize * fanInNum))
      newConfig = (newConfig & ~(1 << (singleConfigSize * (fanOutNum + inPortNum)))) | (internalNum << (singleConfigSize * (fanOutNum + inPortNum)))
      updateConfigArray(newConfig)
    }else{
      updateConfigArray(fanInNum)
    }
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

  def getBigIntConfig() : Int = {
    var ret : Int = 0
    val configSize = getConfigBit()
    for(i <- 0 until configSize){
      ret = ret << 1
      ret = ret + configArray.reverse(i)
    }
    ret
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
//    if(outPorts.size > 1){
//      for(internalNode <- internalNodes){
//        val nodeIn = new NodeMRRG(internalNode+"_in")
//        val nodeOut = new NodeMRRG(internalNode+"_out")
//        nodeIn.fanOut.append(nodeOut)
//        nodeOut.fanIn.append(nodeIn)
//        if(supOps.size > 0){
//          nodeIn.ops.appendAll(supOps)
//        }
//        mrrg.addNode(nodeIn)
//        mrrg.addNode(nodeOut)
//        for(inPort <- inPorts){
//          mrrg(inPort).fanOut.append(mrrg(internalNode+"_in"))
//          mrrg(internalNode+"_in").fanIn.append(mrrg(inPort))
//        }
//        for(outPort <- outPorts){
//          mrrg(internalNode+"_out").fanOut.append(mrrg(outPort))
//          mrrg(outPort).fanIn.append(mrrg(internalNode+"_out"))
//        }
//      }
//    }else{
      for(internalNode <- internalNodes){
        val node = new NodeMRRG(internalNode)
        if(supOps.size > 0){
          node.ops.appendAll(supOps)
        }
        mrrg.addNode(node)
        for(inPort <- inPorts){
          mrrg.addConnect(inPort, internalNode)
        }
        for(outPort <- outPorts){
          mrrg.addConnect(internalNode, outPort)
        }
      }
   // }
    mrrg
  }

}
