package tetriski.pillars.core

import chisel3.util.log2Up
//import tetriski.pillars.hardware.PillarsConfig._
//import tetriski.pillars.core.NodeMRRG

trait ModuleTrait extends Ports with ModuleBasic {
  var mrrg = new MRRG()
  var internalNodes = List[String]()


  //to be update
  def updateConfig(fanInNums : List[Int], fanOutNums : List[Int], internalNum : Int): Unit ={
    if(internalNum > 0 ){
      if(supOps.size > 0){
        //alu passby
        //ALU_COPY_A = 11.U(4.W)
        //ALU_COPY_B = 12.U(4.W)
        val newConfig = fanInNums(0) match {
          case 0 => 11
          case 1 => 12
        }
        updateConfigArray(newConfig)
      }else{
        //register files
        val configSize = getConfigBit()
        val inPortNum = getInPorts().size
        val outPortNum = getOutPorts().size
        val internalNodeNum = internalNodes.size
        val singleConfigSize = log2Up(internalNodeNum)
        val oldConfig = getBigIntConfig()
        var newConfig = oldConfig
        for(fanInNum <- fanInNums){
          newConfig = (newConfig & ~(1 << (singleConfigSize * fanInNum))) | (internalNum << (singleConfigSize * fanInNum))
        }
        for(fanOutNum <- fanOutNums){
          newConfig = (newConfig & ~(1 << (singleConfigSize * (fanOutNum + inPortNum)))) | (internalNum << (singleConfigSize * (fanOutNum + inPortNum)))
        }
        updateConfigArray(newConfig)
      }
    }else{
      updateConfigArray(fanInNums(0))
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
      for(i <- 0 until internalNodes.size){
        val internalNode = internalNodes(i)
        val node = new NodeMRRG(internalNode)
        //only the first internalNode can contain ops
        if(supOps.size > 0 && i == 0){
          node.ops.appendAll(supOps)
        }
        mrrg.addNode(node)
        //ALU passby
        if(supOps.size>0 && internalNodes.size > 1){
          val nodeOut = new NodeMRRG(internalNode + "_out")
          mrrg.addNode(nodeOut)
          mrrg.addConnect(internalNode, internalNode + "_out")
        }
      }

    for(internalNode <- internalNodes){
      for(inPort <- inPorts){
        mrrg.addConnect(inPort, internalNode)
      }
      if(supOps.size > 0 && internalNodes.size > 1){
        for(outPort <- outPorts){
          mrrg.addConnect(internalNode + "_out", outPort)
        }
      }else{
        for(outPort <- outPorts){
          mrrg.addConnect(internalNode, outPort)
        }
      }
    }
   // }
    mrrg
  }

}
