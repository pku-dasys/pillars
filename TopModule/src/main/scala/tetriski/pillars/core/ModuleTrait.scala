package tetriski.pillars.core

import chisel3.util.log2Up

import scala.collection.mutable.ArrayBuffer
import MRRGMode._
import util.control.Breaks._
//import tetriski.pillars.hardware.PillarsConfig._
//import tetriski.pillars.core.NodeMRRG

trait ModuleTrait extends Ports with ModuleBasic {
  var mrrg = new MRRG()
  var mode = NORMAL_MODE
  var internalNodes = List[String]()
  var bannedINodeSet = Set[BigInt]()

  def setMRRGMode(newMode : Int): Unit ={
    mode = newMode
  }

  //to be update
  def updateConfig(fanInNums : List[Int], fanOutNums : List[Int], internalNum : Int): Unit ={
    if(internalNodes.size > 1 ){
      if(supOps.size > 0){
        //alu passby
        //ALU_COPY_A = 12.U(4.W)
        //ALU_COPY_B = 13.U(4.W)
        val newConfig = fanInNums(0) match {
          case 0 => 12
          case 1 => 13
        }
        updateConfigArray(newConfig)
      }else {
        //register files
        //        val configSize = getConfigBit()
        val inPortNum = getInPorts().size
        val outPortNum = getOutPorts().size
        val internalNumBigInt: BigInt = internalNum
        val internalNodeNum = internalNodes.size
        val singleConfigSize = log2Up(internalNodeNum)
        val oldConfig = getBigIntConfig()
        var newConfig: BigInt = oldConfig
        val singleConfigMask: BigInt = ((1 << singleConfigSize) - 1)
        for (fanInNum <- fanInNums) {
          breakable {
            if(fanInNum == -1){
              break
            }
            if (fanInNum >= inPortNum) {
              bannedINodeSet = bannedINodeSet + internalNumBigInt
              break
            }

            // guarantee a single register does not have two inputs
            val currentInputConfigArray = new ArrayBuffer[BigInt]()

            for (i <- 0 until inPortNum) {
              val tempMask: BigInt = singleConfigMask << (singleConfigSize * i)
              val singleConfig = (newConfig & tempMask) >> (singleConfigSize * i)
              currentInputConfigArray.append(singleConfig)
            }
            var configSet = Set[BigInt]()
            for (i <- 0 until internalNodeNum) {
              configSet = configSet + i
            }
            //val unusedConfigArray = (configSet &~ currentInputConfigArray.toSet).toArray
            bannedINodeSet = bannedINodeSet + internalNumBigInt
            val unusedConfigArray = (configSet &~ bannedINodeSet).toArray
            val unusedConfig = unusedConfigArray(0)
            if (currentInputConfigArray.contains(internalNumBigInt)) {
              for (i <- 0 until inPortNum) {
                val inputConfig = currentInputConfigArray(i)
                if (inputConfig == internalNumBigInt) {
                  val tempMask: BigInt = ~(singleConfigMask << (singleConfigSize * i))
                  val clearConfig = newConfig & tempMask
                  val replaceConfig: BigInt = unusedConfig << (singleConfigSize * i)
                  newConfig = clearConfig | replaceConfig
                }
              }
            }


            val mask: BigInt = ~(singleConfigMask << (singleConfigSize * (fanInNum)))
            val clearConfig = newConfig & mask
            val replaceConfig: BigInt = internalNumBigInt << (singleConfigSize * fanInNum)
            newConfig = clearConfig | replaceConfig
          }
        }
        for (fanOutNum <- fanOutNums) {
          breakable {
            if(fanOutNum == -1){
              break
            }
            if (fanOutNum >= outPortNum) {
              break
            }
            //          val singleConfigMask : BigInt = ((1<< singleConfigSize)-1)
            val mask: BigInt = ~(singleConfigMask << (singleConfigSize * (fanOutNum + inPortNum)))
            val clearConfig = newConfig & mask
            val replaceConfig: BigInt = internalNumBigInt << (singleConfigSize * (fanOutNum + inPortNum))
            newConfig = clearConfig | replaceConfig
          }
        }
          updateConfigArray(newConfig)

      }
    }else{
      //mux
      updateConfigArray(fanInNums(0))
    }
  }

  def updateConfig(opcode : Int): Unit ={
    for (i <- 0 until supOps.size){
      if(supOps(i).toString.toInt == opcode){
        updateConfigArray(OpcodeTranslator.getModuleOpcode(supOps(i)))
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

  def updateConfigArray(newConfig : BigInt): Unit ={
    configArray.clear()
    var t = newConfig
    val configSize = getConfigBit()
    for(i <- 0 until configSize){
      val bit = t & 1
      configArray.append(bit.toInt)
      t = t >> 1
    }
  }

  def getBigIntConfig() : BigInt = {
    var ret : BigInt = 0
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
    val newNodes = (0 to num-1).map(i => "internalNode_" + (i + size).toString).toList
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
        node.setMode(mode)
        //only the first internalNode can contain ops
        if(supOps.size > 0 && i == 0){
          node.ops.appendAll(supOps)
        }
        mrrg.addNode(node)
        //ALU byPass
        if(supOps.size>0 && internalNodes.size > 1){
          var nodeName = "funcOut"
          if(i > 0){
            nodeName = "byPassOut"
          }
          val nodeOut = new NodeMRRG(nodeName)
          mrrg.addNode(nodeOut)
          mrrg.addConnect(internalNode, nodeName)
        }
      }

    for(i <- 0 until internalNodes.size){
      val internalNode = internalNodes(i)
      for(inPort <- inPorts){
        mrrg.addUndeterminedInConnect(inPort, internalNode)
      }
      if(supOps.size > 0 && internalNodes.size > 1){
        for(outPort <- outPorts){
          var nodeName = "funcOut"
          if(i > 0){
            nodeName = "byPassOut"
          }
          mrrg.addConnect(nodeName, outPort)
        }
      }else{
        for(outPort <- outPorts){
          mrrg.addUndeterminedOutConnect(internalNode, outPort)
          //mrrg.addConnect(internalNode, outPort)
        }
      }
    }
   // }
    mrrg
  }

}
