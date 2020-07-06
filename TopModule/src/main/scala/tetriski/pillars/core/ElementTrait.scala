package tetriski.pillars.core

import chisel3.util.log2Up
import tetriski.pillars.core.MRRGMode._

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/** The trait of elements.
 * An element should have this trait to generate
 * the structure for reconfiguration.
 */
trait ElementTrait extends Ports with BasicTrait {
  /** The MRRG corresponding to this element.
   */
  var mrrg = new MRRG()

  /** The MRRG generation mode of this element.
   */
  var mode = NORMAL_MODE

  /** The names of internal nodes in the MRRG.
   */
  var internalNodes = List[String]()

  /** A set of internal nodes which have been used for routing.
   */
  var bannedINodeSet = Set[BigInt]()

  /** Get a port of this element.
   *
   * @param portName the name of this port
   * @return a valid port
   */
  def /(portName: String): ValidPort = {
    if (!(getInPorts().toSet.contains(portName) || getOutPorts().toSet.contains(portName))) {
      System.err.println(s"Invalid port name $name / $portName!")
    }
    new ValidPort(name, portName)
  }

  /** Set the MRRG generation mode of this element.
   */
  def setMRRGMode(newMode: Int): Unit = {
    mode = newMode
  }

  /** Update the configuration of the module represented by this element.
   * This function only is used when this module will perform routing.
   *
   * @param fanInNums   a list of the identification numbers of fan-ins
   * @param fanOutNums  a list of the identification numbers of fan-outs
   * @param internalNum the identification numbers of internal node that
   */
  def updateConfig(fanInNums: List[Int], fanOutNums: List[Int], internalNum: Int): Unit = {
    if (internalNodes.size > 1) {
      if (supOps.size > 0) {
        //ALU bypass
        //ALU_COPY_A = 12.U(4.W)
        //ALU_COPY_B = 13.U(4.W)
        val newConfig = fanInNums(0) match {
          case 0 => 12
          case 1 => 13
        }
        updateConfigArray(newConfig)
      } else {
        //register files
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
            if (fanInNum == -1) {
              break
            }

            //Guarantee a single register does not have two inputs.
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

            bannedINodeSet = bannedINodeSet + internalNumBigInt
            val unusedConfigArray = (configSet &~ bannedINodeSet).toArray
            if (unusedConfigArray.size == 0) {
              //Forbidden input.
              var replaceConfig: BigInt = 1
              //A matter of expediency for bugs when testing cap for II = 3
              //TODO: debug or find out why it's right
              if (bannedINodeSet.size <= 2) {
                replaceConfig = 0
              }
              replaceConfig = replaceConfig << (configArray.size - 1)
              newConfig = newConfig | replaceConfig
              break
            }

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

            if (fanInNum >= inPortNum) {
              break
            }

            //Update the configuration of the module.
            val mask: BigInt = ~(singleConfigMask << (singleConfigSize * (fanInNum)))
            val clearConfig = newConfig & mask
            val replaceConfig: BigInt = internalNumBigInt << (singleConfigSize * fanInNum)
            newConfig = clearConfig | replaceConfig
          }
        }
        for (fanOutNum <- fanOutNums) {
          breakable {
            if (fanOutNum == -1) {
              break
            }
            if (fanOutNum >= outPortNum) {
              break
            }

            val mask: BigInt = ~(singleConfigMask << (singleConfigSize * (fanOutNum + inPortNum)))
            val clearConfig = newConfig & mask
            val replaceConfig: BigInt = internalNumBigInt << (singleConfigSize * (fanOutNum + inPortNum))
            newConfig = clearConfig | replaceConfig
          }
        }
        updateConfigArray(newConfig)

      }
    } else {
      if (mode == REG_MODE) {
        //Single register.
        if (fanInNums(0) != -1) {
          updateConfigArray(1)
        }
      } else {
        //Multiplexer.
        updateConfigArray(fanInNums(0))
      }
    }
  }

  /** Update the configuration of the module represented by this element.
   * This function only is used when this module will perform a function.
   *
   * @param opcode the opcode of the function will be performed
   */
  def updateConfig(opcode: Int): Unit = {
    for (i <- 0 until supOps.size) {
      if (supOps(i).toString.toInt == opcode) {
        updateConfigArray(OpcodeTranslator.getModuleConfig(supOps(i)))
      }
    }
  }

  /** Update the configuration of the module represented by this element.
   * The configuration is saved in an array of bits.
   *
   * @param newConfig the new configuration in Int format
   */
  def updateConfigArray(newConfig: Int): Unit = {
    configArray.clear()
    var t = newConfig
    val configSize = getConfigBit()
    for (i <- 0 until configSize) {
      val bit = t & 1
      configArray.append(bit)
      t = t >> 1
    }
  }

  /** Update the configuration of the module represented by this element.
   * The configuration is saved in an array of bits.
   *
   * @param newConfig the new configuration in BigInt format
   */
  def updateConfigArray(newConfig: BigInt): Unit = {
    configArray.clear()
    var t = newConfig
    val configSize = getConfigBit()
    for (i <- 0 until configSize) {
      val bit = t & 1
      configArray.append(bit.toInt)
      t = t >> 1
    }
  }

  /** Get the configuration of the module represented by this element.
   *
   * @return the new configuration in BigInt format
   */
  def getBigIntConfig(): BigInt = {
    var ret: BigInt = 0
    val configSize = getConfigBit()
    for (i <- 0 until configSize) {
      ret = ret << 1
      ret = ret + configArray.reverse(i)
    }
    ret
  }

  /** Add some internal nodes.
   *
   * @param arg a list of names of the internal nodes will be added
   */
  def addInternalNodes(arg: List[String]): Unit = {
    internalNodes = internalNodes ::: arg
  }

  /** Add some internal nodes.
   * When using this function,
   * users should guarantee correct correspondence of the number of internal routing nodes
   * and the routing capabilities of a module.
   *
   * @param num the number of the internal nodes will be added
   */
  def addInternalNodesNum(num: Int): Unit = {
    val size = internalNodes.length
    val newNodes = (0 to num - 1).map(i => "internalNode_" + (i + size).toString).toList
    addInternalNodes(newNodes)
  }

  /** Initialize MRRG of this element.
   *
   * @return the MRRG of this element
   */
  def initMRRG(): MRRG = {

    for (inPort <- inPorts) {
      val node = new NodeMRRG(inPort)
      mrrg.addNode(node)
    }
    for (outPort <- outPorts) {
      val node = new NodeMRRG(outPort)
      mrrg.addNode(node)
    }
    for (i <- 0 until internalNodes.size) {
      val internalNode = internalNodes(i)
      val node = new NodeMRRG(internalNode)
      node.setMode(mode)

      //Only the first internalNode can support opcodes.
      if (supOps.size > 0 && i == 0) {
        node.ops.appendAll(supOps)
      }
      mrrg.addNode(node)

      //ALU bypass.
      if (supOps.size > 0 && internalNodes.size > 1) {
        var nodeName = "funcOut"
        if (i > 0) {
          nodeName = "byPassOut"
        }
        val nodeOut = new NodeMRRG(nodeName)
        mrrg.addNode(nodeOut)
        mrrg.addConnect(internalNode, nodeName)
      }
    }

    for (i <- 0 until internalNodes.size) {
      val internalNode = internalNodes(i)
      for (inPort <- inPorts) {
        mrrg.addUndeterminedInConnect(inPort, internalNode)
      }
      if (supOps.size > 0 && internalNodes.size > 1) {
        for (outPort <- outPorts) {
          var nodeName = "funcOut"
          if (i > 0) {
            nodeName = "byPassOut"
          }
          mrrg.addConnect(nodeName, outPort)
        }
      } else {
        for (outPort <- outPorts) {
          mrrg.addUndeterminedOutConnect(internalNode, outPort)
        }
      }
    }
    mrrg
  }

}

/** An abstract of valid ports.
 *
 * @constructor create a valid port.
 * @param parentName the name of the element or block which this port belongs to
 * @param portName   the name of this port
 */
class ValidPort(parentName: String, portName: String) {
  /** Get the name of the element or block which this port belongs to.
   *
   * @return the name of the element or block which this port belongs to
   */
  def getParentName(): String = parentName

  /** Get the name of this port.
   *
   * @return the name of this port
   */
  def getPortName(): String = portName

  /** Translate connections between valid ports into List format.
   *
   * @example If the return value of portA->portB is List(List(2, 3, 1), List(1, 1, 2)),
   *          it means that
   *          the output port "1" of the module "3" in the type "2" is connected to
   *          the input port "2" of the module "1" in the type "1".
   * @return connections between valid ports in List format
   */
  def ->(dst: ValidPort): List[List[String]] = {
    var srcList = List(parentName, portName)
    var dstList = List(dst.getParentName(), dst.getPortName())
    if (parentName == null) {
      srcList = List(portName)
    }
    if (dst.getParentName() == null) {
      dstList = List(dst.getPortName())
    }
    List(srcList, dstList)
  }
}