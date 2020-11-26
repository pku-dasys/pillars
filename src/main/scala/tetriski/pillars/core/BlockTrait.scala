package tetriski.pillars.core

import java.io.{File, PrintWriter}

import tetriski.pillars.core.MRRGMode._
import tetriski.pillars.core.OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer

/** The trait of blocks.
 * A block should have this trait to adapt to hierarchical designs.
 *
 * Since some modification of coding style of this trait may influence
 * the order of MRRG nodes in MRRG TXT, the mapping results of new MRRG TXT
 * may be different with the some results prepared in advance,
 * but the prepared results can still work well.
 */
//TODO: update the results prepared in advance
trait BlockTrait extends ElementTrait {

  /** A variable indicating whether this block is a configuration region.
   */
  var isConfigRegion = false

  /** An array of ALUs belonging to this block or its sub-blocks.
   */
  var ALUsArray = new ArrayBuffer[ElementTrait]

  /** An array of RFs belonging to this block or its sub-blocks.
   */
  var RFsArray = new ArrayBuffer[ElementTrait]

  /** An array of multiplexers belonging to this block or its sub-blocks.
   */
  var MuxsArray = new ArrayBuffer[ElementTrait]

  /** An array of const units belonging to this block or its sub-blocks.
   */
  var ConstsArray = new ArrayBuffer[ElementTrait]

  /** An array of LSUs belonging to this block or its sub-blocks.
   */
  var LSUsArray = new ArrayBuffer[ElementTrait]

  /** An array of elements belonging to this block or its sub-blocks.
   */
  var elementsArray = new ArrayBuffer[ArrayBuffer[ElementTrait]]

  /** An array of elements belonging to this block directly.
   * Lists in Int format represent those elements.
   */
  var owningElements = new ArrayBuffer[List[Int]]

  //TODO: update this part with the features of Scala
  //It should be guaranteed that the order of element arrays here is consist with the type ID.
  elementsArray.append(ALUsArray)
  elementsArray.append(RFsArray)
  elementsArray.append(MuxsArray)
  elementsArray.append(ConstsArray)
  elementsArray.append(LSUsArray)

  /** The number of types of elements.
   */
  val typeNum = elementsArray.size

  /** The hierarchy name of this block.
   */
  var hierarchyName = new ArrayBuffer[String]

  /** Array(List(src, dst)),
   * where src and dst are lists representing ports which have a connection
   */
  var connectArray = new ArrayBuffer[List[List[String]]]

  /** A map between the name of a sub-block and itself.
   */
  var blockMap = Map[String, BlockTrait]()

  /** A map between the name of an element and itself.
   */
  var elementsMap = Map[String, ElementTrait]()

  /** A parameter indicating whether the MRRG is initialized.
   */
  var initialized = false

  /** Get a port of this block,
   * and declare it belongs to this block.
   * It will be used for get ports of sub-block of current block when designing the architecture.
   *
   * @param portName the name of this port
   * @return a valid port
   */
  override def /(portName: String): ValidPort = {
    if (!(getInPorts().toSet.contains(portName) || getOutPorts().toSet.contains(portName))) {
      System.err.println(s"Invalid port name $name / $portName!")
    }
    new ValidPort(name + "/", portName)
  }

  /** Initialize the name and hierarchy name of this block.
   */
  def initName(arg: String): Unit = {
    hierarchyName.append(arg)
    setName(arg)
  }

  /** Get a port of this block.
   * It will be used for get ports of current block when designing the architecture.
   *
   * @param portName the name of this port
   * @return a valid port
   */
  def term(portName: String): ValidPort = {
    if (!(getInPorts().toSet.contains(portName) || getOutPorts().toSet.contains(portName))) {
      System.err.println(s"Invalid port name $portName!")
    }
    new ValidPort(null, portName)
  }

  /** Update the hierarchy name.
   *
   * @param arg the new hierarchy name
   */
  def updateHierarchyName(arg: ArrayBuffer[String]): Unit = {
    for (str <- arg) {
      hierarchyName.append(str)
    }
    for (subBlocks <- blockMap.values) {
      subBlocks.updateHierarchyName(arg)
    }
  }

  /** Add a sub-block into this block.
   *
   * @param arg this sub-block
   * @return the map between the name of a sub-block and itself.
   */
  def addBlock(arg: BlockTrait): Map[String, BlockTrait] = {
    blockMap += (arg.getName() -> arg)
    //Add sub-block's elements into relevant array of parent module
    for (i <- 0 until arg.elementsArray.size) {
      for (j <- 0 until arg.elementsArray(i).size) {
        elementsArray(i).append(arg.elementsArray(i)(j))
      }
    }
    arg.updateHierarchyName(hierarchyName)
    configBit += arg.getConfigBit()
    blockMap
  }

  /** Add an element into this block.
   *
   * @param arg this element
   */
  def addElement(arg: ElementTrait): Unit = {
    arg.initMRRG()
    val typeNum = arg.getTypeID()
    elementsArray(typeNum).append(arg)
    elementsMap += (arg.getName() -> arg)
    owningElements.append(List(typeNum, elementsArray(typeNum).size - 1))
    configBit += arg.getConfigBit()
  }

  /** Add a connection into this block.
   *
   * @param arg List(src, dst),
   *            where src and dst are lists representing ports which have a connection
   */
  def addConnect(arg: List[List[String]]): Unit = {
    connectArray.append(arg)
  }

  /** Add a connection into this block.
   *
   * @param src a list representing the source port
   * @param dst a list representing the sink port
   */
  def addConnect(src: List[String], dst: List[String]): Unit = {
    addConnect(List(src, dst))
  }

  /** Update the connections when the hierarchy name of this block is changed.
   *
   * @return the new array representing connections
   */
  def updateConnect(): ArrayBuffer[List[List[String]]] = {
    val nameList = hierarchyName.toList.reverse
    val resArray = new ArrayBuffer[List[List[String]]]
    for (i <- 0 until connectArray.size) {
      val src = connectArray(i)(0)
      val dst = connectArray(i)(1)
      val resSrc = nameList.map(str => str + "/") ::: src
      val resDst = nameList.map(str => str + "/") ::: dst
      resArray.append(List(resSrc, resDst))
    }
    connectArray = resArray
    for (subBlock <- blockMap.values) {
      val ret = subBlock.updateConnect()
      ret.foreach(i => connectArray.append(i))
    }
    connectArray
  }

  /** Update the MRRG of a block with its hierarchy name.
   *
   * @param block this block
   */
  def updateMRRG(block: BlockTrait): Unit = {
    val addName = block.hierarchyName.map(i => i + ".").reverse.reduce(_ + _)
    for (module <- block.owningElements) {
      val typeID = module(0)
      val moduleID = module(1)
      val m = block.elementsArray(typeID)(moduleID)
      for (oldName <- m.mrrg.nodeMap.keys) {
        m.mrrg.update(oldName, addName + m.getName() + "." + oldName)
      }
    }
  }

  /** Translate a list of String representing a MRRG node into a String.
   *
   * @param listStr a list of String representing this MRRG node
   * @return a String representing this MRRG node
   */
  def getStrMRRG(listStr: List[String]): String = {
    val mrrgStr = ArrayBuffer[String]()
    listStr.foreach(str => mrrgStr.append(str.replaceAll("/", ".")))
    if (listStr.size == 1) {
      listStr(0)
    } else {
      if (!mrrgStr(listStr.size - 2).contains(".")) {
        mrrgStr(listStr.size - 2) = mrrgStr(listStr.size - 2).concat(".")
      }
      mrrgStr.reduce(_ + _)
    }
  }

  /** Save an MRRG as a TXT.
   *
   * @example An MRRG TXT is in the following format.
   *          "number of nodes without opcode"
   *          "<the name of an MRRG node>    "
   *          "nIn (the number of fan-ins)   "
   *          "the name of fan-in 0          "
   *          ......
   *          "the name of fan-in nIn        "
   *          "nOut (the number of fan-outs) "
   *          "the name of fan-out 0         "
   *          ......
   *          "the name of fan-out nOut      "
   *          ......
   *          "number of nodes with opcodes  "
   *          "<the name of an MRRG node>    "
   *          "nIn (the number of fan-ins)   "
   *          "the name of fan-in 0          "
   *          ......
   *          "the name of fan-in nIn        "
   *          "nOut (the number of fan-outs) "
   *          "the name of fan-out 0         "
   *          ......
   *          "the name of fan-out nOut      "
   *          "nOp (the number of opcodes)   "
   *          "opcode 0                      "
   *          ......
   *          "opcode nOp                    "
   *          ......
   * @param writer       the JAVA PrintWriter
   * @param targetedMRRG this MRRG
   */
  def dumpMRRGAsTXT(writer: PrintWriter, targetedMRRG: MRRG): Unit = {
    writer.flush()
    val noOpSet = targetedMRRG.getNoOpSet()
    val funSet = targetedMRRG.nodes.toSet &~ (noOpSet)
    writer.println(noOpSet.size)
    for (node <- noOpSet.toArray.sortBy(x => x.getName())) {
      writer.println("<" + node.getName() + ">")
      writer.println(node.fanIn.size)
      for (in <- node.fanIn) {
        writer.println(in.getName())
      }
      writer.println(node.fanOut.size)
      for (out <- node.fanOut) {
        writer.println(out.getName())
      }
    }

    writer.println(funSet.size)
    for (node <- funSet.toArray.sortBy(x => x.getName())) {
      writer.println("<" + node.getName() + ">")
      writer.println(node.fanIn.size)
      for (in <- node.fanIn) {
        writer.println(in.getName())
      }
      writer.println(node.fanOut.size)
      for (out <- node.fanOut) {
        writer.println(out.getName())
      }
      writer.println(node.ops.size)
      for (op <- node.ops) {
        writer.println(op)
      }
    }
    writer.close()
  }

  /** Initialize MRRG of this block.
   * The hierarchy name of its sub-blocks will be updated accordingly.
   * The I/O ports of this block will generate opNodes that can perform input/output operation.
   */
  def initialization(): Unit = {
    if (initialized) {
      return
    }
    initMRRG()
    val allBlocks = getAllBlocks()
    for (block <- allBlocks) {
      updateMRRG(block)
    }

    for (outPort <- outPorts) {
      val funNode = new NodeMRRG(outPort + ".fun")
      funNode.ops.append(OpEnum.OUTPUT)
      mrrg.addNode(funNode)
      mrrg.addConnect(outPort, List(funNode.getName()))
    }
    for (inPort <- inPorts) {
      val funNode = new NodeMRRG(inPort + ".fun")
      funNode.ops.append(OpEnum.INPUT)
      mrrg.addNode(funNode)
      mrrg.addConnect(funNode.getName(), List(inPort))
    }

    val addName = hierarchyName.map(i => i + ".").reverse.reduce(_ + _)
    for (oldName <- mrrg.nodeMap.keys) {
      mrrg.update(oldName, addName + oldName)
    }

    val connect = new Connect(connectArray)
    val mapRelation = connect.mapRelation
    var mapRelationMRRG = Map[String, List[String]]()
    for (src <- mapRelation.keys) {
      val srcMRRG = getStrMRRG(src)
      val dstMRRG = mapRelation(src).map(str => getStrMRRG(str))
      mapRelationMRRG = mapRelationMRRG + (srcMRRG -> dstMRRG.toList)
    }

    for (modules <- elementsArray) {
      for (module <- modules) {
        val tempMRRG = module.asInstanceOf[ElementTrait].mrrg
        mrrg.merge(tempMRRG)
      }
    }

    mapRelationMRRG.foreach(connect => mrrg.addConnect(connect._1, connect._2))
    initialized = true
  }

  /** Unroll a MRRG with II.
   * In this function,
   * the modeling consistency guarantees the latency between an I/O port
   * and an inner routing or functional unit in a module keeps consistency with
   * the corresponding latency in generated MRRG.
   *
   * @param oriMRRG the origin MRRG
   * @param II      the targeted II
   * @return unrolled MRRG
   */
  def graphUnroll(oriMRRG: MRRG, II: Int): MRRG = {
    /** Get (i + 1) mod II.
     *
     * @param i the input Int
     * @return (i + 1) mod II
     */
    def incModII(i: Int): Int = {
      (i + 1) % II
    }

    val targetedMRRG = new MRRG()
    for (i <- 0 until II) {
      val tempMRRG = oriMRRG.clone()
      for (node <- tempMRRG.nodeMap) {
        val name = node._1
        tempMRRG.update(name, i.toString + ":" + name)
        //Make sure the input operators only appear in the first reconfiguration cycle.
        if (i > 0) {
          val tempNode = tempMRRG.nodes(node._2)
          if (tempNode.ops.contains(OpEnum.INPUT)) {
            tempNode.ops.clear()
          }
        }
      }
      targetedMRRG.merge(tempMRRG)
    }
    //Add edges between copies of internal routing nodes,
    //which modeling the storage function of registers.
    var regSourceSet = Set[String]()

    for (undeterminedInConnect <- oriMRRG.undeterminedInConnects) {
      val source = undeterminedInConnect(0).getName()
      val sink = undeterminedInConnect(1).getName()
      if (II == 1) {
        targetedMRRG.addConnect("0:" + source, "0:" + sink)
      } else {
        val sinkNode = undeterminedInConnect(1)
        for (i <- 0 until II) {
          //The latency between input ports and inner nodes.
          if (sinkNode.mode == REG_MODE) {
            targetedMRRG.addConnect(i.toString + ":" + source, incModII(i).toString + ":" + sink)
          } else {
            targetedMRRG.addConnect(i.toString + ":" + source, i.toString + ":" + sink)
          }
        }
      }
    }

    for (undeterminedOutConnect <- oriMRRG.undeterminedOutConnects) {
      val source = undeterminedOutConnect(0).getName()
      val sink = undeterminedOutConnect(1).getName()
      if (II == 1) {
        targetedMRRG.addConnect("0:" + source, "0:" + sink)
      } else {
        val sourceNode = undeterminedOutConnect(0)
        for (i <- 0 until II) {
          //The latency between inner nodes and output ports.
          if (sourceNode.mode == MEM_MODE) {
            targetedMRRG.addConnect(i.toString + ":" + source, incModII(i).toString + ":" + sink)
          } else if (sourceNode.mode == NORMAL_MODE) {
            targetedMRRG.addConnect(i.toString + ":" + source, i.toString + ":" + sink)
          } else if (sourceNode.mode == REG_MODE) {
            targetedMRRG.addConnect(i.toString + ":" + source, i.toString + ":" + sink)
            regSourceSet = regSourceSet + source
          }
        }
      }
    }


    //Add edges between copies of internal routing nodes,
    //which modeling the storage function of registers.
    for (source <- regSourceSet) {
      for (i <- 0 until II) {
        targetedMRRG.addConnect(i.toString + ":" + source, incModII(i).toString + ":" + source)
      }
    }
    targetedMRRG
  }

  /** Get the MRRG of this block with II.
   *
   * @param II the targeted II
   */
  def getMRRG(II: Int): MRRG = {
    initialization()
    graphUnroll(mrrg, II)
  }

  /** Dump the MRRG of this block with II.
   *
   * @param II       the targeted II
   * @param filename the name of file for dumping
   */
  def dumpMRRG(II: Int, filename: String = null): Unit = {

    val targetedMRRG = getMRRG(II)

    var outFilename = filename
    if (filename == null) {
      outFilename = hierarchyName.map(str => str + ".").reverse.reduce(_ + _) + "mrrg.txt"
    }
    val writer = new PrintWriter(new File(outFilename))
    dumpMRRGAsTXT(writer, targetedMRRG)
  }

  /** Get all sub-blocks belonging to this block as a array.
   *
   * @return the all sub-blocks belonging to this block
   */
  def getAllSubBlocks(): ArrayBuffer[BlockTrait] = {
    val ret = ArrayBuffer[BlockTrait]()
    for (subBlock <- this.blockMap.values) {
      ret.append(subBlock)
      ret.appendAll(subBlock.getAllSubBlocks())
    }
    ret
  }

  /** Get all sub-blocks belonging to this block and itself as a array.
   *
   * @return the all sub-blocks belonging to this block
   */
  def getAllBlocks(): ArrayBuffer[BlockTrait] = {
    val ret = getAllSubBlocks()
    ret.append(this)
    ret
  }

  /** Print sub-blocks and elements of this block with PrintWriter.
   *
   * @param writer the JAVA PrintWriter
   */
  def printModules(writer: PrintWriter): Unit = {
    /** Translate a list of String into a String with tail.
     *
     * @param ori  the list of String
     * @param tail the tail
     * @return the result String
     */
    def tails(ori: List[String], tail: String): String = {
      var ret = ""
      for (i <- 0 until ori.size) {
        ret += ori(i)
        if (i != ori.size - 1) {
          ret += tail
        }
      }
      ret
    }

    writer.println("\"" + getName() + "\": {")
    val inPorts = getInPorts()
    val strInPorts = tails(inPorts.toList, " ")
    writer.print("\"in ports\": \"" + strInPorts + "\",\n")
    val outPorts = getOutPorts()
    val strOutPorts = tails(outPorts.toList, " ")
    writer.print("\"out ports\": \"" + strOutPorts + "\",\n")
    writer.print("\"config bit\": " + getConfigBit() + ",\n")
    var i = 0
    val sortedBlocks = blockMap.toList.sortBy(x => x._1)
    for (blk <- sortedBlocks) {
      i += 1
      blk._2.printModules(writer)
      if (i < blockMap.size || owningElements.size > 0) {
        writer.print(",\n")
      } else {
        writer.print("\n")
      }
    }

    for (i <- 0 until owningElements.size) {
      val typeNum = owningElements(i)(0)
      val moduleNum = owningElements(i)(1)
      val m = elementsArray(typeNum)(moduleNum).asInstanceOf[ElementTrait]
      writer.println("\"" + m.getName() + "\": {")
      val inPorts = m.getInPorts()
      val strInPorts = tails(inPorts.toList, " ")
      writer.print("\"in ports\": \"" + strInPorts + "\",\n")
      val outPorts = m.getOutPorts()
      val strOutPorts = tails(outPorts.toList, " ")
      writer.print("\"out ports\": \"" + strOutPorts + "\",\n")
      writer.print("\"config bit\": " + m.getConfigBit() + ",\n")
      val ops = m.getSupOps()
      val strOps = tails(ops.toList.map(i => i.toString()), " ")
      writer.print("\"ops\": \"" + strOps + "\"\n")
      if (i < owningElements.size - 1) {
        writer.print("},\n")
      } else {
        writer.print("}\n")
      }
    }
    writer.print("}")
  }

  /** Set this block as a reconfiguration region.
   */
  def setConfigRegion(): Unit = {
    isConfigRegion = true
  }

  /** Get a sub-block.
   *
   * @param name the name of this sub-block
   */
  def apply(name: String): BlockTrait = blockMap(name)

  /** Get an element.
   *
   * @param name the name of this element
   */
  def getElement(name: String): ElementTrait = elementsMap(name)
}
