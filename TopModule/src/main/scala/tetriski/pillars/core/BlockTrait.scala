package tetriski.pillars.core

import java.io.{File, PrintWriter}

import firrtl.annotations.Target.NamedException
import MRRGMode._
import scala.collection.mutable.ArrayBuffer

trait BlockTrait extends ElementTrait {

  //  var memPortArray = new ArrayBuffer[MemPort]
  //  var rfArray      = new ArrayBuffer[Rf]
  //  var funArray     = new ArrayBuffer[Fun]
  //  var muxArray  = new ArrayBuffer[Mux]
  //  var adderArray  = new ArrayBuffer[OpAdder]
  //  var mulArray  = new ArrayBuffer[OpMul]

  var isConfigRegion = false


  //Explicit declaration of module types
  //  var adderArray = new ArrayBuffer[Any]
  //  var mulArray = new ArrayBuffer[Any]
  var aluArray = new ArrayBuffer[Any]
  var RFsArray = new ArrayBuffer[Any]
  var MuxsArray = new ArrayBuffer[Any]
  var ConstsArray = new ArrayBuffer[Any]
  var LSUsArray = new ArrayBuffer[Any]
  //var PEArray = new ArrayBuffer[Any]
  var elementsArray = new ArrayBuffer[ArrayBuffer[Any]]
  var owningElements = new ArrayBuffer[List[Int]]

  elementsArray.append(aluArray)
  elementsArray.append(RFsArray)
  elementsArray.append(MuxsArray)
  elementsArray.append(ConstsArray)
  elementsArray.append(LSUsArray)
  val typeNum = elementsArray.size

  var hierName = new ArrayBuffer[String]
  var connectArray = new ArrayBuffer[List[List[String]]]

  //blockMap: name -> sub-block
  var blockMap = Map[String, BlockTrait]()
  //modulesMap: name -> corresponding module of this block
  var elementsMap = Map[String, ElementTrait]()

  override def /(portName: String): ValidPort = {
    if (!(getInPorts().toSet.contains(portName) || getOutPorts().toSet.contains(portName))) {
      System.err.println(s"Invalid port name $name / $portName!")
    }
    new ValidPort(name + "/", portName)
  }

  def term(portName: String): ValidPort = {
    if (!(getInPorts().toSet.contains(portName) || getOutPorts().toSet.contains(portName))) {
      System.err.println(s"Invalid port name $portName!")
    }
    new ValidPort(null, portName)
  }

  def updateHierName(arg: ArrayBuffer[String]): Unit = {
    for (str <- arg) {
      hierName.append(str)
    }
    for (subBlocks <- blockMap.values) {
      subBlocks.updateHierName(arg)
    }
  }

  //Add high level block
  def addBlock(arg: BlockTrait): Map[String, BlockTrait] = {
    blockMap += (arg.getName() -> arg)
    //Add sub-block's realistic modules into relevant array of parent module
    for (i <- 0 until arg.elementsArray.size) {
      for (j <- 0 until arg.elementsArray(i).size) {
        elementsArray(i).append(arg.elementsArray(i)(j))
      }
    }
    arg.updateHierName(hierName)
    configBit += arg.getConfigBit()
    blockMap
  }

  //Add a realistic module into this block's modulesArray
  def addElement(arg: ElementTrait): Unit = {
    arg.initMRRG()
    val typeNum = arg.getTypeID()
    elementsArray(typeNum).append(arg)
    elementsMap += (arg.getName() -> arg)
    owningElements.append(List(typeNum, elementsArray(typeNum).size - 1))
    configBit += arg.getConfigBit()
  }

  def addConnect(arg: List[List[String]]): Unit = {
    connectArray.append(arg)
  }

  def addConnect(src: List[String], dst: List[String]): Unit = {
    addConnect(List(src, dst))
  }

  def updateConnect(): ArrayBuffer[List[List[String]]] = {
    val nameList = hierName.toList.reverse
    var resArray = new ArrayBuffer[List[List[String]]]
    for (i <- 0 until connectArray.size) {
      val src = connectArray(i)(0)
      val dst = connectArray(i)(1)
      val resSrc = nameList.map(str => str + "/") ::: src
      val resDst = nameList.map(str => str + "/") ::: dst
      resArray.append(List(resSrc, resDst).asInstanceOf[List[List[String]]])
    }
    connectArray = resArray
    //println("connectArray", connectArray)
    for (subBlock <- blockMap.values) {
      val ret = subBlock.updateConnect()
      ret.foreach(i => connectArray.append(i))
    }
    connectArray
  }

  def dumpMRRG(II: Int, filename: String = null): Unit = {
    def updateMRRG(block: BlockTrait): Unit = {
      val addName = block.hierName.map(i => i + ".").reverse.reduce(_ + _)
      for (module <- block.owningElements) {
        val typeID = module(0)
        val moduleID = module(1)
        val m = block.elementsArray(typeID)(moduleID).asInstanceOf[ElementTrait]
        for (oldName <- m.mrrg.nodeMap.keys) {
          m.mrrg.update(oldName, addName + m.getName() + "." + oldName)
        }
      }
    }

    def getStrMRRG(listStr: List[String]): String = {
      val mrrgStr = ArrayBuffer[String]()
      listStr.foreach(str => mrrgStr.append(str.replaceAll("/", ".")))
      if (listStr.size == 1) {
        listStr(0)
      }else {
        if (!mrrgStr(listStr.size - 2).contains(".")) {
          mrrgStr(listStr.size - 2) = mrrgStr(listStr.size - 2).concat(".")
        }
        mrrgStr.reduce(_ + _)
      }
    }

    def dumpAsTXT(writer: PrintWriter, targetMRRG: MRRG): Unit = {
      writer.flush()
      val noOpSet = targetMRRG.getNoOpSet()
      val funSet = targetMRRG.nodes.toSet &~ (noOpSet)
      writer.println(noOpSet.size)
      for (node <- noOpSet) {
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
      for (node <- funSet) {
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

    def initialization(): Unit = {
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

      val addName = hierName.map(i => i + ".").reverse.reduce(_ + _)
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
          mrrg.mergy(tempMRRG)
        }
      }

      mapRelationMRRG.foreach(connect => mrrg.addConnect(connect._1, connect._2))
    }

    def graphUnroll(oriMRRG: MRRG, II: Int): MRRG = {
      def incModII(i: Int): Int = {
        (i + 1) % II
      }

      var targetMRRG = new MRRG()
      for (i <- 0 until II) {
        var tempMRRG = oriMRRG.clone()
        for (node <- tempMRRG.nodeMap) {
          val name = node._1
          tempMRRG.update(name, i.toString + ":" + name)
        }
        targetMRRG.mergy(tempMRRG)
      }
      var regSourceSet = Set[String]()

      for (undeterminedInConnect <- oriMRRG.undeterminedInConnects) {
        val source = undeterminedInConnect(0).getName()
        val sink = undeterminedInConnect(1).getName()
        if (II == 1) {
          targetMRRG.addConnect("0:" + source, "0:" + sink)
        } else {
          val sinkNode = undeterminedInConnect(1)
          for (i <- 0 until II) {
            if (sinkNode.mode == REG_MODE) {
              targetMRRG.addConnect(i.toString + ":" + source, incModII(i).toString + ":" + sink)
            } else {
              targetMRRG.addConnect(i.toString + ":" + source, i.toString + ":" + sink)
            }
          }
        }
      }

      for (undeterminedOutConnect <- oriMRRG.undeterminedOutConnects) {
        val source = undeterminedOutConnect(0).getName()
        val sink = undeterminedOutConnect(1).getName()
        if (II == 1) {
          targetMRRG.addConnect("0:" + source, "0:" + sink)
        } else {
          val sourceNode = undeterminedOutConnect(0)
          for (i <- 0 until II) {
            if (sourceNode.mode == MEM_MODE) {
              targetMRRG.addConnect(i.toString + ":" + source, incModII(i).toString + ":" + sink)
            } else if (sourceNode.mode == NORMAL_MODE) {
              targetMRRG.addConnect(i.toString + ":" + source, i.toString + ":" + sink)
            } else if (sourceNode.mode == REG_MODE) {
              targetMRRG.addConnect(i.toString + ":" + source, i.toString + ":" + sink)
              regSourceSet = regSourceSet + source
            }
          }
        }
      }


      for (source <- regSourceSet) {
        for (i <- 0 until II) {
          targetMRRG.addConnect(i.toString + ":" + source, incModII(i).toString + ":" + source)
        }
      }
      targetMRRG
    }

    initialization()
    val targetMRRG = graphUnroll(mrrg, II)

    var outFilename = filename
    if (filename == null) {
      outFilename = hierName.map(str => str + ".").reverse.reduce(_ + _) + "mrrg.txt"
    }
    val writer = new PrintWriter(new File(outFilename))
    dumpAsTXT(writer, targetMRRG)
  }

  def getAllSubBlocks(): ArrayBuffer[BlockTrait] = {
    val ret = ArrayBuffer[BlockTrait]()
    for (subBlock <- this.blockMap.values) {
      ret.append(subBlock)
      ret.appendAll(subBlock.getAllSubBlocks())
    }
    ret
  }

  def getAllBlocks(): ArrayBuffer[BlockTrait] = {
    val ret = getAllSubBlocks()
    ret.append(this)
    ret
  }

  //print sub-blocks and modules
  def printModules(writer: PrintWriter): Unit = {
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

  def setConfigRegion(): Unit = {
    isConfigRegion = true
  }

  //We can use block("name") to get a sub-block
  def apply(name: String): BlockTrait = blockMap(name)

  //We can use block.getElement("name") to get a realistic module
  def getElement(name: String): ElementTrait = elementsMap(name)
}
