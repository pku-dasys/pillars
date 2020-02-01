package tetriski.pillars.core

import java.io.{File, PrintWriter}

import firrtl.annotations.Target.NamedException
import MRRGMode._
import scala.collection.mutable.ArrayBuffer

trait BlockTrait extends ModuleTrait {

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
  var modulesArray = new ArrayBuffer[ArrayBuffer[Any]]
  var owningModules = new ArrayBuffer[List[Int]]

  modulesArray.append(aluArray)
  modulesArray.append(RFsArray)
  modulesArray.append(MuxsArray)
  modulesArray.append(ConstsArray)
  modulesArray.append(LSUsArray)
  val typeNum = modulesArray.size

  var hierName = new ArrayBuffer[String]
  var connectArray = new ArrayBuffer[List[List[String]]]

  //blockMap: name -> sub-block
  var blockMap = Map[String, BlockTrait]()
  //modulesMap: name -> corresponding module of this block
  var modulesMap = Map[String, ModuleTrait]()

  def updateHierName(arg : ArrayBuffer[String]): Unit ={
    for(str <- arg){
      hierName.append(str)
    }
    for (subBlocks <- blockMap.values){
      subBlocks.updateHierName(arg)
    }
  }

  //Add high level block
  def addBlock(arg: BlockTrait): Map[String, BlockTrait] = {
    blockMap += (arg.getName() -> arg)
    //Add sub-block's realistic modules into relevant array of parent module
    for (i <- 0 until arg.modulesArray.size) {
      for (j <- 0 until arg.modulesArray(i).size) {
        modulesArray(i).append(arg.modulesArray(i)(j))
      }
    }
    arg.updateHierName(hierName)
    configBit += arg.getConfigBit()
    blockMap
  }

  //Add a realistic module into this block's modulesArray
  def addModule(arg: ModuleTrait): Unit = {
    arg.initMRRG()
    val typeNum = arg.getTypeID()
    modulesArray(typeNum).append(arg)
    modulesMap += (arg.getName() -> arg)
    owningModules.append(List(typeNum, modulesArray(typeNum).size - 1))
    configBit += arg.getConfigBit()
  }

  def addConnect(arg : List[List[String]]): Unit ={
    connectArray.append(arg)
  }

  def addConnect(src : List[String], dst : List[String]) : Unit ={
    addConnect(List(src, dst))
  }

  def updateConnect(): ArrayBuffer[List[List[String]]] ={
    val nameList = hierName.toList.reverse
    var resArray = new ArrayBuffer[List[List[String]]]
    for (i <- 0 until connectArray.size){
      val src = connectArray(i)(0)
      val dst = connectArray(i)(1)
      val resSrc = nameList.map( str => str+"/") ::: src
      val resDst = nameList.map( str => str+"/") ::: dst
      resArray.append(List(resSrc, resDst).asInstanceOf[List[List[String]]])
    }
    connectArray = resArray
    //println("connectArray", connectArray)
    for(subBlock <- blockMap.values){
      val ret = subBlock.updateConnect()
      ret.foreach(i => connectArray.append(i))
    }
    connectArray
  }

  def dumpMRRG(II: Int): Unit ={
    def updateMRRG(block: BlockTrait): Unit ={
      val addName = block.hierName.map(i => i+".").reverse.reduce(_+_)
      for(module <- block.owningModules){
        val typeID = module(0)
        val moduleID = module(1)
        val m = block.modulesArray(typeID)(moduleID).asInstanceOf[ModuleTrait]
        for(oldName <- m.mrrg.nodeMap.keys){
          m.mrrg.update(oldName, addName + m.getName() + "." + oldName)
        }
      }
    }
    def getStrMRRG(listStr: List[String]) : String= {
      val mrrgStr = ArrayBuffer[String]()
      listStr.foreach(str => mrrgStr.append(str.replaceAll("/", ".")))
      if(!mrrgStr(listStr.size-2).contains(".")){
        mrrgStr(listStr.size-2) = mrrgStr(listStr.size-2).concat(".")
      }
      mrrgStr.reduce(_+_)
    }
    def dumpAsTXT(writer: PrintWriter, targetMRRG: MRRG): Unit ={
      writer.flush()
      val noOpSet = targetMRRG.getNoOpSet()
      val funSet = targetMRRG.nodes.toSet&~(noOpSet)
      writer.println(noOpSet.size)
      for(node <- noOpSet){
        writer.println("<"+node.getName()+">")
        writer.println(node.fanIn.size)
        for(in <- node.fanIn){
          writer.println(in.getName())
        }
        writer.println(node.fanOut.size)
        for(out <- node.fanOut){
          writer.println(out.getName())
        }
      }

      writer.println(funSet.size)
      for(node <- funSet){
        writer.println("<"+node.getName()+">")
        writer.println(node.fanIn.size)
        for(in <- node.fanIn){
          writer.println(in.getName())
        }
        writer.println(node.fanOut.size)
        for(out <- node.fanOut){
          writer.println(out.getName())
        }
        writer.println(node.ops.size)
        for(op <- node.ops){
          writer.println(op)
        }
      }
      writer.close()
    }
    def initialization(): Unit ={
      initMRRG()
      val allBlocks = getAllBlocks()
      for(block <- allBlocks){
        updateMRRG(block)
      }

      for(outPort <- outPorts){
        val funNode = new NodeMRRG(outPort + ".fun")
        funNode.ops.append(OpEnum.OUTPUT)
        mrrg.addNode(funNode)
        mrrg.addConnect(outPort, List(funNode.getName()))
      }
      for(inPort <- inPorts){
        val funNode = new NodeMRRG(inPort + ".fun")
        funNode.ops.append(OpEnum.INPUT)
        mrrg.addNode(funNode)
        mrrg.addConnect(funNode.getName(), List(inPort))
      }

      val addName = hierName.map(i => i+".").reverse.reduce(_+_)
      for(oldName <- mrrg.nodeMap.keys){
        mrrg.update(oldName, addName  + oldName)
      }

      val connect = new Connect(connectArray)
      val mapRelation = connect.mapRelation
      var mapRelationMRRG = Map[String, List[String]]()
      for(src <- mapRelation.keys) {
        val srcMRRG = getStrMRRG(src)
        val dstMRRG = mapRelation(src).map(str => getStrMRRG(str))
        mapRelationMRRG = mapRelationMRRG + (srcMRRG -> dstMRRG.toList)
      }

      for(modules <- modulesArray){
        for(module <- modules){
          val tempMRRG = module.asInstanceOf[ModuleTrait].mrrg
          mrrg.mergy(tempMRRG)
        }
      }

      mapRelationMRRG.foreach( connect => mrrg.addConnect(connect._1, connect._2))
    }
    def graphUnroll(oriMRRG: MRRG, II: Int): MRRG ={
      def incModII(i: Int): Int = {
        (i + 1) % II
      }
      var targetMRRG = new MRRG()
      for(i <- 0 until II){
        var tempMRRG = oriMRRG.clone()
        for(node <- tempMRRG.nodeMap){
          val name = node._1
          tempMRRG.update(name, i.toString + ":" + name)
        }
        targetMRRG.mergy(tempMRRG)
      }
      var regSourceSet = Set[String]()
      for(undeterminedConnect <- oriMRRG.undeterminedConnects){
        val source = undeterminedConnect(0).getName()
        val sink = undeterminedConnect(1).getName()
        if(II == 1){
          targetMRRG.addConnect("0:" + source, "0:" + sink)
        }else{
          val sourceNode = undeterminedConnect(0)
          for(i <- 0 until II){
            if(sourceNode.mode == MEM_MODE){
              targetMRRG.addConnect(i.toString + ":" + source, incModII(i).toString + ":" + sink)
            }else if(sourceNode.mode == NORMAL_MODE){
              targetMRRG.addConnect(i.toString + ":" + source, i.toString + ":" + sink)
            }else if(sourceNode.mode == REG_MODE){
              targetMRRG.addConnect(i.toString + ":" + source, i.toString + ":" + sink)
              regSourceSet = regSourceSet + source
            }
          }
        }
      }
      for(source <- regSourceSet){
        for(i <- 0 until II) {
          targetMRRG.addConnect(i.toString + ":" + source, incModII(i).toString + ":" + source)
        }
      }
      targetMRRG
    }

    initialization()
    val targetMRRG = graphUnroll(mrrg, II)

    val writer = new PrintWriter(new File( hierName.map(str => str + ".").reverse.reduce(_+_) + "mrrg.txt"))
    dumpAsTXT(writer, targetMRRG)
  }

  def getAllSubBlocks() : ArrayBuffer[BlockTrait] = {
    val ret = ArrayBuffer[BlockTrait]()
    for(subBlock <- this.blockMap.values){
      ret.append(subBlock)
      ret.appendAll(subBlock.getAllSubBlocks())
    }
    ret
  }

  def getAllBlocks() : ArrayBuffer[BlockTrait] = {
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
    for (blk <- blockMap.values) {
      i += 1
      blk.printModules(writer)
      if (i < blockMap.size || owningModules.size > 0) {
        writer.print(",\n")
      } else {
        writer.print("\n")
      }
    }

    for (i <- 0 until owningModules.size) {
      val typeNum = owningModules(i)(0)
      val moduleNum = owningModules(i)(1)
      val m = modulesArray(typeNum)(moduleNum).asInstanceOf[ModuleTrait]
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
      if (i < owningModules.size - 1) {
        writer.print("},\n")
      } else {
        writer.print("}\n")
      }
    }
    writer.print("}")
  }

  def setConfigRegion(): Unit ={
    isConfigRegion = true
  }

  //We can use block("name") to get a sub-block
  def apply(name: String): BlockTrait = blockMap(name)

  //We can use block.getModule("name") to get a realistic module
  def getModule(name: String): ModuleTrait = modulesMap(name)
}
