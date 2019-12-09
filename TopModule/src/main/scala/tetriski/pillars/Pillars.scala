package tetriski.pillars

import chisel3.iotesters

import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}

import scala.collection.mutable.Queue


//Since almost every modules has the common 'port' array, we extract
//the 'port' array and wrap it in the Trait to be extended.
trait Ports {
  var portMap = Map[String, Int]()

  //Initial ports with portMap
  def setInPortMap(args: Array[String]): Map[String, Int] = {
    for (i <- 0 until args.length) {
      portMap += (args(i) -> i)
    }
    portMap
  }

  def setOutPortMap(args: Array[String]): Map[String, Int] = {
    for (i <- 0 until args.length) {
      portMap += (args(i) -> i)
    }
    portMap
  }

  def getPorts(): Iterable[String] = {
    portMap.keys
  }

  //We can use ** operator to get a port's ID with its name
  def **(name: String) = portMap(name)
}

//ModuleInfo is basic information of a modle
trait ModuleInfo {
  var typeID = -1
  var moduleID = -1
  var width = -1
  var name = ""
  var supOps = new ArrayBuffer[String]
  var configBit = 0

  def setModuleID(arg: Int): Unit = {
    moduleID = arg
  }

  def setTypeID(arg: Int): Unit = {
    typeID = arg
  }

  def setWidth(arg: Int): Unit = {
    width = arg
  }

  def setName(arg: String): Unit = {
    name = arg
  }

  def setSupOps(arg: List[String]): Unit = {
    arg.foreach(t => supOps.append(t))
  }

  def setConfigBit(arg: Int): Unit = {
    configBit = arg
  }

  def getModuleID(): Int = {
    moduleID
  }

  def getTypeID(): Int = {
    typeID
  }

  def getName(): String = {
    name
  }

  def getWidth(): Int = {
    width
  }

  def getSupOps(): ArrayBuffer[String] = {
    supOps
  }

  def getConfigBit(): Int = {
    configBit
  }


}

//Important note:
//Each module should possess this trait, with the help of which
//we can translate a string representation of a port into a list of integer.

//For example，in our minimal case, ["cgra/b_0:out/b_0_1:out/mul0:out"] can be
//translated into [1, 0, 0], where 1 (TypeID) means this port belongs to a multiplier
//and the first 0 (ModuleID) means this multiplier has index 0 in global,
//while the second 0 (portID) means the taget port is "out".

trait ModuleTrait extends Ports with ModuleInfo {

}


//############### Currently Unused #################
//The 'MemUnit'/'MuxAddr'/'MuxData' are the submodules of the 'MemPort',
//we abstract them and put them in the MemTrait.
trait MemTrait extends ModuleTrait {
  val memUnit = new MemUnit
  val muxAddr = new MuxAddr
  val muxData = new MuxData
}

class MemUnit extends ModuleTrait {
}

class MuxAddr extends ModuleTrait {
}

class MuxData extends ModuleTrait {
}

class MemPort extends MemTrait {
}

//############### Currently Unused #################


class OpAlu(name: String, width: Int) extends ModuleTrait {
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  setOutPortMap(Array("out_0"))
  setInPortMap(Array("input_0", "input_1"))
  //Module ID 0
  setTypeID(0)
  //Support add, sub, and, or, xor
  setSupOps(List("add", "sub", "and", "or", "xor"))
  //4 bit configuration
  setConfigBit(4)

  setWidth(width)
  setName(name)
}

class OpRF1_1_2(name: String, width: Int) extends ModuleTrait {
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  setOutPortMap(Array("out_0", "out_1"))
  setInPortMap(Array("input_0"))
  //Module ID 1
  setTypeID(1)
  //Support 2 registers
  setSupOps(List("registers(2)"))
  //4 bit configuration
  setConfigBit(3)

  setWidth(width)
  setName(name)
}

class OpMux5(name: String, width: Int) extends ModuleTrait {
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  setOutPortMap(Array("out_0"))
  setInPortMap(Array("input_0", "input_1", "input_2", "input_3", "input_4"))
  //Module ID 2
  setTypeID(2)
  //Support 5 to 1 mux
  setSupOps(List("mux(5)"))
  //4 bit configuration
  setConfigBit(3)

  setWidth(width)
  setName(name)
}


class Block(name: String) extends BlockTrait {
  setName(name)
  hierName.append(name)
}


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
  //var PEArray = new ArrayBuffer[Any]
  var modulesArray = new ArrayBuffer[ArrayBuffer[Any]]
  var owningModules = new ArrayBuffer[List[Int]]

  modulesArray.append(aluArray)
  modulesArray.append(RFsArray)
  modulesArray.append(MuxsArray)
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
    val typeNum = arg.getTypeID()
    modulesArray(typeNum).append(arg)
    modulesMap += (arg.getName() -> arg)
    owningModules.append(List(typeNum, modulesArray(typeNum).size - 1))
    configBit += arg.getConfigBit()
  }

  def addConnect(arg : List[List[String]]): Unit ={
    connectArray.append(arg)
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
    val ports = getPorts()
    val strPorts = tails(ports.toList, " ")
    writer.print("\"ports\": \"" + strPorts + "\",\n")
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
      val ports = m.getPorts()
      val strPorts = tails(ports.toList, " ")
      writer.print("\"ports\": \"" + strPorts + "\",\n")
      writer.print("\"config bit\": " + m.getConfigBit() + ",\n")
      val ops = m.getSupOps()
      val strOps = tails(ops.toList, " ")
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

class PEBlock(name: String) extends BlockTrait{
  setName(name)
  hierName.append(name)
  isConfigRegion = true

  setOutPortMap(Array("out"))
  setInPortMap(Array("input_0", "input_1", "input_2", "input_3"))

  val alu0 = new OpAlu("alu0", 32)
  addModule(alu0)

  val mux0 = new OpMux5("mux0", 32)
  addModule(mux0)

  val mux1 = new OpMux5("mux1", 32)
  addModule(mux1)

  val rf0 = new OpRF1_1_2("rf0", 32)
  addModule(rf0)

  connectArray =
    ArrayBuffer(List(List("input_0"),List("mux0","input_0")),
      List(List("input_0"),List("mux1","input_0")),
      List(List("input_1"),List("mux0","input_1")),
      List(List("input_1"),List("mux1","input_1")),
      List(List("input_2"),List("mux0","input_2")),
      List(List("input_2"),List("mux1","input_2")),
      List(List("input_3"),List("mux0","input_3")),
      List(List("input_3"),List("mux1","input_3")),
      List(List("rf0", "out_0"),List("mux0","input_4")),
      List(List("rf0", "out_0"),List("mux1","input_4")),
      List(List("mux0","out_0"),List("alu0","input_0")),
      List(List("mux1","out_0"),List("alu0","input_1")),
      List(List("alu0","out_0"),List("rf0","input_0")),
      List(List("rf0","out_1"),List("out_0")))

}

//This class describes the archtectures of the designed CGRA Demo
class ArchitctureHierarchy extends BlockTrait {

  setName("cgra")
  hierName.append(name)

  //Get integer module list.
  //In minimal case, it's [2,1], which means this CGRA contains 2 adder and 1 multiplier.
  def getModuleList(): List[List[Int]] = {
    var ret = List[List[Int]]()
    var moduleNums = List[Int]()
    var moduleWidths = List[Int]()
    for (i <- 0 until modulesArray.size) {
      moduleNums = moduleNums :+ modulesArray(i).size
      for (j <- 0 until modulesArray(i).size) {
        moduleWidths = moduleWidths :+ modulesArray(i)(j).asInstanceOf[ModuleTrait].getWidth()
      }
    }
    List(moduleNums, moduleWidths)
  }

  def getConfigList(): List[List[List[Int]]] = {
    var ret = List[List[List[Int]]]()
    for (subBlock <- blockMap.values){
      var moduleList = List[List[Int]]()
      for (i <- 0 until subBlock.modulesArray.size) {
        for (j <- 0 until subBlock.modulesArray(i).size) {
          val module = subBlock.modulesArray(i)(j).asInstanceOf[ModuleTrait]
          if(module.getConfigBit()>0)
          moduleList = moduleList :+ List(module.getTypeID(), module.getModuleID())
        }
      }
      ret = ret :+ moduleList
    }
    ret
  }

  //After initialization, all module's ModuleID, also called global index,
  //is set as it's sequence number in relevant array of ArchitctureHierarchy
  def init(): Unit = {
    for (i <- 0 until modulesArray.size) {
      for (j <- 0 until modulesArray(i).size) {
        val module = modulesArray(i)(j).asInstanceOf[ModuleTrait]
        module.setModuleID(j)
      }
    }
    updateConnect()
  }

  //Save hierarchy information as modules.json
  def dumpArchitcture() = {
    val writer = new PrintWriter(new File("modules.json"))
    writer.flush()

    writer.println("{")
    printModules(writer)
    writer.println("}")

    writer.close()
  }
}


//This class is obtained from GenerateConnection and records the connection information of the two blocks mentioned above.
//The outArray and inArray individually contains the ports which have the intrinsic mapping sequences.
class Connect(outArray: ArrayBuffer[List[String]], inArray: ArrayBuffer[List[String]]) {

  var mapRelation = Map[List[String], ArrayBuffer[List[String]]]()


  for (i <- 0 until outArray.length) {
    //Connect the corresponding ports via mapping
    if (mapRelation.contains(outArray(i))) {
      mapRelation(outArray(i)).append(inArray(i))
    } else {
      mapRelation += (outArray(i) -> ArrayBuffer(inArray(i)))
    }

  }
  mapRelation = simplify()

  // (to be defined)
  //"cgra/b_0/b_0_0:out" should be equal to "cgra/b_0/b_0_0:out/adder0:out"
  def simplify(): Map[List[String], ArrayBuffer[List[String]]] = {
    var ret = Map[List[String], ArrayBuffer[List[String]]]()
    val srcs = outArray.toSet
    val dsts = inArray.toSet
    val sources = srcs&~(srcs.&(dsts))
    val sinks = dsts&~(srcs.&(dsts))
    println(sources)
    for (src <- sources){
      //BFS
      var targets = ArrayBuffer[List[String]]()
      var queue = Queue[List[String]]()
      mapRelation(src).map(i => queue.enqueue(i))
      while (!queue.isEmpty){
        val temp = queue.dequeue()
        if(sinks(temp)){
          targets.append(temp)
        }else{
          mapRelation(temp).map(i => queue.enqueue(i))
        }
      }
      ret += (src -> targets)
    }
    ret
  }

  //Get connect relations
  def getConnect(): Map[List[String], ArrayBuffer[List[String]]] = {
    mapRelation
  }

  //Save connection information as connect.json
  def dumpConnect() = {
    val writer = new PrintWriter(new File("connect.json"))

    writer.flush()

    def printConnect(src: List[String], dsts: ArrayBuffer[List[String]], comma: Boolean): Unit = {
      def printPort(port: List[String]): Unit = {
        var ii = 0
        for (i <- 0 until port.size) {
          var str = port(i)
          var label = ""
          if (ii % 2 != 0) label = "/"
          else label = ":"
          if (str(str.size - 1) == '/') writer.print(str)
          else {
            if (i != port.size - 1) writer.print(str + label)
            else writer.print(str)
            ii += 1
          }
        }
      }

      writer.print("\"")
      printPort(src)
      writer.print("\":\n[")
      for (i <- 0 until dsts.size) {
        writer.print("\"")
        printPort(dsts(i))
        if (i != dsts.size - 1) writer.print("\",\n")
        else writer.print("\"")
      }
      if (comma) writer.print("],\n")
      else writer.print("]\n")
    }

    writer.print("{")
    mapRelation.foreach((x) => printConnect(x._1, x._2, !x.equals(mapRelation.last)))
    writer.print("}")
    writer.close()
  }


}


////This class combines the ArchitctureHierarchy class and Connect class to help TopModule generate final
////hardware modules with linking information.
class HardwareGeneration(arch: BlockTrait, connect: Connect) {
  var connectMap = Map[List[Int], List[List[Int]]]()
  var mapRelation = connect.mapRelation

  def getConnectList(src: List[String], dsts: ArrayBuffer[List[String]]): Map[List[Int], List[List[Int]]] = {
    //Encode a string representation of a port into a list of integer as mentioned before
    def encode(strs: List[String]): List[Int] = {
      var ret = List[Int]()
      if (strs.size == 2) {
        return List(arch.typeNum, 0, arch ** strs(1))
      }

      var temp = arch
      var ii = 0
      for (i <- 0 until strs.size) {
        if (strs(i)(strs(i).size - 1) == '/') {
          if (strs(i) != "cgra/")
            temp = temp(strs(i).substring(0, strs(i).size - 1))
        } else if (i == strs.size - 2) {
          var target = temp.getModule(strs(i))
          return List(target.getTypeID(), target.getModuleID(), target ** strs(strs.size - 1))
        } else if (ii % 2 == 0) {
          temp = temp(strs(i))
          ii += 1
        } else {
          ii += 1
        }

      }
      ret
    }

    val encodeSrc = encode(src)
    val encodeDsts = dsts.map(encode).toList

    Map(encodeSrc -> encodeDsts)
  }

  //Return integer representation of connection, which is needed in TopModule
  var connectList = mapRelation.map((x) => getConnectList(x._1, x._2)).foreach((x) => connectMap = connectMap.++(x))

  val archList = arch.asInstanceOf[ArchitctureHierarchy].getModuleList()

  val configList = arch.asInstanceOf[ArchitctureHierarchy].getConfigList()

}

object Pillars {
  def main(args: Array[String]): Unit = {

    var arch = new ArchitctureHierarchy()
    //The order of ports should be same as TopModule
    arch.setOutPortMap(Array("output"))
    arch.setInPortMap(Array("input_0", "input_1"))

    val pe0 = new PEBlock("pe0")
    val pe1 = new PEBlock("pe1")

    arch.addBlock(pe0)
    arch.addBlock(pe1)

    arch.addConnect(List(List("input_0"),List("pe0/", "input_0")))
    arch.addConnect(List(List("input_0"),List("pe0/", "input_1")))
    arch.addConnect(List(List("input_1"),List("pe0/", "input_2")))
    arch.addConnect(List(List("input_1"),List("pe1/", "input_0")))
    arch.addConnect(List(List("input_1"),List("pe1/", "input_1")))
    arch.addConnect(List(List("input_0"),List("pe1/", "input_2")))
    arch.addConnect(List(List("pe1/","out_0"),List("pe0/", "input_3")))
    arch.addConnect(List(List("pe0/","out_0"),List("pe1/", "input_3")))
    arch.addConnect(List(List("pe0/","out_0"),List("output")))

    arch.init()

    arch.dumpArchitcture()

    val connectArray = arch.connectArray


    val outArray = ArrayBuffer[List[String]]()
    val inArray = ArrayBuffer[List[String]]()
    connectArray.foreach(t => outArray.append(t(0)))
    connectArray.foreach(t => inArray.append(t(1)))

    val connect = new Connect(outArray, inArray)
    //val test = connect.getConnect()

    connect.dumpConnect()

    val cp = new HardwareGeneration(arch, connect)

    //println(cp.connectMap)

    //Verilog generation
    chisel3.Driver.execute(args, () => new TopModule(cp.archList, cp.connectMap, cp.configList, 32))

    //Run tester
    iotesters.Driver.execute(args, () => new TopModule(cp.archList, cp.connectMap, cp.configList, 32)) {
      c => new TopModulePEUnitTest(c)
    }



  }
}


