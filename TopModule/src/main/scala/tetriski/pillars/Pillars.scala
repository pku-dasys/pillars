package tetriski.pillars

import chisel3.iotesters
import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}

//Since almost every modules has the common 'port' array, we extract
//the 'port' array and wrap it in the Trait to be extended.
trait Ports {
  var portMap = Map[String, Int]()

  //Initial ports with portMap
  def setPortMap(args: Array[String]): Map[String, Int] = {
    for (i <- 0 until args.length) {
      portMap += (args(i) -> i)
    }
    portMap
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


//The 'OpAdd' and 'OpSub' are the submodules of 'Fun'
//class OpAdd (bitwidth:Int) extends Ports {
//}
//class OpSub (bitwidth:Int) extends Ports {
//}
class Fun(name: String) extends ModuleTrait {
  //  val opAdd = new OpAdd(32)
  //  val opSub = new OpSub(32)
}

//'Mux'/'Const'/'Rf' are the submodule candidates of 'Block'
class Mux extends ModuleTrait {
}

class Const extends ModuleTrait {
}

class Rf(name: String) extends ModuleTrait {
}

//############### Currently Unused #################


class OpAdder(name: String, width: Int) extends ModuleTrait {
  //port sequnces: 0:out, 1:input_b, 2: input_a
  setPortMap(Array("out", "input_b", "input_a"))
  //Module ID 0
  setTypeID(0)

  setWidth(width)
  setName(name)
}

class OpMul(name: String, width: Int) extends ModuleTrait {
  //port sequnces: 0:out, 1:input_b, 2: input_a
  setPortMap(Array("out", "input_b", "input_a"))
  //Module ID 1
  setTypeID(1)

  setWidth(width)
  setName(name)
}


class Block(name: String) extends BlockTrait {
  setName(name)
}


trait BlockTrait extends ModuleTrait {

  //  var memPortArray = new ArrayBuffer[MemPort]
  //  var rfArray      = new ArrayBuffer[Rf]
  //  var funArray     = new ArrayBuffer[Fun]
  //  var muxArray  = new ArrayBuffer[Mux]
  //  var adderArray  = new ArrayBuffer[OpAdder]
  //  var mulArray  = new ArrayBuffer[OpMul]


  //Explicit declaration of module types
  var typeNum = 2
  var adderArray = new ArrayBuffer[Any]
  var mulArray = new ArrayBuffer[Any]
  var modulesArray = new ArrayBuffer[ArrayBuffer[Any]]

  modulesArray.append(adderArray)
  modulesArray.append(mulArray)

  // var modulesArray = List(adderArray, mulArray)

  //blockMap: name -> sub-block
  var blockMap = Map[String, Block]()
  //modulesMap: name -> corresponding module of this block
  var modulesMap = Map[String, ModuleTrait]()

  //Add high level block
  def addBlock(arg: Block): Map[String, Block] = {
    blockMap += (arg.getName() -> arg)
    //Add sub-block's realistic modules into relevant array of parent module
    for (i <- 0 until arg.modulesArray.size) {
      for (j <- 0 until arg.modulesArray(i).size) {
        modulesArray(i).append(arg.modulesArray(i)(j))
      }
    }
    blockMap
  }

  //Add a realistic module into this block's modulesArray
  def addModule(arg: ModuleTrait): Unit = {
    val typeNum = arg.getTypeID()
    modulesArray(typeNum).append(arg)
    modulesMap += (arg.getName() -> arg)
  }

  //We can use block("name") to get a sub-block
  def apply(name: String): Block = blockMap(name)

  //We can use block.getModule("name") to get a realistic module
  def getModule(name: String): ModuleTrait = modulesMap(name)
}

//This class describes the archtectures of the designed CGRA Demo
class ArchitctureHierarchy extends BlockTrait {

  setName("cgra")

  //Get integer module list.
  //In minimal case, it's [2,1], which means this CGRA contains 2 adder and 1 multiplier.
  def getModuleList(): List[Int] = {
    var ret = List[Int]()
    for (i <- 0 until modulesArray.size) {
      ret = ret :+ modulesArray(i).size
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
  }

  //Save hierarchy information as modules.json (to be defined)
  def dumpArchitcture() = {}
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

  // (to be defined)
  //"cgra/b_0/b_0_0:out" should be equal to "cgra/b_0/b_0_0:out/adder0:out"
  def simplify(): Map[List[String], ArrayBuffer[List[String]]] = {
    var ret = Map[List[String], ArrayBuffer[List[String]]]()
    var equivalence = Set[List[String]]()
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
    //mapRelation.foreach(println)
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

  //Return integer representation of a group of connection.
  //For example,
  //"cgra:input0": [
  //      "cgra/b_0:in0/b_0_0:in0/adder0:input_a",
  //      "cgra/b_0:in0/b_0_1:in0/mul0:input_a"
  //      ]
  //will be translate into Map([2, 0, 1] -> [[0, 0, 2], [1, 0 ,2]]).
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

}

object Pillars {
  def main(args: Array[String]): Unit = {

    var arch = new ArchitctureHierarchy()
    //The order shoule be same as TopModule
    arch.setPortMap(Array("output", "input1", "input0"))

    //Create the first Block
    val block_0 = new Block("b_0")
    block_0.setPortMap(Array("in0", "in1", "out"))

    val block_0_0 = new Block("b_0_0")
    block_0_0.setPortMap(Array("in0", "in1", "out"))

    val add0 = new OpAdder("adder0", 16)
    block_0_0.addModule(add0)

    val block_0_1 = new Block("b_0_1")
    block_0_1.setPortMap(Array("in0", "in1", "out"))

    val mul0 = new OpMul("mul0", 16)
    block_0_1.addModule(mul0)

    block_0.addBlock(block_0_0)
    block_0.addBlock(block_0_1)

    //Create the second Block
    val block_1 = new Block("b_1")
    block_1.setPortMap(Array("in0", "in1", "out"))

    val add1 = new OpAdder("adder0", 32)
    block_1.addModule(add1)

    arch.addBlock(block_0)
    arch.addBlock(block_1)

    arch.init()

    //   connections:
    //    {
    //      "cgra/b_0/b_0_0:out/adder0:out": [
    //      "cgra/b_0/b_0_1:in1/mul0:input_b"
    //      ],
    //      "cgra:input0": [
    //      "cgra/b_0:in0/b_0_0:in0/adder0:input_a",
    //      "cgra/b_0:in0/b_0_1:in0/mul0:input_a"
    //      ],
    //      "cgra/b_0:out/b_0_1:out/mul0:out": [
    //      "cgra/b_1:in0/adder0:input_b"
    //      ],
    //      "cgra/b_1:out/adder0:out": [
    //      "cgra:output"
    //      ],
    //      "cgra:input1": [
    //      "cgra/b_0:in1/b_0_0:in1/adder0:input_b",
    //      "cgra/b_1:in1/adder0:input_a"
    //      ]
    //    }

    //Format to be modified as  ArrayBuffer[List[List[String],List[String]]]
    val outArray = ArrayBuffer(List("cgra", "input0"),
      List("cgra", "input0"),
      List("cgra", "input1"),
      List("cgra", "input1"),
      List("cgra/", "b_0/", "b_0_0", "out", "adder0", "out"),
      List("cgra/", "b_0", "out", "b_0_1", "out", "mul0", "out"),
      List("cgra/", "b_1", "out", "adder0", "out"))
    val inArray = ArrayBuffer(List("cgra/", "b_0", "in0", "b_0_0", "in0", "adder0", "input_a"),
      List("cgra/", "b_0", "in0", "b_0_1", "in0", "mul0", "input_a"),
      List("cgra/", "b_0", "in1", "b_0_0", "in1", "adder0", "input_b"),
      List("cgra/", "b_1", "in1", "adder0", "input_b"),
      List("cgra/", "b_0/", "b_0_1", "in1", "mul0", "input_b"),
      List("cgra/", "b_1", "in0", "adder0", "input_a"),
      List("cgra", "output"))


    val connect = new Connect(outArray, inArray)
    val test = connect.getConnect()

    connect.dumpConnect()

    val cp = new HardwareGeneration(arch, connect)

    //Verilog generation
    chisel3.Driver.execute(args, () => new TopModule(cp.archList, cp.connectMap, 32))

    //Run tester
    iotesters.Driver.execute(args, () => new TopModule(cp.archList, cp.connectMap, 32)) {
      c => new TopModuleUnitTest(c)
    }

  }
}

