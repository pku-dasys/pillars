package tetriski.pillars

import java.util.Date

import chisel3.iotesters

import scala.collection.mutable.ArrayBuffer



//Since almost every modules has the common 'port' array, we extract
//the 'port' array and wrap it in the Trait to be extended.
trait Ports {
  var portMap = Map[String, Int]()

  def setPortMap(args:Array[String]) : Map[String, Int] = {
    for (i <- 0 until args.length) {
      portMap += (args(i) -> i)
    }
    portMap
  }

  def **(name : String ) = portMap(name)
}

trait ModuleInfo{
  var ModuleID = -1
  var TypeID = -1
  var name = ""
  def setModuleID(arg : Int): Unit ={
    ModuleID = arg
  }
  def setTypeID(arg : Int): Unit ={
    TypeID = arg
  }
  def setName(arg : String): Unit ={
    name = arg
  }
  def getModuleID(): Int ={
    ModuleID
  }
  def getTypeID(): Int ={
    TypeID
  }
  def getName(): String ={
    name
  }
}

trait ModuleTrait extends Ports with ModuleInfo{

}


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
class Fun(name : String) extends ModuleTrait {
//  val opAdd = new OpAdd(32)
//  val opSub = new OpSub(32)
}



class OpAdder(name : String) extends ModuleTrait {
  //port sequnces: 0:out, 1:input_b, 2: input_a
  setPortMap(Array("out", "input_b", "input_a"))
  //Module ID 0
  setTypeID(0)

  setName(name)
}
class OpMul(name : String) extends ModuleTrait{
  //port sequnces: 0:out, 1:input_b, 2: input_a
  setPortMap(Array("out", "input_b", "input_a"))
  //Module ID 1
  setTypeID(1)

  setName(name)
}

//'Mux'/'Const'/'Rf' are the submodule candidates of 'Block'
class Mux extends ModuleTrait {
}
class Const extends ModuleTrait {
}
class Rf(name : String)  extends ModuleTrait {
}
class Block(name : String) extends BlockTrait {
  setName(name)
}
trait BlockTrait extends ModuleTrait {
//  var memPortArray = new ArrayBuffer[MemPort]
//  var rfArray      = new ArrayBuffer[Rf]
//  var funArray     = new ArrayBuffer[Fun]
//  var muxArray  = new ArrayBuffer[Mux]
//  var adderArray  = new ArrayBuffer[OpAdder]
//  var mulArray  = new ArrayBuffer[OpMul]

//  var modulesArray = new ArrayBuffer[ArrayBuffer[Any]]
//
  var typeNum = 2
  var adderArray  = new ArrayBuffer[Any]
  var mulArray  = new ArrayBuffer[Any]
//
//  modulesArray.append(adderArray)
//  modulesArray.append(mulArray)

  var modulesArray = List(adderArray, mulArray)

  var blockMap = Map[String, Block]()
  var modulesMap = Map[String, ModuleTrait]()

  //Add high level block
  def addBlock(arg: Block): Map[String, Block] = {
    blockMap += (arg.getName() -> arg)
    //println(arg.getName())
    for (i <- 0 until arg.modulesArray.size){
      for (j <- 0 until arg.modulesArray(i).size){
        modulesArray(i).append(arg.modulesArray(i)(j))
      }
    }
    blockMap
  }

  def addModule(typeNum : Int, arg : ModuleTrait): Unit ={
    modulesArray(typeNum).append(arg)
    modulesMap += (arg.getName() -> arg)
  }

  def apply(name : String): Block = blockMap(name)
  def getModule(name : String) : ModuleTrait = modulesMap(name)
}

//This class describes the archtectures of the designed CGRA Demo
class ArchitctureHierarchy extends BlockTrait{

  setName("cgra")

  def getModuleList(): List[Int] ={
    var ret = List[Int]()
    for (i <- 0 until modulesArray.size){
      ret = ret :+ modulesArray(i).size
    }
    ret
  }

  def init(): Unit ={
    for( i <- 0 until modulesArray.size){
      for( j <- 0 until modulesArray(i).size){
        val module = modulesArray(i)(j).asInstanceOf[ModuleTrait]
        module.setModuleID(j)
      }
    }
  }

  //Save hierarchy information as modules.json
  def dumpArchitcture() = {}
}


//This class is obtained from GenerateConnection and records the connection information of the two blocks mentioned above.
//The outArray and inArray individually contains the ports which have the intrinsic mapping sequences.
class Connect (outArray: ArrayBuffer[List[String]], inArray: ArrayBuffer[List[String]]) {

var mapRelation = Map[List[String], ArrayBuffer[List[String]]]()



for (i <- 0 until outArray.length) {
//Connect the corresponding ports via mapping
  if(mapRelation.contains(outArray(i))){
    mapRelation(outArray(i)).append(inArray(i))
  }else{
    mapRelation += ( outArray(i) -> ArrayBuffer(inArray(i)) )
  }

}

// to be defined
  def simplify(): Map[List[String], ArrayBuffer[List[String]]] ={
    var ret = Map[List[String], ArrayBuffer[List[String]]]()
    var equivalence = Set[List[String]]()
    ret
  }

//Get connect relations
def getConnect() : Map[List[String], ArrayBuffer[List[String]]] = {
mapRelation
}

//Save connection information as connect.txt
def dumpConnect() = {

  def printConnect(src :List[String], dsts: ArrayBuffer[List[String]], comma : Boolean): Unit ={
    def printPort(port :List[String]): Unit ={
      var ii = 0
      for (i <- 0 until port.size){
        var str = port(i)
        var label = ""
        if(ii % 2 != 0) label = "/"
        else label = ":"
        if(str(str.size-1)== '/') print(str)
        else {
          if (i != port.size-1) print(str + label)
          else print(str)
          ii += 1
        }
      }
    }

    print("\"")
    printPort(src)
    print("\":\n[")
    for (i <- 0 until dsts.size){
      print("\"")
      printPort(dsts(i))
      if (i != dsts.size-1)print("\",\n")
      else print("\"")
    }
    if (comma) print("],\n")
    else print("]\n")
  }
  //mapRelation.foreach(println)
  print("{")
  mapRelation.foreach((x) => printConnect(x._1, x._2, !x.equals(mapRelation.last)))
  print("}")
}

}



class HardwareGeneration(arch: BlockTrait, connect: Connect){
  var connectMap = Map[List[Int] , List[List[Int]]]()
  var mapRelation = connect.mapRelation

  def getConnctList(src :List[String], dsts: ArrayBuffer[List[String]]): Map[List[Int],List[List[Int]]] = {
    def encode(strs : List[String]) : List[Int] = {
    var ret = List[Int]()
      if(strs.size == 2){
        return List (arch.typeNum, 0, arch ** strs(1))
      }

      var temp = arch
      var ii = 0
      for (i <- 0 until strs.size){
        //println(strs(i)(strs(i).size-1))
        if(strs(i)(strs(i).size-1)=='/' ) {
          if(strs(i) != "cgra/")
          temp = temp(strs(i).substring(0, strs(i).size-1))
          //println(temp.getName())
        }else if(i == strs.size - 2){
          var target = temp.getModule(strs(i))
          return List (target.getTypeID(), target.getModuleID(), target ** strs(strs.size - 1))
          //println(target.getName())
        }else if(ii % 2 == 0 ){
          temp = temp(strs(i))
          //println(temp.getName())
          ii += 1
        }else {
          ii += 1
        }

      }
    ret
    }

    val encodeSrc  = encode(src)
    val encodeDsts = dsts.map(encode).toList

   // println(List (encodeSrc, encodeDsts))
    Map (encodeSrc-> encodeDsts)
  }
  var connectList = mapRelation.map((x) => getConnctList(x._1, x._2)).foreach((x) => connectMap = connectMap.++(x))

  val archList = arch.asInstanceOf[ArchitctureHierarchy].getModuleList()

}
////This class combines the ArchitctureHierarchy class and Connect class to generate final
////hardware modules with linking information.
//class HardwareGeneration (arch: ArchitctureHierarchy, connect: Connect) {
//
//  //The ArchitectureParse(to be defined) class can generate the pure hardware modules according to
//  //the ArchitctureHierarchy class
//  val archParser = new ArchitectureParse(arch)
//  val pureHardModules = archParser.generateModule()
//
//  //The ConnectionParser(to be defined) class can link the relative ports according to
//  //the Connect class
//  val connectParser = new ConnectionParser(connect)
//  val hardwareModules = connectParser.link(pureHardModules)
//
//}


object Pillars{
  def main(args: Array[String]): Unit = {

    var arch = new ArchitctureHierarchy()
    arch.setPortMap(Array("output", "input1", "input0"))

    //Create the first Block
//    val block_one = new Block("b_rf1")
//
//    block_one.setPortArray(Array("in0" ,"out0", "out1"))
//    val rf = new Rf("rf")
//    rf.setPortArray(Array("WE0", "address_in0", "address_out0", "address_out1", "in0", "out0", "out1"))
//    block_one.rfArray.append(rf)

    val block_0 = new Block("b_0")
    block_0.setPortMap(Array("in0" ,"in1", "out"))

    val block_0_0 = new Block("b_0_0")
    block_0_0.setPortMap(Array("in0" ,"in1", "out"))

    val add0 = new OpAdder("adder0")
    block_0_0.addModule(add0.getTypeID(), add0)

    val block_0_1 = new Block("b_0_1")
    block_0_1.setPortMap(Array("in0" ,"in1", "out"))

    val mul0 = new OpMul("mul0")
    block_0_1.addModule(mul0.getTypeID(), mul0)

    block_0.addBlock(block_0_0)
    block_0.addBlock(block_0_1)



    //Create the second Block
//    val block_two = new Block("b_fun1")
//
//    block_two.setPortArray(Array("in0" ,"in1", "out0"))
//    val fun = new Fun("fun")
//    fun.setPortArray(Array("in_a", "in_b", "out", "select"))
//    block_two.funArray.append(fun)

    val block_1 = new Block("b_1")
    block_1.setPortMap(Array("in0" ,"in1", "out"))

    val add1 = new OpAdder("adder0")
    block_1.addModule(add1.getTypeID(), add1)


    //modules: (config units unimplemented)
    // {
    //   "cgra":{
    //     "ports": "input0 input1 output",
    //     "b_rf1":{
    //       "ports": "in0 out0 out1",
    //       "rf":{
    //         "ports": "WE0 address_in0 address_out0 address_out1 in0 out0 out1"
    //       }
    //     },
    //     "b_fun1":{
    //       "ports": "in0 in1 out0",
    //       "fun":{
    //         "ports": "in_a in_b out select",
    //         "ops": "add sub"
    //       }
    //     }
    //   }
    // }
    arch.addBlock(block_0)
    arch.addBlock(block_1)

    arch.init()

    //println(arch.getModuleList())
    //println(arch("b_1").getName())

    // connections:
    // cgra:input0 -> cgra/b_rf1:in0/rf:in0
    // {cgra/}b_rf1:out0/rf:address_out0 -> b_fun1:in0/fun:in_a
    // {cgra/}b_rf1:out0/rf:address_out0 -> b_fun1:in1/fun:in_b
    // cgra/b_fun1:out0/fun:out -> cgra:input1
//    val outArray = ArrayBuffer(List("cgra", "input0"),
//      List("cgra/", "b_rf1", "out0", "rf", "address_out0"),
//      List("cgra/", "b_rf1", "out0", "rf", "address_out0"),
//      List("cgra/", "b_fun1", "out0", "fun", "out"))
//    val inArray = ArrayBuffer(List("cgra/", "b_rf1", "in0", "rf", "in0"),
//      List("cgra/", "b_fun1", "in0", "fun", "in_a"),
//      List("cgra/", "b_fun1", "in1", "fun", "in_b"),
//      List("cgra", "input1"))

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





    val connect = new Connect (outArray, inArray)
    val test = connect.getConnect()

    connect.dumpConnect()

    val cp = new HardwareGeneration(arch, connect)
    chisel3.Driver.execute(args, () => new TopModule(cp.archList,arch.typeNum, cp.connectMap))  //verilog generation

    iotesters.Driver.execute(args, () => new TopModule(cp.archList,arch.typeNum, cp.connectMap)) {
      c => new TopMoUnitTest(c)
    }

    //println(test(List("cgra", "input0")))



//    val generate = new HardwareGeneration(arch, connect)



  }
}

