package tetriski.pillars

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.{Bundle, Input, Module, Output, UInt}
import chisel3.util._

import scala.collection.mutable.ArrayBuffer


//configBits is the last param
class PillarsModuleInfo(moduleNums: List[Int], params : List[List[Int]]) {
  def getModuleNums(): List[Int] ={
    moduleNums
  }
  def getParams(num : Int): List[Int] ={
    params(num)
  }
  def getConfigBits(typeID : Int, moduleID :Int) : Int = {
    var currentNum = 0
    for (i <- 0 until typeID){
      currentNum += moduleNums(i)
    }
    currentNum += moduleID
    params(currentNum)(params(currentNum).length-1)
  }
}


class TopModule(val moduleInfos: PillarsModuleInfo, val connect: Map[List[Int], List[List[Int]]],
                val configList : List[List[List[Int]]], w: Int) extends Module {
  val io = IO(new Bundle {
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_a, 1: input_b
    val configTest = Output(Vec(2, UInt(w.W)))
    val configuration = Input(UInt(26.W))
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  //print("configList", configList)

//  def getConfigBit (typeID : Int): Int ={
//    val ret = typeID match {
//      case 0 => 4
//      case 1 => 3
//      case 2 => 3
//      case 3 => 32
//    }
//    ret
//  }
  val input_0 = io.inputs(0)
  val input_1 = io.inputs(1)
  val out = io.outs(0)

  val moduleNums = moduleInfos.getModuleNums()
  val types = moduleNums.size
  //  println(io.getElements(0))
  var currentNum = 0
//  val addNum = moduleNums(0)
//  val adders = (0 until addNum).toArray.map(t => Module(new Adder(moduleInfo(1)(t + currentNum))))
//  currentNum += addNum
//
//  val mulNum = moduleNums(1)
//  val muls = (0 until mulNum).toArray.map(t => Module(new Multiplier(moduleInfo(1)(t + currentNum))))
//  currentNum += mulNum
//
//  val PENum = moduleNums(3)
//  val PEs = (0 until PENum).toArray.map(t => Module(new ADRESPE(moduleInfo(1)(t + currentNum))))
//  currentNum += PENum
//
  val aluNum = moduleNums(0)
  val alus = (0 until aluNum).toArray.map(t => Module(new Alu(moduleInfos.getParams(t + currentNum)(0))))
  currentNum += aluNum



  val RFNum = moduleNums(1)
  val RFs = (0 until RFNum).toArray
    .map(t => Module(new RegisterFiles(moduleInfos.getParams(t + currentNum)(0),
      moduleInfos.getParams(t + currentNum)(1),
      moduleInfos.getParams(t + currentNum)(2),
      moduleInfos.getParams(t + currentNum)(3))))
  currentNum += RFNum

  val MuxNum = moduleNums(2)
  val Muxs = (0 until MuxNum).toArray
    .map(t => Module(new Multiplexer(moduleInfos.getParams(t + currentNum)(0), moduleInfos.getParams(t + currentNum)(1))))
  //println("2110", moduleInfos.getParams(11 + currentNum))
  currentNum += MuxNum

  val ConstNum = moduleNums(3)
  val Consts = (0 until ConstNum).toArray
    .map(t => Module(new ConstUnit(moduleInfos.getParams(t + currentNum)(0))))
  currentNum += ConstNum

  val modules = List(alus, RFs, Muxs, Consts)


  val outPorts = new ArrayBuffer[Array[List[Any]]]
  outPorts.append(alus.map(i => i.io.outs.toList))
  outPorts.append(RFs.map(i => i.io.outs.toList))
  outPorts.append(Muxs.map(i => i.io.outs.toList))
  outPorts.append(Consts.map(i => i.io.outs.toList))

  val inPorts = new ArrayBuffer[Array[List[Any]]]
  inPorts.append(alus.map(i => i.io.inputs.toList))
  inPorts.append(RFs.map(i => i.io.inputs.toList))
  inPorts.append(Muxs.map(i => i.io.inputs.toList))
  inPorts.append(Consts.map(i => List()))


  //println(configList)
  var dispatchs = ArrayBuffer[Dispatch]()
  var regionConfigBits = List[Int]()
  for (region <- configList){
    var configBits = List[Int]()
    var configPorts = List[Data]()
    for(moduleList <- region){
      val typeID = moduleList(0)
      val moduleID = moduleList(1)
      configBits = configBits :+ moduleInfos.getConfigBits(typeID, moduleID)
      val configPort = typeID match {
        case 0 => alus(moduleID).io.configuration
        case 1 => RFs(moduleID).io.configuration
        case 2 => Muxs(moduleID).io.configuration
        case 3 => Consts(moduleID).io.configuration
      }
      configPorts = configPorts :+ configPort
    }
    val regionTotalBits = configBits.reduce(_+_)
    //println(configBits)
    regionConfigBits = regionConfigBits :+ regionTotalBits
    val dispatch = Module(new Dispatch(regionTotalBits, configBits))
    for (i <- 0 until configBits.size){
      configPorts(i) := dispatch.io.outs(i)
    }
    //io.configTest(0) := alus
    dispatchs.append(dispatch)
  }
  val totalBits = regionConfigBits.reduce(_+_)
  val topDispatch = Module(new Dispatch(totalBits, regionConfigBits))
  topDispatch.io.configuration := io.configuration
  for (i <- 0 until dispatchs.size){
    dispatchs(i).io.configuration := topDispatch.io.outs(i)
  }

  //println(regionConfigBits)
  io.configTest(0) := topDispatch.io.outs(0)
  io.configTest(1) := topDispatch.io.outs(1)

  //sent configuration to modules
//  val dispatch = Module(new Dispatch(13, List(3, 3, 4, 3)))
//  dispatch.io.configuration := io.configuration
//
//  io.configTest(0) := dispatch.io.outs(0)
//  io.configTest(1) := dispatch.io.outs(1)
//
//  Muxs5(0).io.configuration := dispatch.io.outs(0)
//  Muxs5(1).io.configuration := dispatch.io.outs(1)
//  alus(0).io.configuration := dispatch.io.outs(2)
//  RFs1_1_2(0).io.configuration := dispatch.io.outs(3)


//  for (i <- 0 until connect.keys.size) {
//    println(connect.keys.toList(i))
//  }


  for (i <- 0 until connect.keys.size) {
    val src = connect.keys.toList(i)
    val dsts = connect(src)
    for (j <- 0 until dsts.size) {
      val dst = dsts(j)
      //println(dst, src)
      if (dst(0) == types) {
        io.outs(dst(2)) := outPorts(src(0))(src(1))(src(2)).asInstanceOf[Data]
      } else if (src(0) == types) {
        inPorts(dst(0))(dst(1))(dst(2)).asInstanceOf[Data] := io.inputs(src(2))
      } else {
        inPorts(dst(0))(dst(1))(dst(2)).asInstanceOf[Data] := outPorts(src(0))(src(1))(src(2)).asInstanceOf[Data]
      }
    }
  }

}

class TopModule2PEUnitTest(c: TopModule) extends PeekPokeTester(c) {
  //MixedVec don't support c.io.inputs(0) in poke
  poke(c.input_0, 2)
  poke(c.input_1, 3)
  //010 001 001 0001 010 001 001 0000// PE1(13) PE013)
  //PE0: 010 001 110 0000 //  mux0(3) mux1(3) register(3)(rf(1) -> out1(output), rf(1) -> out0(to self), input -> rf(0)) alu(4)
  //PE1: 010 001 110 0001
  //0100011100000
  //save (a+b) in pe0.rf(0), to next cycle //5
  //save (b-a) in pe1.rf(0), to next cycle //1
  poke(c.io.configuration, 18622688)
  expect(c.out, 0)
  expect(c.io.configTest(0), 2272)
  expect(c.io.configTest(1), 2273)
  step(1)
  expect(c.out, 0)
  //001 100 101 0011 001 100 101 0011
  //PE0: 001 100 101 0011 //output rf(1)=7
  //PE1: 001 100 101 0011
  //0011001010011
  // pe0.rf(1) = pe0.rf(0) or a // 5 or 2 =7
  // pe1.rf(1) = pe1.rf(0) or b // 1 or 3 =3
  poke(c.io.configuration, 13264467)
  expect(c.io.configTest(0), 1619)
  expect(c.io.configTest(1), 1619)
//  step(1)
//  expect(c.out, 2) //0 or 2 due to SyncReadMem
  step(1)
  expect(c.out, 7)
  //000 000 100 0000 011 100 010 0000
  //PE0: 011 100 010 0000 //
  //PE1: 000 000 100 0000 //output rf(1) to pe0
  //0000001000000 0111000100000
  // pe0.rf(0) = pe0.rf(1) + pe1.rf(1) // 7 + 3 = 10
  poke(c.io.configuration, 527904)
  expect(c.io.configTest(0), 3616)
  expect(c.io.configTest(1), 64)
//  step(1)
//  expect(c.out, 8)// 1 + 7 due to SyncReadMem
  step(1)
  expect(c.out, 10)
  step(1)
  expect(c.out, 10)
}

class TopModuleAdresUnitTest(c: TopModule, bitstream :BigInt) extends PeekPokeTester(c) {
  //MixedVec don't support c.io.inputs(0) in poke
  poke(c.input_0, 2)
  poke(c.input_1, 3)
  poke(c.io.configuration, bitstream)
  step(1)
  expect(c.out, 10)
  step(1)
  expect(c.out, 10)
}

