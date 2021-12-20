package tetriski.Purlin.SwitchBox

import chisel3.iotesters.PeekPokeTester
import chisel3.{Bool, Bundle, UInt, _}
import tetriski.Purlin.utils.{MeshSBModel, Parameters, SBModel}

import scala.io.Source

object LocalParam {
  val NOP = 0
  val E = 4
  val S = 3
  val W = 2
  val N = 1
  val T = 5

  val n = 1
  val w = 1 << 1
  val s = 1 << 2
  val e = 1 << 3
  val t = 1 << 4
}

class HeadFlit(val w: Int = 256, val maxHops: Int = 64) extends Bundle {
  val flag = UInt(1.W)
  val info = UInt(16.W)
  val localID = UInt(3.W)
  val routing = UInt((3 * maxHops).W)
  val unused = UInt((w - 3 * maxHops - 20).W)

  def getRoutingWidth = 3 * maxHops
}

class FunctionUnit(w: Int, totalChannelNum: Int, maxGrants: Int = 5) extends Module {
  val io = IO(new Bundle {
    val inputs = Input(Vec(totalChannelNum, UInt(w.W)))
    val outputs = Output(Vec(totalChannelNum, UInt(w.W)))
    val grants = Output(Vec(totalChannelNum, Vec(maxGrants, Bool())))
  })
}

class LocalLUM(w: Int, totalChannelNum: Int, routingTable: RoutingTable)
  extends FunctionUnit(w, totalChannelNum) {
  val LUMem = Mem(6, UInt(5.W))
  for (i <- 0 until 6) {
    LUMem(i) := routingTable.mapArray(i).U
  }

  for (i <- 0 until totalChannelNum) {
    for(j <- 0 until 5){
      io.grants(i)(j) := false.B
    }
    val flit = io.inputs(i).asTypeOf(new HeadFlit(w))
    val outputWire = Wire(new HeadFlit(w))
    outputWire := flit
    outputWire.routing := flit.routing(flit.getRoutingWidth - 4, 0) << 3
    io.outputs(i) := outputWire.asUInt()
//    printf("%d\n", flit.flag)
//    val debugger = Module(new Debugger(w))
//    debugger.io.flit := flit
    when(flit.flag === true.B) {
      val direction = flit.routing(flit.getRoutingWidth - 1, flit.getRoutingWidth - 3)
      when(direction === LocalParam.NOP.U) {
        val localID = flit.localID
        val encoding = LUMem(localID)
        for(j <- 0 until 5){
          io.grants(i)(j) := encoding(j)
        }
      }.otherwise {
        io.grants(i)(direction - 1.U) := true.B
      }
    }
  }
}

class Debugger(w: Int) extends Module{
  val io = IO(new Bundle() {
    val flit = Input(new HeadFlit(w))
  })
  printf("%d\n", io.flit.flag)
  printf("%d\n", io.flit.localID)
  printf("%d\n", io.flit.routing(io.flit.getRoutingWidth - 1, io.flit.getRoutingWidth - 3))
  val reg = RegInit(5.U)
  reg := io.flit.routing
}


class SBController(w: Int, totalChannelNum: Int, model: SBModel, table: RoutingTable) extends Module{
  override def desiredName = "SBController_" + model.x + "_" + model.y
  val io = IO(new Bundle{
    val en = Input(Bool())

    val inputs = Input(Vec(totalChannelNum, UInt(w.W)))
    val outputs = Output(Vec(totalChannelNum, UInt(w.W)))
  })
  def directionTrans(d: Int): Int ={
    val ret = (d + 1) match {
      case LocalParam.E => Parameters.E
      case LocalParam.S => Parameters.S
      case LocalParam.W => Parameters.W
      case LocalParam.N => Parameters.N
      case LocalParam.T => -1
    }
    ret
  }

  val sb = Module(new FlexibleSB(model, w, false))
  val lum = Module(new LocalLUM(w, model.adjacency.size, table))

  sb.io.en <> io.en

  assert(model.channelSize == 1)
  lum.io.inputs <> io.inputs
  val regs = (0 until totalChannelNum).map(_ => RegInit(0.U(w.W)))
  for(i <- 0 until totalChannelNum){
    regs(i) := lum.io.outputs(i)
    sb.io.inputs(i)(0) := regs(i)
    io.outputs(i) := sb.io.outputs(i)(0)
  }

  val configRegs = (0 until totalChannelNum).map(_ => RegInit(0.U(model.configSize.W)))

  for(i <- 0 until totalChannelNum){
    sb.io.configs(i)(0) := configRegs(i)
  }
  val maxGrants = 5
  for(i <- 0 until maxGrants){
    for(j <- 0 until maxGrants){
      val jIndex = directionTrans(j)
      if(model.adjacency.contains(jIndex)){
        val jPort = model.adjacency.indexOf(jIndex)
        val src = (jIndex, 0)
        val dst = (directionTrans(i), 0)
        if(model.adjacency.contains(dst._1)){
          if(model.portConnectMap(dst).contains(src)){
              when(lum.io.grants(jPort)(i)){
                val regIndex = model.adjacency.indexOf(directionTrans(i))
                configRegs(regIndex) := model.portConnectMap(dst).indexOf(src).U
            }
          }
        }
      }

    }
  }

}

class RoutingTable {
  var x = 0
  var y = 0
  var mapArray = new Array[BigInt](6)
}

class SBConfigIO {
  var routingTableMap = Map[(Int, Int), RoutingTable]()

  def initRoutingTable(filename: String): Unit = {
    val buffer = Source.fromFile(filename)
    val file = buffer.getLines().toArray

    val tableNum = file.length / 7
    for (i <- 0 until tableNum) {
      val offset = i * 7
      val pos = file(offset).split(" ")
      val routingTable = new RoutingTable
      routingTable.x = pos(0).toInt
      routingTable.y = pos(1).toInt
      for (j <- 0 until 6) {
        routingTable.mapArray(j) = BigInt(file(offset + j + 1), 2)
      }
      routingTableMap += (routingTable.x, routingTable.y) -> routingTable
    }
  }
}



class DeterministicSB(model: MeshSBModel, w: Int, configIO: SBConfigIO) extends Module {
  val channelSize = model.channelSize
  assert(channelSize == 1)
  val xSize = model.xSize
  val ySize = model.ySize
  val Fs = model.Fs
  val configSize = model.configSize
  val maxAdjacency = model.maxAdjacency

  override def desiredName = "DeterministicSwitchBoxNetwork_" + channelSize + "_" + xSize + "_" + ySize + "_Fs." + Fs

  val io = IO(new Bundle {
    val en = Input(Bool())

    val inputFromTiles = Input(Vec(ySize, Vec(xSize, Vec(channelSize, UInt(w.W)))))
    val outputToTiles = Output(Vec(ySize, Vec(xSize, Vec(channelSize, UInt(w.W)))))

  })

  var routerMap = Map[(Int, Int), SBController]()
  for (x <- 0 until xSize) {
    for (y <- 0 until ySize) {
      val routerModel = model.routerModelMap(x, y)

      val controller = Module(new SBController(w, routerModel.adjacency.size,
        routerModel, configIO.routingTableMap((x, y))))
      routerMap += (y, x) -> controller
      controller.io.en <> io.en

      for (c <- 0 until channelSize) {
        controller.io.inputs.last <> io.inputFromTiles(y)(x)(c)
        io.outputToTiles(y)(x)(c) <> controller.io.outputs.last
      }
    }
  }

  val topologyMap = model.topologyMap

  for (pair <- topologyMap) {
    val dstModel = pair._2._1
    val dstRouter = routerMap(dstModel.y, dstModel.x)
    val dstPortIndex = pair._2._2

    val srcModel = pair._1._1
    val srcRouter = routerMap(srcModel.y, srcModel.x)
    val srcPortIndex = pair._1._2

    for (c <- 0 until channelSize) {
      dstRouter.io.inputs(dstPortIndex) <> srcRouter.io.outputs(srcPortIndex)
    }
  }
}

object testMeshDSB extends App {
  val model = new MeshSBModel(1, 3, 3, 4)
  val configIO = new SBConfigIO
  configIO.initRoutingTable("routing_table.dat")
  val network = () => new DeterministicSB(model, 256, configIO)

  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), network) {
    c => new MeshDSBTester(c)
  }
}

class MeshDSBTester(c: DeterministicSB) extends PeekPokeTester(c) {

  poke(c.io.en, 1)
  // (x, y) => (y, x)
  val data = BigInt("1011000000000000000110001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 2)
  poke(c.io.inputFromTiles(0)(0)(0), data)

  for (i <- 0 until 10) {
    step(1)
    poke(c.io.inputFromTiles(0)(0)(0), i + 1)
    println("step " + (i + 1).toString)
    println("...  y: 1, x: 1::: " + peek(c.io.outputToTiles(1)(1)(0)).toString())
    println("...  y: 1, x: 2::: " + peek(c.io.outputToTiles(1)(2)(0)).toString())
    println("...  y: 2, x: 1::: " + peek(c.io.outputToTiles(2)(1)(0)).toString())
    println("...  y: 2, x: 2::: " + peek(c.io.outputToTiles(2)(2)(0)).toString())
  }
}
