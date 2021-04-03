package tetriski.pillars.NoC

import chisel3.iotesters.PeekPokeTester
import chisel3.util._
import chisel3.{Bundle, Input, Module, Output, UInt, Vec, _}

class MeshNoC(routerRule: (Int, Int) => Router, packetRule: () => Bundle) extends Module {

  val io = IO(new Bundle {
    val en = Input(Bool())

    //    val tileOutputValid = Vec(NoCParam.ySize, Vec(NoCParam.xSize, Input(Bool())))
    val enqFromTiles = Vec(NoCParam.ySize, Vec(NoCParam.xSize, Flipped(new DecoupledIO(packetRule.apply()))))
    //    val tileInputReady = Vec(NoCParam.ySize, Vec(NoCParam.xSize, Input(Bool())))
    val deqToTiles = Vec(NoCParam.ySize, Vec(NoCParam.xSize, new DecoupledIO(packetRule.apply())))
  })
  var routerMap = Map[(Int, Int), Router]()
  //  var tileMap = Map[(Int, Int), Data]()


  for (x <- 0 until NoCParam.xSize) {
    for (y <- 0 until NoCParam.ySize) {
      val router = Module(routerRule.apply(y, x))
      routerMap += (y, x) -> router
      router.io.en <> io.en
      router.io.enqFromTile <> io.enqFromTiles(y)(x)
      router.io.deqToTile <> io.deqToTiles(y)(x)

      //      val outputReg = RegInit(packetRule.apply())
      //      when(io.en && io.tileOutputValid(y)(x)) {
      //        outputReg := io.enqFromTiles(y)(x)
      //        router.io.enqFromTile.valid := true.B
      //      }
      //      router.io.enqFromTile.bits := outputReg
      //
      //      val inputReg = RegInit(packetRule.apply())
      //      tileMap += (y, x) -> inputReg
      //      io.deqToTiles(y)(x) := inputReg

      //      val headerMem = Mem(NoCParam.memSize, new Header)
      //      val header = io.headers(y)(x)

      //      val packet = Cat(header, outputReg).asTypeOf(new Packet)
      //      val packet = Wire(new Packet)
      //      packet


      //      router.io.deqToTile.ready := io.tileInputReady(y)(x)
      //      when(router.io.deqToTile.valid && io.tileInputReady(y)(x)) {
      ////        val packet = router.io.deqToTile.bits.asTypeOf(new Packet)
      //        inputReg := router.io.deqToTile.bits
      //      }.otherwise{
      //        inputReg := 0.U.asTypeOf(packetRule.apply())
      //      }

    }
  }

  for (x <- 0 until NoCParam.xSize) {
    for (y <- 0 until NoCParam.ySize) {
      val srcRouter = routerMap(y, x)
      val connectArray = srcRouter.connectArray
      for (i <- 0 until connectArray.size) {
        val direction = connectArray(i)
        val reDirection = NoCParam.reverse(direction)

        val dstRouter = direction match {
          case NoCParam.E => routerMap(y, x + 1)
          case NoCParam.W => routerMap(y, x - 1)
          case NoCParam.S => routerMap(y + 1, x)
          case NoCParam.N => routerMap(y - 1, x)
        }

        val dstIndex = dstRouter.connectArray.indexOf(reDirection)
        dstRouter.io.enqs(dstIndex) <> srcRouter.io.deqs(i)
      }
    }
  }

  //  io.test := tileMap(3, 2)
}

object MeshNoCTest extends App {
  //  val router = () => new MeshNoC((y, x) => new SimpleRouter(y, x), () => new Packet)
  //  //  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), router)
  //
  //  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), router) {
  //    c => new MeshNoCTester(c)
  //  }

  NoCParam.useMultiChannelRouter = true
//  NoCParam.xSize = 3
//  NoCParam.ySize = 3
  val multiChannelRouter = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x), () => new MultiChannelPacket)
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), multiChannelRouter) {
    c => new MeshMCNoCTester(c)
  }
}


class MeshNoCTester(mesh: MeshNoC) extends PeekPokeTester(mesh) {
  poke(mesh.io.en, true)
  poke(mesh.io.enqFromTiles(1)(1).valid, true)
  poke(mesh.io.enqFromTiles(1)(1).bits, Map("header.src.x" -> BigInt(1),
    "header.src.y" -> BigInt(1), "header.dst.x" -> BigInt(1), "header.dst.y" -> BigInt(1)))

  for (i <- 0 until 20) {
    println("Cycle: " + i)
    for (y <- 0 until NoCParam.ySize) {
      for (x <- 0 until NoCParam.xSize) {
        poke(mesh.io.deqToTiles(y)(x).ready, true)
        poke(mesh.io.enqFromTiles(y)(x).bits, Map("payload" -> BigInt(x + y + i)))
        print(peek(mesh.io.deqToTiles(y)(x).bits)("payload").toString() + "\t")
      }
      print("\n")
    }
    step(1)
  }

}

class MeshMCNoCTester(mesh: MeshNoC) extends PeekPokeTester(mesh) {
  poke(mesh.io.en, true)
  poke(mesh.io.enqFromTiles(1)(1).valid, true)
  val enqBundle = mesh.io.enqFromTiles(1)(1).bits.asInstanceOf[MultiChannelPacket]
  poke(enqBundle.validNum, 2)
  poke(enqBundle.packets(0).header.src.x, 1)
  poke(enqBundle.packets(0).header.src.y, 1)
  poke(enqBundle.packets(0).header.dst.x, 1)
  poke(enqBundle.packets(0).header.dst.y, 1)
  poke(enqBundle.packets(1).header.src.x, 1)
  poke(enqBundle.packets(1).header.src.y, 1)
  poke(enqBundle.packets(1).header.dst.x, 0)
  poke(enqBundle.packets(1).header.dst.y, 0)
  val routing =  Array("W", "N").reverse
    .map(d => NoCParam.intDirection(d)).reduce((l ,r) => (l << 2) + r)
  poke(enqBundle.packets(1).header.routing, routing)

  val enqBundle2 = mesh.io.enqFromTiles(2)(0).bits.asInstanceOf[MultiChannelPacket]
  poke(mesh.io.enqFromTiles(2)(0).valid, true)
  poke(enqBundle2.validNum, 1)
  poke(enqBundle2.packets(0).header.src.x, 0)
  poke(enqBundle2.packets(0).header.src.y, 2)
  poke(enqBundle2.packets(0).header.dst.x, 1)
  poke(enqBundle2.packets(0).header.dst.y, 1)
  val routing2 =  Array("E", "N").reverse
    .map(d => NoCParam.intDirection(d)).reduce((l ,r) => (l << 2) + r)
  poke(enqBundle2.packets(0).header.routing, routing2)

  val enqBundle3 = mesh.io.enqFromTiles(3)(3).bits.asInstanceOf[MultiChannelPacket]
  poke(mesh.io.enqFromTiles(3)(3).valid, true)
  poke(enqBundle3.validNum, 1)
  poke(enqBundle3.packets(0).header.src.x, 3)
  poke(enqBundle3.packets(0).header.src.y, 3)
  poke(enqBundle3.packets(0).header.dst.x, 3)
  poke(enqBundle3.packets(0).header.dst.y, 3)

//  val enqBundle4 = mesh.io.enqFromTiles(3)(1).bits.asInstanceOf[MultiChannelPacket]
//  poke(mesh.io.enqFromTiles(3)(1).valid, true)
//  poke(enqBundle4.validNum, 1)
//  poke(enqBundle4.packets(0).header.src.x, 1)
//  poke(enqBundle4.packets(0).header.src.y, 3)
//  poke(enqBundle4.packets(0).header.dst.x, 1)
//  poke(enqBundle4.packets(0).header.dst.y, 0)
//  val routing4 =  Array("N", "N", "N", "N").reverse
//    .map(d => NoCParam.intDirection(d)).reduce((l ,r) => (l << 2) + r)
//  poke(enqBundle4.packets(0).header.routing, routing4)

  testRunning()

  def testRunning()={
    for (i <- 0 until 30) {
      println("Cycle: " + i)
      for (y <- 0 until NoCParam.ySize) {
        for (x <- 0 until NoCParam.xSize) {
          poke(mesh.io.deqToTiles(y)(x).ready, true)
          if(i < 10){
            poke(mesh.io.enqFromTiles(y)(x).bits.asInstanceOf[MultiChannelPacket].packets(0).payload,
              x + y * 10 + i * 100)
            poke(mesh.io.enqFromTiles(y)(x).bits.asInstanceOf[MultiChannelPacket].packets(1).payload,
              10 * (x + y * 10 + i * 100))
          }else{
            poke(mesh.io.enqFromTiles(3)(3).valid, false)
            poke(mesh.io.enqFromTiles(1)(1).valid, false)
//            poke(mesh.io.enqFromTiles(3)(1).valid, false)
          }
          val bundle = mesh.io.deqToTiles(y)(x).bits.asInstanceOf[MultiChannelPacket]
          print(peek(bundle.validNum) + ": (")
          for(c <- 0 until NoCParam.channelSize){
            var printItem = peek(bundle.packets(c).payload).toString()
            if(c < NoCParam.channelSize - 1){
              printItem += ", "
            }
            print(printItem)
          }
          print(")\t")
        }
        print("\n")
      }
      step(1)
    }
  }


}

