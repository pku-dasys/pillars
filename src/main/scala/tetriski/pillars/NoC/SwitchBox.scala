package tetriski.pillars.NoC

import chisel3.iotesters.PeekPokeTester
import chisel3.{Bundle, Input, Module, Output, Vec}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.{MuxLookup, log2Ceil}

import scala.collection.mutable.ArrayBuffer

class SwitchBox(size: Int, channelSize: Int, configSize: Int, w: Int) extends Module {

  //FixedPoint(w.W,3.BP)


  val io = IO(new Bundle() {
    val en = Input(Bool())
    val inputs = Input(Vec(size, Vec(channelSize, UInt(w.W))))
    val outputs = Output(Vec(size, Vec(channelSize, UInt(w.W))))
    val configs = Input(Vec(size, Vec(channelSize, UInt(configSize.W))))
  })

  val inputFromTile = io.inputs(size - 1)
  val outputToTile = io.outputs(size - 1)
}

class FlexibleSB(model: SBModel, w: Int)
  extends SwitchBox(model.adjacency.size, model.channelSize, model.configSize, w) {
  override def desiredName = "Router_" + model.x + "_" + model.y

  val portConnectMap = model.portConnectMap
  for (pair <- portConnectMap) {
    val dst = pair._1
    val srcs = pair._2
    val func = (0 until srcs.size).map(i => i.U -> io.inputs(model.findPortIndex(srcs(i)._1))(srcs(i)._2))

    when(io.en) {
      io.outputs(model.findPortIndex(dst._1))(dst._2) :=
        RegNext(MuxLookup(io.configs(model.findPortIndex(dst._1))(dst._2), 0.U, func))
    }.otherwise {
      io.outputs(model.findPortIndex(dst._1))(dst._2) := RegNext(0.U)
    }
  }
}

//
//class ErgodicSB(channelSize: Int, w: Int, x: Int, y: Int) extends SwitchBox(channelSize, w, x, y, false) {
//  for (i <- 0 until size) {
//    for (c <- 0 until channelSize) {
//      val func = new ArrayBuffer[(UInt, UInt)]()
//      for (j <- 0 until size)
//        for (cc <- 0 until channelSize) {
//          func.append((j * channelSize + cc).U -> io.inputs(j)(cc))
//        }
//      when(io.en) {
//        io.outputs(i)(c) := RegNext(MuxLookup(io.configs(i)(c), 0.U, func))
//      }.otherwise {
//        io.outputs(i)(c) := RegNext(0.U)
//      }
//
//    }
//  }
//}
//
//class DiscreteSB(channelSize: Int, w: Int, x: Int, y: Int) extends SwitchBox(channelSize, w, x, y, true) {
//  for (i <- 0 until size) {
//    for (c <- 0 until channelSize) {
//      val func = new ArrayBuffer[(UInt, UInt)]()
//      for (j <- 0 until size)
//        func.append((j * channelSize + c).U -> io.inputs(j)(c))
//      when(io.en) {
//        io.outputs(i)(c) := RegNext(MuxLookup(io.configs(i)(c), 0.U, func))
//      }.otherwise {
//        io.outputs(i)(c) := RegNext(0.U)
//      }
//    }
//  }
//}

//class SwitchBoxNetwork(channelSize: Int, w: Int, xSize: Int, ySize: Int, Fs: Int, configSize: Int) extends Module {
//  override def desiredName = "SwitchBoxNetwork_" + channelSize + "_" + xSize + "_" + ySize + "_Fs." + Fs
//
//  val io = IO(new Bundle {
//    val en = Input(Bool())
//
//    val inputFromTiles = Input(Vec(ySize, Vec(xSize, Vec(channelSize, UInt(w.W)))))
//    val outputToTiles = Output(Vec(ySize, Vec(xSize, Vec(channelSize, UInt(w.W)))))
//
//    val configs = Input(Vec(ySize, Vec(xSize, Vec(5, Vec(channelSize, UInt(configSize.W))))))
//  })
//
//  var routerMap = Map[(Int, Int), (SwitchBox, SBModel)]()
//  for (x <- 0 until xSize) {
//    for (y <- 0 until ySize) {
//      val routerModel = new SBModel(Fs, x, y, channelSize)
//      val router = Module(new FlexibleSB(routerModel, w))
//      routerMap += (y, x) -> router
//      router.io.en <> io.en
//      for (s <- 0 until routerModel.adjacency.size) {
//        router.io.configs(s) <> io.configs(y)(x)(s)
//      }
//
//      for (c <- 0 until channelSize) {
//        router.inputFromTile(c) <> io.inputFromTiles(y)(x)(c)
//        io.outputToTiles(y)(x)(c) <> router.outputToTile(c)
//      }
//    }
//  }
//
//  for (x <- 0 until xSize) {
//    for (y <- 0 until ySize) {
//      val srcRouter = routerMap(y, x)._1
//      val routerModel = routerMap(y, x)._2
//      val adjacency = routerModel.adjacency
//      for (i <- 0 until adjacency.size - 1) {
//        val direction = adjacency(i)
//        val reDirection = NoCParam.reverse(direction)
//
//        val dstRouter = direction match {
//          case NoCParam.E => routerMap(y, x + 1)
//          case NoCParam.W => routerMap(y, x - 1)
//          case NoCParam.S => routerMap(y + 1, x)
//          case NoCParam.N => routerMap(y - 1, x)
//        }
//
//        val dstIndex = dstRouter._2.adjacency.indexOf(reDirection)
//        for (c <- 0 until channelSize) {
//          dstRouter._1.io.inputs(dstIndex)(c) <> srcRouter.io.outputs(i)(c)
//        }
//
//      }
//    }
//  }
//}

class MeshSwitchBox(model: MeshSBModel, w: Int) extends Module {
  val channelSize = model.channelSize
  val xSize = model.xSize
  val ySize = model.ySize
  val Fs = model.Fs
  val configSize = model.configSize
  val maxAdjacency = model.maxAdjacency

  override def desiredName = "SwitchBoxNetwork_" + channelSize + "_" + xSize + "_" + ySize + "_Fs." + Fs

  val io = IO(new Bundle {
    val en = Input(Bool())

    val inputFromTiles = Input(Vec(ySize, Vec(xSize, Vec(channelSize, UInt(w.W)))))
    val outputToTiles = Output(Vec(ySize, Vec(xSize, Vec(channelSize, UInt(w.W)))))

    val configs = Input(Vec(ySize, Vec(xSize, Vec(5, Vec(channelSize, UInt(configSize.W))))))
  })

  var routerMap = Map[(Int, Int), SwitchBox]()
  for (x <- 0 until xSize) {
    for (y <- 0 until ySize) {
      val routerModel = model.routerModelMap(x, y)
      val router = Module(new FlexibleSB(routerModel, w))
      routerMap += (y, x) -> router
      router.io.en <> io.en
      for (s <- 0 until routerModel.adjacency.size) {
        router.io.configs(s) <> io.configs(y)(x)(s)
      }

      for (c <- 0 until channelSize) {
        router.inputFromTile(c) <> io.inputFromTiles(y)(x)(c)
        io.outputToTiles(y)(x)(c) <> router.outputToTile(c)
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
      dstRouter.io.inputs(dstPortIndex)(c) <> srcRouter.io.outputs(srcPortIndex)(c)
    }
  }
}


//object generateRTL extends App {
//  NoCParam.useMultiChannelRouter = true
//  NoCParam.abandonBroadcast()
//
//  for (xSize <- 2 to 8) {
//    for (ySize <- 2 to 8) {
//      for (channelSize <- 2 to 4) {
//        NoCParam.channelSize = channelSize
//        NoCParam.xSize = xSize
//        NoCParam.ySize = ySize
//        NoCParam.tilePortSize = NoCParam.channelSize
//
//        val design = () => new SwitchBoxNetwork(NoCParam.channelSize, NoCParam.payloadSize,
//          NoCParam.xSize, NoCParam.ySize, 5, )
//        chisel3.Driver.execute(Array("-td", "router/"), design)
//        val design1 = () => new SwitchBoxNetwork(NoCParam.channelSize, NoCParam.payloadSize,
//          NoCParam.xSize, NoCParam.ySize, false)
//        chisel3.Driver.execute(Array("-td", "router/"), design1)
//        val design2 = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x), () => new MultiChannelPacket)
//        chisel3.Driver.execute(Array("-td", "router/"), design2)
//      }
//    }
//  }
//
//}

object testMeshSB extends App {
  val model = new MeshSBModel(2, 4, 4, 5)
  val network = () => new MeshSwitchBox(model, 16)

  val routerModel = model.routerModelMap(1, 1)
  val router = () => new FlexibleSB(routerModel,32)
  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), router)
//  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), network) {
//    c => new MeshSBTester(c, model)
//  }
}

class MeshSBTester(c: MeshSwitchBox, model: MeshSBModel) extends PeekPokeTester(c) {
  model.setPath(1, 1, (-1, 0), (NoCParam.E, 0))
  model.setPath(2, 1, (NoCParam.W, 0), (NoCParam.E, 1))
  model.setPath(3, 1, (NoCParam.W, 1), (NoCParam.N, 1))
  model.setPath(3, 0, (NoCParam.S, 1), (-1, 1))

  model.setPath(2, 3, (-1, 1), (NoCParam.N, 1))
  model.setPath(2, 2, (NoCParam.S, 1), (NoCParam.N, 0))
  model.setPath(2, 1, (NoCParam.S, 0), (NoCParam.N, 1))
  model.setPath(2, 0, (NoCParam.S, 1), (NoCParam.E, 1))
  model.setPath(3, 0, (NoCParam.W, 1), (-1, 0))

  val configArray = model.getConfigArray

  poke(c.io.en, 1)
  // (x, y) => (y, x)
  poke(c.io.inputFromTiles(1)(1)(0), 123)
  poke(c.io.inputFromTiles(3)(2)(1), 666)
  for (configBundle <- configArray) {
    val y = configBundle.y
    val x = configBundle.x
    val dIndex = configBundle.dIndex
    val channel = configBundle.channel
    val config = configBundle.config
    poke(c.io.configs(y)(x)(dIndex)(channel), config)
  }

  for (i <- 0 until 10) {
    step(1)
    println("y: 0, x: 3, c: 1 ::: " + peek(c.io.outputToTiles(0)(3)(1)).toString()
      + "...  y: 0, x: 3, c: 0 ::: " + peek(c.io.outputToTiles(0)(3)(0)).toString())
  }
}

class RoutingResultTester(c: MeshSwitchBox, model: MeshSBModel, globalRouting: GlobalRouting)
  extends PeekPokeTester(c) {

  val configArray = model.getConfigArray

  poke(c.io.en, 1)
  // (x, y) => (y, x)
  for (configBundle <- configArray) {
    val y = configBundle.y
    val x = configBundle.x
    val dIndex = configBundle.dIndex
    val channel = configBundle.channel
    val config = configBundle.config
    poke(c.io.configs(y)(x)(dIndex)(channel), config)
  }

  for (i <- 0 until 20) {
    println("Cycle: " + i)
    for (message <- globalRouting.messages) {
      val srcX: Int = message.srcX
      val srcY: Int = message.srcY
      val srcChannel: Int = message.routingStrategy.get.apply(0).srcChannel.getOrElse(-1)
      poke(c.io.inputFromTiles(srcY)(srcX)(srcChannel), srcX * 10000 + srcY * 100 + i + srcChannel)

      val dstX: Int = message.dstX
      val dstY: Int = message.dstY
      val dstChannel: Int = message.routingStrategy.get.last.dstChannel.getOrElse(-1)

      val size = message.routingStrategy.get.size
      if(i > size){
        expect(c.io.outputToTiles(dstY)(dstX)(dstChannel),
          srcX * 10000 + srcY * 100 + i + srcChannel - size)
        println("From: (" + srcX + ", " + srcY + ") to (" + dstX + ", " + dstY +
          "); Expected: " + (srcX * 10000 + srcY * 100 + i + srcChannel - size).toString
          + "\tReceived: " + peek(c.io.outputToTiles(dstY)(dstX)(dstChannel)).toString())
      }
    }
    step(1)
  }
}
