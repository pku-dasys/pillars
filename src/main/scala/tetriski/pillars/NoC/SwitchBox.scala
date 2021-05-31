package tetriski.pillars.NoC

import chisel3.iotesters.PeekPokeTester
import chisel3.{Bundle, Input, Module, Output, Vec}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.{MuxLookup, log2Ceil}

import scala.collection.mutable.ArrayBuffer

class SwitchBox(channelSize: Int, w: Int, x: Int, y: Int, discrete: Boolean) extends Module {
  override def desiredName = "Router_" + y + "_" + x
  //FixedPoint(w.W,3.BP)

  val connectArray = new ArrayBuffer[Int]()
  if (x < NoCParam.xSize - 1) {
    connectArray.append(NoCParam.E)
  }
  if (x > 0) {
    connectArray.append(NoCParam.W)
  }
  if (y < NoCParam.ySize - 1) {
    connectArray.append(NoCParam.S)
  }
  if (y > 0) {
    connectArray.append(NoCParam.N)
  }

  val connectSize = connectArray.size
  val size = connectArray.size + 1

  val configSize = if (discrete) {
    channelSize
  } else {
    size * channelSize
  }

  val io = IO(new Bundle() {
    val en = Input(Bool())
    val inputs = Input(Vec(size, Vec(channelSize, UInt(w.W))))
    val outputs = Output(Vec(size, Vec(channelSize, UInt(w.W))))
    val configs = Input(Vec(size, Vec(channelSize, UInt(log2Ceil(configSize).W))))
  })

  val inputFromTile = io.inputs(size - 1)
  val outputToTile = io.outputs(size - 1)
}

class ErgodicSB(channelSize: Int, w: Int, x: Int, y: Int) extends SwitchBox(channelSize, w, x, y, false) {
  for (i <- 0 until size) {
    for (c <- 0 until channelSize) {
      val func = new ArrayBuffer[(UInt, UInt)]()
      for (j <- 0 until size)
        for (cc <- 0 until channelSize) {
          func.append((j * channelSize + cc).U -> io.inputs(j)(cc))
        }
      when(io.en) {
        io.outputs(i)(c) := RegNext(MuxLookup(io.configs(i)(c), 0.U, func))
      }.otherwise {
        io.outputs(i)(c) := RegNext(0.U)
      }

    }
  }
}

class DiscreteSB(channelSize: Int, w: Int, x: Int, y: Int) extends SwitchBox(channelSize, w, x, y, true) {
  for (i <- 0 until size) {
    for (c <- 0 until channelSize) {
      val func = new ArrayBuffer[(UInt, UInt)]()
      for (j <- 0 until size)
        func.append((j * channelSize + c).U -> io.inputs(j)(c))
      when(io.en) {
        io.outputs(i)(c) := RegNext(MuxLookup(io.configs(i)(c), 0.U, func))
      }.otherwise {
        io.outputs(i)(c) := RegNext(0.U)
      }
    }
  }
}

class SwitchBoxNetwork(channelSize: Int, w: Int, xSize: Int, ySize: Int, discrete: Boolean) extends Module {
  override def desiredName = "SwitchBoxNetwork_" + channelSize + "_" + xSize + "_" + ySize + "_discrete." + discrete
  val configSize = if (discrete) {
    channelSize
  } else {
    5 * channelSize
  }

  val io = IO(new Bundle {
    val en = Input(Bool())

    val inputFromTiles = Input(Vec(ySize, Vec(xSize, Vec(channelSize, UInt(w.W)))))
    val outputToTiles = Output(Vec(ySize, Vec(xSize, Vec(channelSize, UInt(w.W)))))

    val configs = Input(Vec(ySize, Vec(xSize, Vec(5, Vec(channelSize, UInt(log2Ceil(configSize).W))))))
  })

  var routerMap = Map[(Int, Int), SwitchBox]()
  for (x <- 0 until xSize) {
    for (y <- 0 until ySize) {
      val router = if (discrete) {
        Module(new DiscreteSB(channelSize, w, x, y))
      } else {
        Module(new ErgodicSB(channelSize, w, x, y))
      }
      routerMap += (y, x) -> router
      router.io.en <> io.en
      for (s <- 0 until router.size) {
        router.io.configs(s) <> io.configs(y)(x)(s)
      }

      for (c <- 0 until channelSize) {
        router.inputFromTile(c) <> io.inputFromTiles(y)(x)(c)
        io.outputToTiles(y)(x)(c) <> router.outputToTile(c)
      }
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
        for (c <- 0 until channelSize) {
          dstRouter.io.inputs(dstIndex)(c) <> srcRouter.io.outputs(i)(c)
        }

      }
    }
  }
}

object generateRTL extends App {
  NoCParam.useMultiChannelRouter = true
  NoCParam.abandonBroadcast()

  for(xSize <- 2 to 8){
    for(ySize <- 2 to 8){
      for(channelSize <- 2 to 4){
        NoCParam.channelSize = channelSize
        NoCParam.xSize = xSize
        NoCParam.ySize = ySize
        NoCParam.tilePortSize = NoCParam.channelSize

        val design = () => new SwitchBoxNetwork(NoCParam.channelSize, NoCParam.payloadSize,
          NoCParam.xSize, NoCParam.ySize, true)
        chisel3.Driver.execute(Array("-td", "router/"), design)
        val design1 = () => new SwitchBoxNetwork(NoCParam.channelSize, NoCParam.payloadSize,
          NoCParam.xSize, NoCParam.ySize, false)
        chisel3.Driver.execute(Array("-td", "router/"), design1)
        val design2 = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x), () => new MultiChannelPacket)
        chisel3.Driver.execute(Array("-td", "router/"), design2)
      }
    }
  }

}

