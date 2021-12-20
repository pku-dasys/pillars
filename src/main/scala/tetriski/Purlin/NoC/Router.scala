package tetriski.Purlin.NoC

import chisel3.util._
import chisel3.{Bundle, Input, Module, Vec, _}
import tetriski.Purlin.utils.{Coordinate, Parameters}

import scala.collection.mutable.ArrayBuffer

class Router(y: Int, x: Int, packetRule: () => Bundle) extends Module {
  override def desiredName = "Router_" + x + "_" + y

  def getPacketRule = packetRule

  val connectArray = new ArrayBuffer[Int]()
  if (x < Parameters.xSize - 1) {
    connectArray.append(Parameters.E)
  }
  if (x > 0) {
    connectArray.append(Parameters.W)
  }
  if (y < Parameters.ySize - 1) {
    connectArray.append(Parameters.S)
  }
  if (y > 0) {
    connectArray.append(Parameters.N)
  }

  val connectSize = connectArray.size
  val size = connectArray.size + 1

  val xUInt = x.U(Parameters.log2X.W)
  val yUInt = y.U(Parameters.log2Y.W)

  val co = Cat(xUInt, yUInt).asTypeOf(new Coordinate)

  val defaultX = 0.U(Parameters.log2X.W)
  val defaultY = 0.U(Parameters.log2Y.W)
  val defaultP = 0.U
  val defaultRouting = 0.U(log2Ceil(4 * (Parameters.xSize + Parameters.ySize)).W)


  val io = IO(new Bundle {
    val enqs = Vec(connectSize, Flipped(new DecoupledIO(packetRule.apply())))
    val deqs = Vec(connectSize, new DecoupledIO(packetRule.apply()))

    val en = Input(Bool())

    val enqFromTile = Flipped(new DecoupledIO(packetRule.apply()))
    val deqToTile = new DecoupledIO(packetRule.apply())

  })



  val enqs = (0 until size).map(_ => Wire(Flipped(new DecoupledIO(packetRule.apply()))))
  (0 until size - 1).foreach(i => enqs(i) <> io.enqs(i))
  enqs(size - 1) <> io.enqFromTile

  val deqs = (0 until size).map(_ => Wire(new DecoupledIO(packetRule.apply())))
  (0 until size - 1).foreach(i => deqs(i) <> io.deqs(i))
  deqs(size - 1) <> io.deqToTile
}
