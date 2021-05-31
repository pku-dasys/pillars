package tetriski.pillars.NoC

import chisel3.{Bool, Bundle, UInt}
import chisel3.util.log2Ceil
import chisel3._

object NoCParam {
  var xSize = 4
  var ySize = 4

  var tilePortSize = 1

  var channelSize = 2

  var useMultiChannelRouter = false

  def log2X = log2Ceil(xSize)

  def log2Y = log2Ceil(ySize)

  def log2TilePortSize = {
    var size = log2Ceil(tilePortSize)
    if(size == 0){
      size = 1
    }
    size
  }

  def log2Routing = 2 * (NoCParam.xSize + NoCParam.ySize)

  def log2MemSize = log2Ceil(memSize)

  var fifoDep = 8

  var payloadSize = 32

  var memSize = 4

  val E = 0
  val W = 3
  val S = 1
  val N = 2

  val Fault = 5

  var grandNumLimit = 4 //For broadcast, a packet could be granted to at most 4 destination
  var grandRegion = 5 //E, W, S, N, Arrival

  var useBroadcast = true

  def abandonBroadcast(): Unit = {
    grandNumLimit = 1
    useBroadcast = false
  }

  def getGrandNumWidth = log2Ceil(NoCParam.grandNumLimit + 1)

  def getGrandWidth = log2Ceil(grandRegion)

  def getRoutingRegionWidth = log2Ceil(grandRegion - 1) //E, W, S, N

  def reverse(direction: Int) = 3 - direction

  def strDirection(direction: Int) = direction match {
    case E => "E"
    case W => "W"
    case S => "S"
    case N => "N"
  }

  def intDirection(direction: String) = direction match {
    case "E" => E
    case "W" => W
    case "S" => S
    case "N" => N
  }
}

class Coordinate() extends Bundle {
  val x = UInt(NoCParam.log2X.W)
  val y = UInt(NoCParam.log2Y.W)

  def ===(that: Coordinate): Bool = {
    x === that.x && y === that.y
  }

}

class Header extends Bundle {
  val src = new Coordinate
  val dst = new Coordinate
  val srcPort = UInt(NoCParam.log2TilePortSize.W)
  val routing = UInt(NoCParam.log2Routing.W)
}

class Packet extends Bundle {
  val header = new Header
  val payload = UInt(NoCParam.payloadSize.W)
}

class AnalyzedPacket extends Bundle {
  val packet = new Packet
  val grandNum = UInt(log2Ceil(NoCParam.grandNumLimit + 1).W)
  val grands = Vec(NoCParam.grandNumLimit, UInt(NoCParam.getGrandWidth.W))
}

class MultiChannelPacket extends Bundle {
  val validNum = UInt(log2Ceil(NoCParam.channelSize + 1).W)
  val packets = Vec(NoCParam.channelSize, new Packet)
}

class DeliverCtrl extends Bundle {
  val validNum = UInt(log2Ceil(NoCParam.tilePortSize + 1).W)
  val headers = Vec(NoCParam.tilePortSize, new Header)
}

class ReceivePattern extends Bundle {
  val src = new Coordinate
  val srcPort = UInt(NoCParam.log2TilePortSize.W)
  val dstPort = UInt(NoCParam.log2TilePortSize.W)

  def check(header: Header) = (src === header.src) & (srcPort === header.srcPort)
}

class ReceiveCtrl extends Bundle {
  val validNum = UInt(log2Ceil(NoCParam.tilePortSize + 1).W)
  val patterns = Vec(NoCParam.tilePortSize, new ReceivePattern)
}