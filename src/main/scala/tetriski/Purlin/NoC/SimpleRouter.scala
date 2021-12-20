package tetriski.Purlin.NoC

import chisel3.util._
import chisel3.{Module, UInt, _}
import tetriski.Purlin.utils.{Packet, Parameters}

import scala.collection.mutable.ArrayBuffer


class SimpleRouter(y: Int, x: Int) extends Router(y, x, () => new Packet) {


  //initialization
  for (i <- 0 until size) {
    deqs(i).valid := false.B
    val defaultPacket = Wire(new Packet)
    defaultPacket.payload := 0.U
    defaultPacket.header.routing := defaultRouting
    defaultPacket.header.src.x := defaultX
    defaultPacket.header.src.y := defaultY
    defaultPacket.header.dst.x := defaultX
    defaultPacket.header.dst.y := defaultY
    defaultPacket.header.srcPort := defaultP
    deqs(i).bits := defaultPacket
    enqs(i).ready := false.B
  }

//  val srcPacketBuffer = Module(new FIFO(new Packet, NoCParam.fifoDep))
//  srcPacketBuffer.io.enq.valid := false.B
//  srcPacketBuffer.io.enq.bits := DontCare
//  srcPacketBuffer.io.deq.ready := false.B
//
//
//  val dstPacketBuffer = Module(new FIFO(new Packet, NoCParam.fifoDep))
//  dstPacketBuffer.io.enq.valid := false.B
//  dstPacketBuffer.io.enq.bits := DontCare
//  dstPacketBuffer.io.deq.ready := false.B


  val packetBuffer = Module(new FIFO(new Packet, Parameters.fifoDep, "packetBuffer"))
  packetBuffer.io.deq.ready := false.B

  var concatValid = io.enqs(0).valid.asUInt()
  val validSeq = new ArrayBuffer[(UInt, UInt)]()

  val validSeqWidth = connectSize
  validSeq.append(1.U(validSeqWidth.W) -> 0.U(3.W))

  for (i <- 1 until connectSize) {
    var valid = io.enqs(i).valid
    for (j <- 0 until i) {
      valid = valid & (!io.enqs(j).valid)
    }
    concatValid = Cat(valid, concatValid)
    validSeq.append((1 << i).U(validSeqWidth.W) -> i.U(3.W))
  }

  val selEnqIndex = MuxLookup(concatValid, 4.U(3.W), validSeq)

  when(selEnqIndex === 4.U(3.W)) {
    packetBuffer.io.enq <> io.enqFromTile
  }.otherwise {
    val selEnq = io.enqs(selEnqIndex)
    packetBuffer.io.enq <> selEnq
  }


  val deqSeq = new ArrayBuffer[(UInt, UInt)]()
  for (i <- 0 until connectSize) {
    deqSeq.append(connectArray(i).U(2.W) -> i.U(3.W))
  }

  val packetValid = packetBuffer.io.deq.valid
  when(io.en && packetValid) {
    val data = packetBuffer.io.deq.bits.payload
    val routing = packetBuffer.io.deq.bits.header.routing
    val src = packetBuffer.io.deq.bits.header.src
    val dst = packetBuffer.io.deq.bits.header.dst


    when(src === dst) {
      //broadcast
      //By default, congestion should not exist.

      packetBuffer.io.deq.ready := true.B
      val packet = packetBuffer.io.deq.bits
      val validBroadcastArray = Array(Parameters.E, Parameters.W, Parameters.S, Parameters.N)
        .filter(i => connectArray.indexOf(i) >= 0)
      val broadcastArray = validBroadcastArray.map(i => (i, connectArray.indexOf(i)))

      for (pair <- broadcastArray) {
        val direction = pair._1
        val index = pair._2
        direction match {
          case Parameters.E => when(src.x <= xUInt && src.y === yUInt) {
            io.deqs(index).valid := true.B
            io.deqs(index).bits := packet
          }
          case Parameters.W => when(src.x >= xUInt && src.y === yUInt) {
            io.deqs(index).valid := true.B
            io.deqs(index).bits := packet
          }
          case Parameters.S => when(src.y <= yUInt) {
            io.deqs(index).valid := true.B
            io.deqs(index).bits := packet
          }
          case Parameters.N => when(src.y >= yUInt) {
            io.deqs(index).valid := true.B
            io.deqs(index).bits := packet
          }
        }
      }

      //      for (index <- broadcastArray) {
      //        //          io.deqs(index) <> packetBuffer.io.deq
      //        io.deqs(index).valid := true.B
      //        io.deqs(index).bits := packet
      //      }

      when(!(src === co)) {
        //        dstPacketBuffer.io.enq <> packetBuffer.io.deq
        io.deqToTile.valid := true.B
        io.deqToTile.bits := packet
      }
    }.otherwise {
      //p2p
      when(dst.x === xUInt && dst.y === yUInt) {
        io.deqToTile <> packetBuffer.io.deq
      }.otherwise {
        val direction = routing(1, 0)
        val deqSelIndex = MuxLookup(direction, 4.U(3.W), deqSeq)
        val deqSel = io.deqs(deqSelIndex)
        deqSel <> packetBuffer.io.deq
        val newPacket = Wire(new Packet)
        newPacket := packetBuffer.io.deq.bits
        newPacket.header.routing := routing(Parameters.log2Routing - 1, 2)
        deqSel.bits := newPacket

      }
    }

  }

//  srcPacketBuffer.io.enq <> io.enqFromTile
//  io.deqToTile <> dstPacketBuffer.io.deq


}

