package tetriski.pillars.NoC

import chisel3.util.{Cat, DecoupledIO, log2Ceil}
import chisel3.{Bool, Vec, _}

import scala.collection.mutable.ArrayBuffer

class MultiChannelRouter(y: Int, x: Int) extends Router(y, x, () => new MultiChannelPacket) {


  //initialization
  for (i <- 0 until size) {
    val MCPacket = Wire(new MultiChannelPacket)
    deqs(i).valid := false.B
    enqs(i).ready := false.B

    MCPacket.validNum := 0.U

    for (j <- 0 until NoCParam.channelSize) {
      val packet = MCPacket.packets(j)
      packet.payload := 0.U
      packet.header.routing := defaultRouting
      packet.header.src.x := defaultX
      packet.header.src.y := defaultY
      packet.header.dst.x := defaultX
      packet.header.dst.y := defaultY
      packet.header.srcPort := defaultP
    }

    deqs(i).bits := MCPacket
  }

  val buffers = (0 until size).map(s => Module(new FIFO(new MultiChannelPacket, NoCParam.fifoDep,
    "packetBuffer_" + s.toString)))
  for(i <- 0 until size){
    buffers(i).io.enq <> enqs(i)
  }

  val packer = Module(new Packer(size * NoCParam.channelSize, size))
  for(i <- 0 until size){
    when(packer.io.packedPacket(i).validNum > 0.U){
      deqs(i).bits := packer.io.packedPacket(i)
      deqs(i).valid := true.B
    }
  }

  val arbiter = Module(new Arbiter(size, NoCParam.channelSize, NoCParam.grantNumLimit, NoCParam.getGrantWidth))

  val deqSeq = new ArrayBuffer[(UInt, UInt)]()
  for (i <- 0 until connectArray.size) {
    deqSeq.append(connectArray(i).U(NoCParam.getRoutingRegionWidth.W) -> i.U(NoCParam.getGrantWidth.W))
  }

  val validBroadcastArray = Array(NoCParam.E, NoCParam.W, NoCParam.S, NoCParam.N)
    .filter(i => connectArray.indexOf(i) >= 0)
  val broadcastArray = validBroadcastArray.map(i => (i, connectArray.indexOf(i)))

  for (i <- 0 until size) {
    val channelReady = (0 until NoCParam.channelSize).map(_ => Wire(Bool()))
//    channelReady.foreach(cr => cr := false.B)

    val analyzers = (0 until NoCParam.channelSize)
      .map(_ => Module(new Analyzer(size, y, x, deqSeq.toArray, broadcastArray)))
    for (channel <- 0 until NoCParam.channelSize) {
      analyzers(channel).io.packet := buffers(i).io.deq.bits.packets(channel)
      analyzers(channel).io.valid := false.B
      analyzers(channel).io.deqsReady := deqs.map(deq => deq.ready)

      val emptyAnalyzedPacket = Wire(new AnalyzedPacket)
      emptyAnalyzedPacket.grantNum := 0.U
      emptyAnalyzedPacket.grants.foreach(grant => grant := 0.U)
      emptyAnalyzedPacket.packet := DontCare
      packer.io.analyzedPackets(i * NoCParam.channelSize + channel) := emptyAnalyzedPacket
      channelReady(channel) := analyzers(channel).io.channelReady
      when(buffers(i).io.deq.valid && io.en) {
        when(buffers(i).io.deq.bits.validNum > channel.U) {
          analyzers(channel).io.valid := true.B
        }
      }
    }

    val multiChannelPacketReady = channelReady.reduce(_ & _)

    val winner = Wire(Bool())
    winner := false.B

    for (channel <- 0 until NoCParam.channelSize) {
      arbiter.io.numGrants(i)(channel) := 0.U
      arbiter.io.grants(i)(channel).foreach(p => p := 0.U)
    }



    when(multiChannelPacketReady && io.en) {
      for (channel <- 0 until NoCParam.channelSize) {
        arbiter.io.numGrants(i)(channel) := analyzers(channel).io.analyzedPacket.grantNum
        arbiter.io.grants(i)(channel) := analyzers(channel).io.analyzedPacket.grants
      }

      winner := arbiter.io.winners(i)
    }


    buffers(i).io.deq.ready := false.B
    when(winner){
      for (channel <- 0 until NoCParam.channelSize) {
        packer.io.analyzedPackets(i * NoCParam.channelSize + channel) := analyzers(channel).io.analyzedPacket
      }
      buffers(i).io.deq.ready := true.B
    }
  }
}

