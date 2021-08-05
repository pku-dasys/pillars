package tetriski.pillars.NoC

import chisel3.{Bool, Bundle, Input, Output}
import chisel3._
import chisel3.iotesters.PeekPokeTester

class Packer(numIn: Int, numOut: Int) extends Module{
  val io = IO(new Bundle() {
    val analyzedPackets = Input(Vec(numIn, new AnalyzedPacket))
    val packedPacket = Output(Vec(numOut, new MultiChannelPacket))
  })
  val channelSize = io.packedPacket(0).packets.size
  val dataWidth = io.packedPacket(0).packets(0).getWidth
  val grantSize = io.analyzedPackets(0).grants.size

  val filters = (0 until numOut).map(_ => Module(new Filter(numIn, channelSize, dataWidth)))
  val filterSignals = VecInit((0 until numOut).map(_ =>
    VecInit((0 until numIn).map(_ => false.B))))
  val filterDatas = VecInit((0 until numOut).map(_ =>
    VecInit((0 until numIn).map(_ => 0.U(filters(0).io.dataRequests(0).getWidth.W)))))

  for(i <- 0 until numOut){
    io.packedPacket(i).validNum := filters(i).io.validNum
    io.packedPacket(i).packets := filters(i).io.resources.asTypeOf(Vec(channelSize, new Packet))
    filters(i).io.signalRequests.foreach(signal => signal := false.B)
    filters(i).io.dataRequests.foreach(data => data := 0.U)
  }

  for(i <- 0 until numIn){
    val grantNum = io.analyzedPackets(i).grantNum
    for(g <- 0 until grantSize){
      when(g.U(grantNum.getWidth.W) < grantNum){
        val grant = io.analyzedPackets(i).grants(g)
        filterSignals(grant)(i) := true.B
        filterDatas(grant)(i) := io.analyzedPackets(i).packet.asUInt()
      }
    }
  }

  for(i <- 0 until numOut){
    for(j <- 0 until numIn){
      filters(i).io.signalRequests(j) := filterSignals(i)(j)
      filters(i).io.dataRequests(j) := filterDatas(i)(j)
    }
  }
}

object PackerTest extends App {
  val packer = () => new Packer(10, 5)
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), packer) {
    c => new PackerTester(c)
  }
}

class PackerTester(packer: Packer) extends PeekPokeTester(packer) {
  poke(packer.io.analyzedPackets(0).packet.payload, 1)
  poke(packer.io.analyzedPackets(0).grantNum, 2)
  poke(packer.io.analyzedPackets(0).grants(0), 1)
  poke(packer.io.analyzedPackets(0).grants(1), 2)
  poke(packer.io.analyzedPackets(0).grants(2), 3) // Inactive

  poke(packer.io.analyzedPackets(1).packet.payload, 5)
  poke(packer.io.analyzedPackets(1).grantNum, 1)
  poke(packer.io.analyzedPackets(1).grants(0), 0)

  poke(packer.io.analyzedPackets(2).packet.payload, 8)
  poke(packer.io.analyzedPackets(2).grantNum, 2)
  poke(packer.io.analyzedPackets(2).grants(0), 2)
  poke(packer.io.analyzedPackets(2).grants(1), 3)

  expect(packer.io.packedPacket(0).validNum, 1)
  expect(packer.io.packedPacket(1).validNum, 1)
  expect(packer.io.packedPacket(2).validNum, 2)
  expect(packer.io.packedPacket(3).validNum, 1)

  expect(packer.io.packedPacket(0).packets(0).payload, 5)
  expect(packer.io.packedPacket(1).packets(0).payload, 1)
  expect(packer.io.packedPacket(2).packets(0).payload, 1)
  expect(packer.io.packedPacket(2).packets(1).payload, 8)
  expect(packer.io.packedPacket(3).packets(0).payload, 8)

  for(i <- 0 until 5){
    println("Valid number: " + peek(packer.io.packedPacket(i).validNum).toString())
    for(channel <- 0 until NoCParam.channelSize){
      print(peek(packer.io.packedPacket(i).packets(channel).payload).toString() + "\t")
    }
    print("\n")
  }

  step(10)

}