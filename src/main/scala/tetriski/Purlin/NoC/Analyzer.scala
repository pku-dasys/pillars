package tetriski.pillars.Purlin.NoC

import chisel3.iotesters.PeekPokeTester
import chisel3.util.{Cat, MuxLookup}
import chisel3.{Bool, Bundle, Input, Module, Output, UInt, Vec, Wire, when, _}
import tetriski.pillars.Purlin._
import tetriski.pillars.Purlin.utils.{AnalyzedPacket, Coordinate, Packet, Parameters}

import scala.collection.mutable.ArrayBuffer

class Analyzer(size: Int, y: Int, x: Int, deqSeq: Array[(UInt, UInt)],
               broadcastArray: Array[(Int, Int)]) extends Module {
  val io = IO(new Bundle() {
    val packet = Input(new Packet)
    val analyzedPacket = Output(new AnalyzedPacket)

    val deqsReady = Input(Vec(size, Bool()))
    val valid = Input(Bool())
    val channelReady = Output(Bool())
  })

//  println(y.toString + " " + x.toString + " " + size.toString)

  val xUInt = x.U(Parameters.log2X.W)
  val yUInt = y.U(Parameters.log2Y.W)

  val co = Cat(xUInt, yUInt).asTypeOf(new Coordinate)

  io.analyzedPacket.grantNum := 0.U
  io.analyzedPacket.grants.foreach(grant => grant := 0.U(Parameters.getGrantWidth.W))
  io.analyzedPacket.packet := io.packet
  //  io.channelReady := false.B

  val channelDeqReady = (0 to broadcastArray.size).map(_ => Wire(Bool()))
  channelDeqReady.foreach(ready => ready := true.B)

  io.channelReady := true.B

  when(io.valid) {
    val routing = io.packet.header.routing
    val src = io.packet.header.src
    val dst = io.packet.header.dst


    //p2p
    when(dst.x === xUInt && dst.y === yUInt) {
      io.analyzedPacket.grantNum := 1.U
      io.analyzedPacket.grants(0) := (size - 1).U
      io.channelReady := io.deqsReady(size - 1)
    }.otherwise {
      val direction = Wire(UInt(2.W))
      if(Parameters.sourceRouting){
        direction := routing(1, 0)
        io.analyzedPacket.packet.header.routing := routing(Parameters.log2Routing - 1, 2)
      }else{
        //X-Y routing
        when(dst.x === xUInt ){
          when(dst.y > yUInt){
            direction := Parameters.S.U
          }.otherwise{
            direction := Parameters.N.U
          }
        }.otherwise{
          when(dst.x > xUInt){
            direction := Parameters.E.U
          }.otherwise{
            direction := Parameters.W.U
          }
        }
        io.analyzedPacket.packet.header.routing := DontCare
      }
      val grant = MuxLookup(direction, Parameters.Fault.U(Parameters.getGrantWidth.W), deqSeq)
      io.channelReady := io.deqsReady(grant)
      io.analyzedPacket.grantNum := 1.U
      io.analyzedPacket.grants(0) := grant

    }

    if(Parameters.useBroadcast){
      when(src === dst) {
        //broadcast
        //By default, we assume the routing algorithm guarantees congestion should not exist when broadcasting packets.

        val filter = Module(new Filter(size, Parameters.grantNumLimit, Parameters.getGrantWidth))
        io.analyzedPacket.grantNum := filter.io.validNum
        io.analyzedPacket.grants := filter.io.resources
        filter.io.signalRequests.foreach(b => b := false.B)
        filter.io.dataRequests.foreach(d => d := 0.U)


        for (i <- 0 until broadcastArray.size) {
          val pair = broadcastArray(i)
          val direction = pair._1
          val index = pair._2
          direction match {
            case Parameters.E => when(src.x <= xUInt && src.y === yUInt) {
              channelDeqReady(index) := io.deqsReady(index)
              filter.io.signalRequests(index) := true.B
              filter.io.dataRequests(index) := i.U
            }
            case Parameters.W => when(src.x >= xUInt && src.y === yUInt) {
              channelDeqReady(index) := io.deqsReady(index)
              filter.io.signalRequests(index) := true.B
              filter.io.dataRequests(index) := i.U
            }
            case Parameters.S => when(src.y <= yUInt) {
              channelDeqReady(index) := io.deqsReady(index)
              filter.io.signalRequests(index) := true.B
              filter.io.dataRequests(index) := i.U
            }
            case Parameters.N => when(src.y >= yUInt) {
              channelDeqReady(index) := io.deqsReady(index)
              filter.io.signalRequests(index) := true.B
              filter.io.dataRequests(index) := i.U
            }
          }
        }

        when(!(src === co)) {
          filter.io.signalRequests(size - 1) := true.B
          filter.io.dataRequests(size - 1) := (size - 1).U
          channelDeqReady(size - 1) := io.deqsReady(size - 1)
        }
        io.channelReady := channelDeqReady.reduce(_ & _)
      }
    }

  }


}


object AnalyzerTest extends App {
  val connectArray = Array(Parameters.E, Parameters.W, Parameters.S, Parameters.N)
  val broadcastArray = connectArray.map(i => (i, connectArray.indexOf(i)))
  val deqSeq = new ArrayBuffer[(UInt, UInt)]()
  for (i <- 0 until connectArray.size) {
    deqSeq.append(connectArray(i).U(Parameters.getRoutingRegionWidth.W) -> i.U(Parameters.getGrantWidth.W))
  }

  val analyzer = () => new Analyzer(connectArray.size + 1, 1, 1, deqSeq.toArray, broadcastArray)
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), analyzer) {
    c => new AnalyzerTester(c, connectArray.size + 1)
  }
}

class AnalyzerTester(analyzer: Analyzer, size: Int) extends PeekPokeTester(analyzer) {
  println("Test0: Grants Correctness for Broadcast")
  poke(analyzer.io.deqsReady(2), true)
  poke(analyzer.io.valid, true)

  expect(analyzer.io.channelReady, false)
  print("Channel Ready: " + peek(analyzer.io.channelReady).toString() + "\n")
  for (i <- 0 until size - 1) {
    print(peek(analyzer.io.analyzedPacket.grants(i)).toString() + "\t")
  }
  print("\nNumber of valid grants: " + peek(analyzer.io.analyzedPacket.grantNum).toString() + "\n")

  expect(analyzer.io.analyzedPacket.grants(0), 2)
  expect(analyzer.io.analyzedPacket.grants(1), size - 1)
  expect(analyzer.io.analyzedPacket.grantNum, 2)
  step(10)

  println("Test1: Channel Ready Correctness")
  poke(analyzer.io.deqsReady(size - 1), true)
  expect(analyzer.io.channelReady, true)
  print("Channel Ready: : " + peek(analyzer.io.channelReady).toString() + "\n")

  step(10)

  println("Test2: Correctness for P2P")
  poke(analyzer.io.packet.header.routing, Parameters.W)
  poke(analyzer.io.packet.header.src.x, 0)
  poke(analyzer.io.packet.header.src.y, 0)
  poke(analyzer.io.packet.header.dst.x, 0)
  poke(analyzer.io.packet.header.dst.y, 1)
  poke(analyzer.io.deqsReady(1), true)

  expect(analyzer.io.channelReady, true)
  print("Channel Ready: : " + peek(analyzer.io.channelReady).toString() + "\n")
  for (i <- 0 until size - 1) {
    print(peek(analyzer.io.analyzedPacket.grants(i)).toString() + "\t")
  }
  print("\nNumber of valid grants: " + peek(analyzer.io.analyzedPacket.grantNum).toString() + "\n")
  print("Routing: " + peek(analyzer.io.analyzedPacket.packet.header.routing).toString() + "\n")

  expect(analyzer.io.analyzedPacket.packet.header.routing, 0)
  expect(analyzer.io.analyzedPacket.grants(0), 1)
  expect(analyzer.io.analyzedPacket.grantNum, 1)

  step(10)
  println("Test3: Correctness for Arrival")
  poke(analyzer.io.packet.header.dst.x, 1)
  expect(analyzer.io.channelReady, true)
  print("Channel Ready: " + peek(analyzer.io.channelReady).toString() + "\n")
  for (i <- 0 until size - 1) {
    print(peek(analyzer.io.analyzedPacket.grants(i)).toString() + "\t")
  }
  print("\nNumber of valid grants: " + peek(analyzer.io.analyzedPacket.grantNum).toString() + "\n")

  expect(analyzer.io.analyzedPacket.grants(0), size - 1)
  expect(analyzer.io.analyzedPacket.grantNum, 1)

  step(10)
  println("Test4: Grants Correctness for Broadcast")
  poke(analyzer.io.packet.header.src.x, 1)
  poke(analyzer.io.packet.header.src.y, 1)
  poke(analyzer.io.packet.header.dst.x, 1)
  poke(analyzer.io.packet.header.dst.y, 1)

  expect(analyzer.io.channelReady, false)
  print("Channel Ready: " + peek(analyzer.io.channelReady).toString() + "\n")
  for (i <- 0 until size - 1) {
    print(peek(analyzer.io.analyzedPacket.grants(i)).toString() + "\t")
  }
  print("\nNumber of valid grants: " + peek(analyzer.io.analyzedPacket.grantNum).toString() + "\n")

  for (i <- 0 until size - 1) {
    expect(analyzer.io.analyzedPacket.grants(i), i)
  }
  expect(analyzer.io.analyzedPacket.grantNum, size - 1)
}

