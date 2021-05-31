package tetriski.pillars.NoC

import chisel3.util.{Cat, MuxLookup, log2Ceil}
import chisel3.{Bool, Bundle, Input, Module, Output, UInt, Vec, Wire, when}
import chisel3._
import chisel3.iotesters.PeekPokeTester

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

  val xUInt = x.U(NoCParam.log2X.W)
  val yUInt = y.U(NoCParam.log2Y.W)

  val co = Cat(xUInt, yUInt).asTypeOf(new Coordinate)

  io.analyzedPacket.grandNum := 0.U
  io.analyzedPacket.grands.foreach(grand => grand := 0.U(NoCParam.getGrandWidth.W))
  io.analyzedPacket.packet := io.packet
  //  io.channelReady := false.B

  val channelDeqReady = (0 until broadcastArray.size).map(_ => Wire(Bool()))
  channelDeqReady.foreach(ready => ready := true.B)

  when(io.valid) {
    val routing = io.packet.header.routing
    val src = io.packet.header.src
    val dst = io.packet.header.dst


    //p2p
    when(dst.x === xUInt && dst.y === yUInt) {
      io.analyzedPacket.grandNum := 1.U
      io.analyzedPacket.grands(0) := (size - 1).U
      io.channelReady := io.deqsReady(size - 1)
    }.otherwise {
      val direction = routing(1, 0)
      val grand = MuxLookup(direction, NoCParam.Fault.U(NoCParam.getGrandWidth.W), deqSeq)
      io.channelReady := io.deqsReady(grand)
      io.analyzedPacket.grandNum := 1.U
      io.analyzedPacket.grands(0) := grand
      io.analyzedPacket.packet.header.routing := routing(NoCParam.log2Routing - 1, 2)
    }

    if(NoCParam.useBroadcast){
      when(src === dst) {
        //broadcast
        //By default, we assume the routing algorithm guarantees congestion should not exist when broadcasting packets.

        val filter = Module(new Filter(size, NoCParam.grandNumLimit, NoCParam.getGrandWidth))
        io.analyzedPacket.grandNum := filter.io.validNum
        io.analyzedPacket.grands := filter.io.resources
        filter.io.signalRequests.foreach(b => b := false.B)
        filter.io.dataRequests.foreach(d => d := 0.U)


        for (i <- 0 until broadcastArray.size) {
          val pair = broadcastArray(i)
          val direction = pair._1
          val index = pair._2
          direction match {
            case NoCParam.E => when(src.x <= xUInt && src.y === yUInt) {
              channelDeqReady(index) := io.deqsReady(index)
              filter.io.signalRequests(index) := true.B
              filter.io.dataRequests(index) := i.U
            }
            case NoCParam.W => when(src.x >= xUInt && src.y === yUInt) {
              channelDeqReady(index) := io.deqsReady(index)
              filter.io.signalRequests(index) := true.B
              filter.io.dataRequests(index) := i.U
            }
            case NoCParam.S => when(src.y <= yUInt) {
              channelDeqReady(index) := io.deqsReady(index)
              filter.io.signalRequests(index) := true.B
              filter.io.dataRequests(index) := i.U
            }
            case NoCParam.N => when(src.y >= yUInt) {
              channelDeqReady(index) := io.deqsReady(index)
              filter.io.signalRequests(index) := true.B
              filter.io.dataRequests(index) := i.U
            }
          }
        }

        when(!(src === co)) {
          filter.io.signalRequests(size - 1) := true.B
          filter.io.dataRequests(size - 1) := (size - 1).U
          io.channelReady := io.deqsReady(size - 1)
        }

      }
    }

  }

  io.channelReady := channelDeqReady.reduce(_ & _)
}


object AnalyzerTest extends App {
  val connectArray = Array(NoCParam.E, NoCParam.W, NoCParam.S, NoCParam.N)
  val broadcastArray = connectArray.map(i => (i, connectArray.indexOf(i)))
  val deqSeq = new ArrayBuffer[(UInt, UInt)]()
  for (i <- 0 until connectArray.size) {
    deqSeq.append(connectArray(i).U(NoCParam.getRoutingRegionWidth.W) -> i.U(NoCParam.getGrandWidth.W))
  }

  val analyzer = () => new Analyzer(connectArray.size + 1, 1, 1, deqSeq.toArray, broadcastArray)
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), analyzer) {
    c => new AnalyzerTester(c, connectArray.size + 1)
  }
}

class AnalyzerTester(analyzer: Analyzer, size: Int) extends PeekPokeTester(analyzer) {
  println("Test0: Grands Correctness for Broadcast")
  poke(analyzer.io.deqsReady(2), true)
  poke(analyzer.io.valid, true)

  expect(analyzer.io.channelReady, false)
  print("Channel Ready: " + peek(analyzer.io.channelReady).toString() + "\n")
  for (i <- 0 until size - 1) {
    print(peek(analyzer.io.analyzedPacket.grands(i)).toString() + "\t")
  }
  print("\nNumber of valid grands: " + peek(analyzer.io.analyzedPacket.grandNum).toString() + "\n")

  expect(analyzer.io.analyzedPacket.grands(0), 2)
  expect(analyzer.io.analyzedPacket.grands(1), size - 1)
  expect(analyzer.io.analyzedPacket.grandNum, 2)
  step(10)

  println("Test1: Channel Ready Correctness")
  poke(analyzer.io.deqsReady(size - 1), true)
  expect(analyzer.io.channelReady, true)
  print("Channel Ready: : " + peek(analyzer.io.channelReady).toString() + "\n")

  step(10)

  println("Test2: Correctness for P2P")
  poke(analyzer.io.packet.header.routing, NoCParam.W)
  poke(analyzer.io.packet.header.src.x, 0)
  poke(analyzer.io.packet.header.src.y, 0)
  poke(analyzer.io.packet.header.dst.x, 0)
  poke(analyzer.io.packet.header.dst.y, 1)
  poke(analyzer.io.deqsReady(1), true)

  expect(analyzer.io.channelReady, true)
  print("Channel Ready: : " + peek(analyzer.io.channelReady).toString() + "\n")
  for (i <- 0 until size - 1) {
    print(peek(analyzer.io.analyzedPacket.grands(i)).toString() + "\t")
  }
  print("\nNumber of valid grands: " + peek(analyzer.io.analyzedPacket.grandNum).toString() + "\n")
  print("Routing: " + peek(analyzer.io.analyzedPacket.packet.header.routing).toString() + "\n")

  expect(analyzer.io.analyzedPacket.packet.header.routing, 0)
  expect(analyzer.io.analyzedPacket.grands(0), 1)
  expect(analyzer.io.analyzedPacket.grandNum, 1)

  step(10)
  println("Test3: Correctness for Arrival")
  poke(analyzer.io.packet.header.dst.x, 1)
  expect(analyzer.io.channelReady, true)
  print("Channel Ready: " + peek(analyzer.io.channelReady).toString() + "\n")
  for (i <- 0 until size - 1) {
    print(peek(analyzer.io.analyzedPacket.grands(i)).toString() + "\t")
  }
  print("\nNumber of valid grands: " + peek(analyzer.io.analyzedPacket.grandNum).toString() + "\n")

  expect(analyzer.io.analyzedPacket.grands(0), size - 1)
  expect(analyzer.io.analyzedPacket.grandNum, 1)

  step(10)
  println("Test4: Grands Correctness for Broadcast")
  poke(analyzer.io.packet.header.src.x, 1)
  poke(analyzer.io.packet.header.src.y, 1)
  poke(analyzer.io.packet.header.dst.x, 1)
  poke(analyzer.io.packet.header.dst.y, 1)

  expect(analyzer.io.channelReady, false)
  print("Channel Ready: " + peek(analyzer.io.channelReady).toString() + "\n")
  for (i <- 0 until size - 1) {
    print(peek(analyzer.io.analyzedPacket.grands(i)).toString() + "\t")
  }
  print("\nNumber of valid grands: " + peek(analyzer.io.analyzedPacket.grandNum).toString() + "\n")

  for (i <- 0 until size - 1) {
    expect(analyzer.io.analyzedPacket.grands(i), i)
  }
  expect(analyzer.io.analyzedPacket.grandNum, size - 1)
}

