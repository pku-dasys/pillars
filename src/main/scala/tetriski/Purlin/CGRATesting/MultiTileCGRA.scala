package tetriski.pillars.Purlin.CGRATesting

import chisel3.iotesters.PeekPokeTester
import chisel3.util.Cat
import chisel3.{Bundle, Input, Module, Output, UInt, Vec, _}
import tetriski.pillars.Purlin.NoC.{DecoupledIOHelper, FIFO, MeshNoC, MultiChannelRouter, SimpleRouter}
import tetriski.pillars.Purlin._
import tetriski.pillars.Purlin.utils.{DeliverCtrl, Header, MultiChannelPacket, Packet, Parameters, ReceiveCtrl}
import tetriski.pillars.hardware.TopModule

class MultiTileCGRA(tileMap: Map[(Int, Int), () => TopModule], bitStreamMap: Map[(Int, Int), BigInt],
                    scheduleMap: Map[(Int, Int), BigInt]) extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val configSel = Input(UInt(Parameters.log2MemSize.W))

    val test = Vec(Parameters.xSize * Parameters.ySize, Output(UInt(Parameters.payloadSize.W)))
  })

  def getTestHeader(isP2P: Boolean, y: Int, x: Int) = {
    val header = Wire(new Header)
    header.srcPort := 0.U

    val srcX = x.U(Parameters.log2X.W)
    val srcY = y.U(Parameters.log2Y.W)
    header.src.x := srcX
    header.src.y := srcY

    var dstX = x.U(Parameters.log2X.W)
    var dstY = y.U(Parameters.log2Y.W)
    if (isP2P) {
      dstX = 2.U(Parameters.log2X.W)
      dstY = 3.U(Parameters.log2Y.W)
    }
    header.dst.x := dstX
    header.dst.y := dstY

    val routing = Array("E", "E", "S", "S", "W").reverse
      .map(d => Parameters.intDirection(d).U(2.W)).reduce(Cat(_, _))

    val fullRouting = Cat(0.U((Parameters.log2Routing - routing.getWidth).W), routing)

    header.routing := fullRouting
    header
  }

  var routerRule = if (Parameters.useMultiChannelRouter) {
    (yIndex: Int, xIndex: Int) => new MultiChannelRouter(yIndex, xIndex)
  } else {
    (yIndex: Int, xIndex: Int) => new SimpleRouter(yIndex, xIndex)
  }
  var packetRule = if (Parameters.useMultiChannelRouter) {
    () => new MultiChannelPacket
  } else {
    () => new Packet
  }


  val meshNoC = Module(new MeshNoC(routerRule, packetRule))

  if (Parameters.useMultiChannelRouter) {
    assert(Parameters.tilePortSize == Parameters.channelSize,
      "The number of tile ports should be equal to the number of channel in the setting in NoCParam.")
  }

  //  meshNoC.io.headerSel := io.configSel
  meshNoC.io.en := io.en


  for (y <- 0 until Parameters.ySize) {
    for (x <- 0 until Parameters.xSize) {
      val tile = Module(tileMap(y, x).apply())

      assert(tile.io.inputs.size == Parameters.tilePortSize && tile.io.outs.size == Parameters.tilePortSize,
        "The number of tile ports should be equal to the setting in NoCParam.")

      assert(tile.io.inputs(0).getWidth == Parameters.payloadSize && tile.io.outs(0).getWidth == Parameters.payloadSize,
        "The payload (data) size of tile should be equal to the setting in NoCParam.")

      meshNoC.io.enqFromTiles(y)(x).bits := DontCare
      meshNoC.io.enqFromTiles(y)(x).valid := false.B
      meshNoC.io.deqToTiles(y)(x).ready := false.B

      tile.io.en := RegNext(io.en)
      tile.io.II := 1.U
      tile.io.schedules := 0.U
      tile.io.configuration := 0.U
      //      tile.io.enConfig := false.B
      tile.io.enConfig := RegNext(io.en)

      tile.io.streamInLSU <> DontCare
      tile.io.streamOutLSU <> DontCare
      tile.io.baseLSU <> DontCare
      tile.io.lenLSU <> DontCare
      tile.io.startLSU <> DontCare
      tile.io.enqEnLSU <> DontCare
      tile.io.deqEnLSU <> DontCare
      tile.io.idleLSU <> DontCare

      for (i <- 0 until Parameters.tilePortSize) {
        tile.io.inputs(i) := 0.U
      }

      val srcPacketBuffers = (0 until Parameters.tilePortSize)
        .map(p => Module(new FIFO(UInt(Parameters.payloadSize.W), Parameters.fifoDep,
          "srcPacketBuffer_" + y.toString + "_" + x.toString + "_" + p.toString)))
      //      val srcEnqs = Vec(NoCParam.tilePortSize, Wire(Flipped(new DecoupledIO(UInt(NoCParam.payloadSize.W)))))
      val srcEnqs = new DecoupledIOHelper(true, Parameters.payloadSize, Parameters.tilePortSize)
      //      val srcDeqs = Vec(NoCParam.tilePortSize, Wire(new DecoupledIO(UInt(NoCParam.payloadSize.W))))
      val srcDeqs = new DecoupledIOHelper(false, Parameters.payloadSize, Parameters.tilePortSize)

      for (i <- 0 until Parameters.tilePortSize) {
        srcDeqs(i).ready := false.B

        srcEnqs(i).valid := false.B
        srcEnqs(i).bits := tile.io.outs(i)
      }


      val dstPacketBuffers = (0 until Parameters.tilePortSize)
        .map(p => Module(new FIFO(UInt(Parameters.payloadSize.W), Parameters.fifoDep,
          "dstPacketBuffer_" + y.toString + "_" + x.toString + "_" + p.toString)))
      //      val dstEnqs = Vec(NoCParam.tilePortSize, Wire(Flipped(new DecoupledIO(UInt(NoCParam.payloadSize.W)))))
      val dstEnqs = new DecoupledIOHelper(true, Parameters.payloadSize, Parameters.tilePortSize)
      //      val dstDeqs = Vec(NoCParam.tilePortSize, Wire(new DecoupledIO(UInt(NoCParam.payloadSize.W))))
      val dstDeqs = new DecoupledIOHelper(false, Parameters.payloadSize, Parameters.tilePortSize)

      for (i <- 0 until Parameters.tilePortSize) {
        dstEnqs(i).valid := false.B
        dstEnqs(i).bits := 0.U

        dstDeqs(i).ready := false.B
      }

      io.test(y * Parameters.xSize + x) := tile.io.outs(0)
      //      meshNoC.io.enqFromTiles(y * NoCParam.xSize + x) := tile.io.outs(0)
      //      tile.io.inputs(0) := meshNoC.io.deqToTiles(y * NoCParam.xSize + x)

      val configSize = tile.io.configuration.getWidth
      /** Memory for configuration.
       * The 1st memory.
       */
      val configMem = Mem(Parameters.memSize, UInt(configSize.W))

      val scheduleSize = tile.io.schedules.getWidth
      /** Memory for schedules.
       * The 2nd memory.
       */
      val scheduleMem = Mem(Parameters.memSize, UInt(scheduleSize.W))

      /** Memory for controlling data deliver, including headers of packets.
       * The 3rd memory.
       */
      val deliverCtrlMem = Mem(Parameters.memSize, new DeliverCtrl)

      /** Memory for controlling data receive, guiding which port should receive the payload of packets.
       * The 4th memory.
       */
      val receiveCtrlMem = Mem(Parameters.memSize, new ReceiveCtrl)


      if (x == 1 && y == 1) {
        val deliverCtrl = Wire(new DeliverCtrl)

        if (Parameters.useMultiChannelRouter) {
          val deliverValidNum = 2
          val header0 = getTestHeader(true, y, x)
          val header1 = getTestHeader(false, y, x)
          deliverCtrl.validNum := deliverValidNum.U
          deliverCtrl.headers(0) := header0
          deliverCtrl.headers(1) := header1
        } else {
          val deliverValidNum = 1
          val header = getTestHeader(true, y, x)
          deliverCtrl.validNum := deliverValidNum.U
          deliverCtrl.headers(0) := header
        }
        deliverCtrlMem.write(1.U, deliverCtrl)

      }

      configMem.write(1.U, bitStreamMap(y, x).U(configSize.W))
      scheduleMem.write(1.U, scheduleMap(y, x).U(scheduleSize.W))

      val receiveCtrl = Wire(new ReceiveCtrl)
      receiveCtrl.validNum := 1.U
      receiveCtrl.patterns(0).dstPort := 0.U
      receiveCtrl.patterns(0).srcPort := 0.U
      receiveCtrl.patterns(0).src.x := 1.U
      receiveCtrl.patterns(0).src.y := 1.U
      receiveCtrlMem.write(1.U, receiveCtrl)

      when(io.en) {
        val config = configMem.read(io.configSel)
        val schedule = scheduleMem.read(io.configSel)
        tile.io.schedules := schedule
        tile.io.configuration := config

        //Encode
        val deliverCtrl = deliverCtrlMem.read(io.configSel)
        val deliverValidNum = deliverCtrl.validNum
        val deliverDataValids = VecInit((0 until Parameters.tilePortSize).map(_ => true.B))
        val NoCEnq = meshNoC.io.enqFromTiles(y)(x)
        val deliverSignal = deliverDataValids.reduce(_ & _) & (deliverValidNum > 0.U)
        NoCEnq.valid := deliverSignal
        val deliverPackets = (0 until Parameters.tilePortSize).map(_ => Wire(new Packet))
        for (deliverIndex <- 0 until Parameters.tilePortSize) {
          //Init
          deliverPackets(deliverIndex).payload := 0.U
          deliverPackets(deliverIndex).header.dst.y := 0.U
          deliverPackets(deliverIndex).header.dst.x := 0.U
          deliverPackets(deliverIndex).header.src.y := 0.U
          deliverPackets(deliverIndex).header.src.x := 0.U
          deliverPackets(deliverIndex).header.srcPort := 0.U
          deliverPackets(deliverIndex).header.routing := 0.U

          when(deliverIndex.U(deliverValidNum.getWidth.W) < deliverValidNum) {
            val header = deliverCtrl.headers(deliverIndex)
            val srcPort = header.srcPort
            val srcEnq = srcEnqs(srcPort)
            val srcDeq = srcDeqs(srcPort)
            srcEnq.valid := true.B
            deliverDataValids(deliverIndex) := srcDeq.valid
            deliverPackets(deliverIndex).payload := srcDeq.bits
            deliverPackets(deliverIndex).header := header
            srcDeq.ready := NoCEnq.ready & deliverSignal
          }
        }
        if (Parameters.useMultiChannelRouter) {
          val multiChannelPacket = Wire(new MultiChannelPacket)
          multiChannelPacket.validNum := deliverValidNum
          for (port <- 0 until Parameters.tilePortSize) {
            multiChannelPacket.packets(port) := deliverPackets(port)
          }
          //          for (port <- NoCParam.tilePortSize until NoCParam.channelSize) {
          //            multiChannelPacket.packets(port) := DontCare
          //          }
          NoCEnq.bits := multiChannelPacket
        } else {
          NoCEnq.bits := deliverPackets(0)
        }

        //decode
        val receiveCtrl = receiveCtrlMem.read(io.configSel)
        val NoCDeq = meshNoC.io.deqToTiles(y)(x)
        val receiveReadys = VecInit((0 until Parameters.tilePortSize).map(_ => true.B))
        val receiveSignal = receiveReadys.reduce(_ & _) & NoCDeq.valid
        NoCDeq.ready := receiveSignal
        if (Parameters.useMultiChannelRouter) {
          val multiChannelPacket = Wire(new MultiChannelPacket)
          multiChannelPacket := NoCDeq.bits
          val validPacketNum = multiChannelPacket.validNum
          for (packetIndex <- 0 until Parameters.channelSize) {
            when(packetIndex.U(validPacketNum.getWidth.W) < validPacketNum) {
              val packet = multiChannelPacket.packets(packetIndex.U(validPacketNum.getWidth.W))
              val header = packet.header
              for (receiveIndex <- 0 until Parameters.tilePortSize) {
                when(receiveIndex.U(receiveCtrl.validNum.getWidth.W) < receiveCtrl.validNum) {
                  val pattern = receiveCtrl.patterns(receiveIndex)
                  when(pattern.check(header)) {
                    val dstEnq = dstEnqs(pattern.dstPort)
                    receiveReadys(receiveIndex) := dstEnq.ready
                    dstEnq.valid := receiveSignal
                    dstEnq.bits := packet.payload
                  }
                }
              }
            }
          }
        }else{
          val packet = Wire(new Packet)
          packet := NoCDeq.bits
          val header = packet.header
          for (receiveIndex <- 0 until Parameters.tilePortSize) {
            when(receiveIndex.U(receiveCtrl.validNum.getWidth.W) < receiveCtrl.validNum) {
              val pattern = receiveCtrl.patterns(receiveIndex)
              when(pattern.check(header)) {
                val dstEnq = dstEnqs(pattern.dstPort)
                receiveReadys(receiveIndex) := dstEnq.ready
                dstEnq.valid := receiveSignal
                dstEnq.bits := packet.payload
              }
            }
          }
        }
      }

      for (i <- 0 until Parameters.tilePortSize) {
        val srcPacketBuffer = srcPacketBuffers(i)
        srcEnqs(i) <> srcPacketBuffer.io.enq
        srcDeqs(i) <> srcPacketBuffer.io.deq

        val dstPacketBuffer = dstPacketBuffers(i)
        dstEnqs(i) <> dstPacketBuffer.io.enq
        dstDeqs(i) <> dstPacketBuffer.io.deq
      }

      when(io.en) {
        for (i <- 0 until Parameters.tilePortSize) {
          dstDeqs(i).ready := true.B
          when(dstDeqs(i).valid) {
            tile.io.inputs(i) := dstDeqs(i).bits
          }
        }
      }

    }
  }
}


class NocMeshCGRATester(cgra: MultiTileCGRA) extends PeekPokeTester(cgra) {
  poke(cgra.io.en, false)
  poke(cgra.io.configSel, 1)
  step(1)
  poke(cgra.io.en, true)
  for (i <- 0 until 20) {
    println("Cycle: " + i)
    for (y <- 0 until Parameters.ySize) {
      for (x <- 0 until Parameters.xSize) {
        print(peek(cgra.io.test(y * Parameters.xSize + x)).toString() + "\t")
      }
      print("\n")
    }

    //    println(peek(mesh.routerMap(3,3).dstPacketBuffer.io.deq.bits.data).toString())
    step(1)
  }

}