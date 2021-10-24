package tetriski.pillars.NoC

import chisel3.iotesters.PeekPokeTester
import chisel3.util._
import chisel3.{Bundle, Input, Module, Output, UInt, Vec, _}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class MeshNoC(routerRule: (Int, Int) => Router, packetRule: () => Bundle) extends Module {
  override def desiredName = "MeshNoc_" + NoCParam.channelSize + "_" + NoCParam.xSize + "_" + NoCParam.ySize

  val io = IO(new Bundle {
    val en = Input(Bool())

    //    val tileOutputValid = Vec(NoCParam.ySize, Vec(NoCParam.xSize, Input(Bool())))
    val enqFromTiles = Vec(NoCParam.ySize, Vec(NoCParam.xSize, Flipped(new DecoupledIO(packetRule.apply()))))
    //    val tileInputReady = Vec(NoCParam.ySize, Vec(NoCParam.xSize, Input(Bool())))
    val deqToTiles = Vec(NoCParam.ySize, Vec(NoCParam.xSize, new DecoupledIO(packetRule.apply())))
  })
  var routerMap = Map[(Int, Int), Router]()
  //  var tileMap = Map[(Int, Int), Data]()


  for (x <- 0 until NoCParam.xSize) {
    for (y <- 0 until NoCParam.ySize) {
      val router = Module(routerRule.apply(y, x))
      routerMap += (y, x) -> router
      router.io.en <> io.en
      router.io.enqFromTile <> io.enqFromTiles(y)(x)
      router.io.deqToTile <> io.deqToTiles(y)(x)

      //      val outputReg = RegInit(packetRule.apply())
      //      when(io.en && io.tileOutputValid(y)(x)) {
      //        outputReg := io.enqFromTiles(y)(x)
      //        router.io.enqFromTile.valid := true.B
      //      }
      //      router.io.enqFromTile.bits := outputReg
      //
      //      val inputReg = RegInit(packetRule.apply())
      //      tileMap += (y, x) -> inputReg
      //      io.deqToTiles(y)(x) := inputReg

      //      val headerMem = Mem(NoCParam.memSize, new Header)
      //      val header = io.headers(y)(x)

      //      val packet = Cat(header, outputReg).asTypeOf(new Packet)
      //      val packet = Wire(new Packet)
      //      packet


      //      router.io.deqToTile.ready := io.tileInputReady(y)(x)
      //      when(router.io.deqToTile.valid && io.tileInputReady(y)(x)) {
      ////        val packet = router.io.deqToTile.bits.asTypeOf(new Packet)
      //        inputReg := router.io.deqToTile.bits
      //      }.otherwise{
      //        inputReg := 0.U.asTypeOf(packetRule.apply())
      //      }

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
        dstRouter.io.enqs(dstIndex) <> srcRouter.io.deqs(i)
      }
    }
  }

  //  io.test := tileMap(3, 2)
}

object MeshNoCTest extends App {
  //  val router = () => new MeshNoC((y, x) => new SimpleRouter(y, x), () => new Packet)
  //  //  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), router)
  //
  //  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), router) {
  //    c => new MeshNoCTester(c)
  //  }

  NoCParam.useMultiChannelRouter = true
  //  NoCParam.xSize = 3
  //  NoCParam.ySize = 3
  //  val router = () => new MultiChannelRouter(1, 1)
  val multiChannelRouter = () => new MeshNoC((y, x) => new MultiChannelRouter(y, x), () => new MultiChannelPacket)
  //  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), router)
  //  chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), multiChannelRouter)
  //  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), multiChannelRouter) {
  //    c => new MeshMCNoCTester(c)
  //  }
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), multiChannelRouter) {
    c => new MeshNoCInjection(c)
  }
}


class MeshNoCTester(mesh: MeshNoC) extends PeekPokeTester(mesh) {
  poke(mesh.io.en, true)
  poke(mesh.io.enqFromTiles(1)(1).valid, true)
  poke(mesh.io.enqFromTiles(1)(1).bits, Map("header.src.x" -> BigInt(1),
    "header.src.y" -> BigInt(1), "header.dst.x" -> BigInt(1), "header.dst.y" -> BigInt(1)))

  for (i <- 0 until 20) {
    println("Cycle: " + i)
    for (y <- 0 until NoCParam.ySize) {
      for (x <- 0 until NoCParam.xSize) {
        poke(mesh.io.deqToTiles(y)(x).ready, true)
        poke(mesh.io.enqFromTiles(y)(x).bits, Map("payload" -> BigInt(x + y + i)))
        print(peek(mesh.io.deqToTiles(y)(x).bits)("payload").toString() + "\t")
      }
      print("\n")
    }
    step(1)
  }

}

class MeshNoCInjection(mesh: MeshNoC) extends PeekPokeTester(mesh) {
  def getRandomArray[T](source: Array[T], num: Int): ArrayBuffer[T] = {
    val res = new ArrayBuffer[T]()
    val oriSize = source.length
    var indexSet = Set[Int]()
    while (indexSet.size < num) {
      indexSet += scala.util.Random.nextInt(oriSize)
    }
    for (index <- indexSet) {
      res.append(source(index))
    }
    res
  }

  val xSize = NoCParam.xSize
  val ySize = NoCParam.ySize
  val channelSize = NoCParam.channelSize

  val portNum = xSize * ySize * channelSize
  val portArray = new ArrayBuffer[(Int, Int)]()
  var existPacketFromTile = Map[(Int, Int), ArrayBuffer[Int]]()
  var routingStrategy = new ArrayBuffer[BigInt]()

  for (x <- 0 until xSize) {
    for (y <- 0 until ySize) {
      existPacketFromTile += (x, y) -> (new ArrayBuffer[Int]())
      for (c <- 0 until channelSize) {
        portArray.append((x, y))
      }
    }
  }

  val injectionRate = 0.5
  val injectionNum = (portNum * injectionRate).toInt

  val srcArray = getRandomArray(portArray.toArray, injectionNum)
  val dstArray = srcArray.map(i => {
    var x = scala.util.Random.nextInt(xSize)
    var y = scala.util.Random.nextInt(ySize)
    while (x == i._1) {
      x = scala.util.Random.nextInt(xSize)
    }
    while (y == i._2) {
      y = scala.util.Random.nextInt(ySize)
    }
    (x, y)
  }
  )

  val minimalDis = new ArrayBuffer[Int]()

  for (i <- 0 until srcArray.size) {
    val xSrc = srcArray(i)._1
    val ySrc = srcArray(i)._2

    val xDst = dstArray(i)._1
    val yDst = dstArray(i)._2

    val xDis = xDst - xSrc
    val yDis = yDst - ySrc

    minimalDis.append(Math.abs(xDis) + Math.abs(yDis))

    val directionArray = new ArrayBuffer[String]()
    if (xDis > 0) {
      for (j <- 0 until xDis) {
        directionArray.append("E")
      }
    } else {
      for (j <- 0 until -xDis) {
        directionArray.append("W")
      }
    }

    if (yDis > 0) {
      for (j <- 0 until yDis) {
        directionArray.append("S")
      }
    } else {
      for (j <- 0 until -yDis) {
        directionArray.append("N")
      }
    }

    //x-y routing
//        val routing = directionArray.map(d => NoCParam.intDirection(d)).reduce((l, r) => (l << 2) + r)

    //random routing
    val routing = Random.shuffle(directionArray).map(d => NoCParam.intDirection(d)).reduce((l, r) => (l << 2) + r)

    routingStrategy.append(routing)

    existPacketFromTile((xSrc, ySrc)).append(i)
  }

  val cycleUpper = 100
  val packetLength = 10

  var sentPackets = scala.collection.mutable.Map[(Int, Int), Int]()

  var experiencedCycle = Map[(Int, Int), Int]()

  for (x <- 0 until xSize) {
    for (y <- 0 until ySize) {
      sentPackets += (x, y) -> 0
    }
  }


  poke(mesh.io.en, true)

  for (cycle <- 0 until cycleUpper) {
    println("Cycle: " + cycle)

    //send packets
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        val pArray = existPacketFromTile(x, y)
        val validNum = pArray.size
        poke(mesh.io.enqFromTiles(y)(x).valid, false)
        if (peek(mesh.io.enqFromTiles(y)(x).ready) == 1 && (sentPackets((x, y)) < packetLength)) {
          poke(mesh.io.enqFromTiles(y)(x).valid, true)
          val packetID = sentPackets((x, y))
          sentPackets((x, y)) = 1 + sentPackets((x, y))
          val enqBundle = mesh.io.enqFromTiles(y)(x).bits.asInstanceOf[MultiChannelPacket]
          poke(enqBundle.validNum, validNum)
          for (p <- 0 until validNum) {
            val index = pArray(p)
            val xSrc = srcArray(index)._1
            val ySrc = srcArray(index)._2

            val xDst = dstArray(index)._1
            val yDst = dstArray(index)._2

            val routing = routingStrategy(index)

            val payload = index * 4096 + packetID * 256 + cycle

            poke(enqBundle.packets(p).header.src.x, xSrc)
            poke(enqBundle.packets(p).header.src.y, ySrc)
            poke(enqBundle.packets(p).header.dst.x, xDst)
            poke(enqBundle.packets(p).header.dst.y, yDst)
            poke(enqBundle.packets(p).header.routing, routing)
            poke(enqBundle.packets(p).payload, payload)
          }
        }
      }
    }



    //receive packets
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        poke(mesh.io.deqToTiles(y)(x).ready, true)
        val bundle = mesh.io.deqToTiles(y)(x).bits.asInstanceOf[MultiChannelPacket]
        print(peek(bundle.validNum) + ": (")
        for (c <- 0 until channelSize) {
          val payload = peek(bundle.packets(c).payload)
          var printItem = payload.toString()
          if (c < channelSize - 1) {
            printItem += ", "
          }
          print(printItem)
          if (peek(bundle.validNum) > c) {
            val index = payload.toInt / 4096
            val packetID = (payload.toInt - index * 4096) / 256
            val oriCycle = payload.toInt - index * 4096 - packetID * 256
            experiencedCycle += (index, packetID) -> (cycle - oriCycle)
            //            println("\n" + payload + ", " + index + ", " + packetID + ", " + (cycle - oriCycle))
          }
        }
        print(")\t")
      }
      print("\n")
    }

    step(1)
  }


  for (i <- 0 until injectionNum) {
    for (j <- 0 until packetLength) {
      val key = (i, j)
      if (!experiencedCycle.contains(key)) {
        println(i + ", " + j + ", " + srcArray(i)._1 + ", " + srcArray(i)._2
          + ", " + dstArray(i)._1 + ", " + dstArray(i)._2)
      }
    }
  }

  val minimalLatency = minimalDis.reduce(_ + _).toDouble / injectionNum
  val realLatency = experiencedCycle.map(item => item._2).reduce(_ + _).toDouble / (injectionNum * packetLength)

  println("Expected received packets: " + packetLength * injectionNum +
    "\nReal received packets: " + experiencedCycle.size +
    "\nExpected average latency: " + minimalLatency +
    "\nReal average latency: " + realLatency
  )
}

class MeshMCNoCTester(mesh: MeshNoC) extends PeekPokeTester(mesh) {
  poke(mesh.io.en, true)
  poke(mesh.io.enqFromTiles(1)(1).valid, true)
  val enqBundle = mesh.io.enqFromTiles(1)(1).bits.asInstanceOf[MultiChannelPacket]
  poke(enqBundle.validNum, 2)
  poke(enqBundle.packets(0).header.src.x, 1)
  poke(enqBundle.packets(0).header.src.y, 1)
  poke(enqBundle.packets(0).header.dst.x, 1)
  poke(enqBundle.packets(0).header.dst.y, 1)
  poke(enqBundle.packets(1).header.src.x, 1)
  poke(enqBundle.packets(1).header.src.y, 1)
  poke(enqBundle.packets(1).header.dst.x, 0)
  poke(enqBundle.packets(1).header.dst.y, 0)
  val routing = Array("W", "N").reverse
    .map(d => NoCParam.intDirection(d)).reduce((l, r) => (l << 2) + r)
  poke(enqBundle.packets(1).header.routing, routing)

  val enqBundle2 = mesh.io.enqFromTiles(2)(0).bits.asInstanceOf[MultiChannelPacket]
  poke(mesh.io.enqFromTiles(2)(0).valid, true)
  poke(enqBundle2.validNum, 1)
  poke(enqBundle2.packets(0).header.src.x, 0)
  poke(enqBundle2.packets(0).header.src.y, 2)
  poke(enqBundle2.packets(0).header.dst.x, 1)
  poke(enqBundle2.packets(0).header.dst.y, 1)
  val routing2 = Array("E", "N").reverse
    .map(d => NoCParam.intDirection(d)).reduce((l, r) => (l << 2) + r)
  poke(enqBundle2.packets(0).header.routing, routing2)

  val enqBundle3 = mesh.io.enqFromTiles(3)(3).bits.asInstanceOf[MultiChannelPacket]
  poke(mesh.io.enqFromTiles(3)(3).valid, true)
  poke(enqBundle3.validNum, 1)
  poke(enqBundle3.packets(0).header.src.x, 3)
  poke(enqBundle3.packets(0).header.src.y, 3)
  poke(enqBundle3.packets(0).header.dst.x, 3)
  poke(enqBundle3.packets(0).header.dst.y, 3)

  //  val enqBundle4 = mesh.io.enqFromTiles(3)(1).bits.asInstanceOf[MultiChannelPacket]
  //  poke(mesh.io.enqFromTiles(3)(1).valid, true)
  //  poke(enqBundle4.validNum, 1)
  //  poke(enqBundle4.packets(0).header.src.x, 1)
  //  poke(enqBundle4.packets(0).header.src.y, 3)
  //  poke(enqBundle4.packets(0).header.dst.x, 1)
  //  poke(enqBundle4.packets(0).header.dst.y, 0)
  //  val routing4 =  Array("N", "N", "N", "N").reverse
  //    .map(d => NoCParam.intDirection(d)).reduce((l ,r) => (l << 2) + r)
  //  poke(enqBundle4.packets(0).header.routing, routing4)

  testRunning()

  def testRunning() = {
    for (i <- 0 until 30) {
      println("Cycle: " + i)
      for (y <- 0 until NoCParam.ySize) {
        for (x <- 0 until NoCParam.xSize) {
          poke(mesh.io.deqToTiles(y)(x).ready, true)
          if (i < 10) {
            poke(mesh.io.enqFromTiles(y)(x).bits.asInstanceOf[MultiChannelPacket].packets(0).payload,
              x + y * 10 + i * 100)
            poke(mesh.io.enqFromTiles(y)(x).bits.asInstanceOf[MultiChannelPacket].packets(1).payload,
              10 * (x + y * 10 + i * 100))
          } else {
            poke(mesh.io.enqFromTiles(3)(3).valid, false)
            poke(mesh.io.enqFromTiles(1)(1).valid, false)
            //            poke(mesh.io.enqFromTiles(3)(1).valid, false)
          }
          val bundle = mesh.io.deqToTiles(y)(x).bits.asInstanceOf[MultiChannelPacket]
          print(peek(bundle.validNum) + ": (")
          for (c <- 0 until NoCParam.channelSize) {
            var printItem = peek(bundle.packets(c).payload).toString()
            if (c < NoCParam.channelSize - 1) {
              printItem += ", "
            }
            print(printItem)
          }
          print(")\t")
        }
        print("\n")
      }
      step(1)
    }
  }


}

