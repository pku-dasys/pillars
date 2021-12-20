package tetriski.Purlin.SwitchBox

import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester
import tetriski.Purlin.utils.{GlobalRouting, MeshSBModel, Parameters}

object testMeshSB extends App {

//  val model = new MeshSBModel(2, 2, 2, 4)
//  val network = () => new MeshSBwithContext(model, 32, 256)
//  chisel3.Driver.execute(Array("-td", "PurlinResult/RTL/SwitchBox-Fs/"), network)

  for(c <- 2 until 4) {
    for (fs <- 4 until 9) {
      val model = new MeshSBModel(c, 4, 4, fs)
      val network = () => new MeshSBwithContext(model, 32, 1)
      chisel3.Driver.execute(Array("-td", "PurlinResult/RTL/SwitchBox-Fs/"), network)
    }
  }

//  for(routersPerDim <- 2 until 7) {
//    val model = new MeshSBModel(2, routersPerDim, routersPerDim, 4)
//    val network = () => new MeshSBwithContext(model, 32, 256)
//    chisel3.Driver.execute(Array("-td", "PurlinResult/RTL/SwitchBox/"), network)
//  }

//  for(i <- 6 until 12) {
//    val contextDepth = 2 << i
//    val model = new MeshSBModel(2, 4, 4, 4)
//    val network = () => new MeshSBwithContext(model, 32, contextDepth)
//    chisel3.Driver.execute(Array("-td", "PurlinResult/RTL/SwitchBox/"), network)
//  }
//
//  val model = new MeshSBModel(2, 4, 4, 4)
//  val network = () => new MeshSwitchBox(model, 16)

//  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), network) {
//    c => new MeshSBTester(c, model)
//  }
}

class MeshSBTester(c: MeshSwitchBox, model: MeshSBModel) extends PeekPokeTester(c) {
  model.setPath(1, 1, (-1, 0), (Parameters.E, 0))
  model.setPath(2, 1, (Parameters.W, 0), (Parameters.E, 1))
  model.setPath(3, 1, (Parameters.W, 1), (Parameters.N, 1))
  model.setPath(3, 0, (Parameters.S, 1), (-1, 1))

  model.setPath(2, 3, (-1, 1), (Parameters.N, 1))
  model.setPath(2, 2, (Parameters.S, 1), (Parameters.N, 0))
  model.setPath(2, 1, (Parameters.S, 0), (Parameters.N, 1))
  model.setPath(2, 0, (Parameters.S, 1), (Parameters.E, 1))
  model.setPath(3, 0, (Parameters.W, 1), (-1, 0))

  val configArray = model.getConfigArray

  poke(c.io.en, 1)
  // (x, y) => (y, x)
  poke(c.io.inputFromTiles(1)(1)(0), 123)
  poke(c.io.inputFromTiles(3)(2)(1), 666)
  for (configBundle <- configArray) {
    val y = configBundle.y
    val x = configBundle.x
    val dIndex = configBundle.dIndex
    val channel = configBundle.channel
    val config = configBundle.config
    poke(c.io.configs(y)(x)(dIndex)(channel), config)
  }

  for (i <- 0 until 10) {
    step(1)
    println("y: 0, x: 3, c: 1 ::: " + peek(c.io.outputToTiles(0)(3)(1)).toString()
      + "...  y: 0, x: 3, c: 0 ::: " + peek(c.io.outputToTiles(0)(3)(0)).toString())
  }
}

class RoutingResultTester(c: MeshSwitchBox, model: MeshSBModel, globalRouting: GlobalRouting)
  extends PeekPokeTester(c) {

  val configArray = model.getConfigArray

  poke(c.io.en, 1)
  // (x, y) => (y, x)
  for (configBundle <- configArray) {
    val y = configBundle.y
    val x = configBundle.x
    val dIndex = configBundle.dIndex
    val channel = configBundle.channel
    val config = configBundle.config
    poke(c.io.configs(y)(x)(dIndex)(channel), config)
  }

  for (i <- 0 until 20) {
    println("Cycle: " + i)
    for (message <- globalRouting.messages) {
      val srcX: Int = message.srcX
      val srcY: Int = message.srcY
      val srcChannel: Int = message.routingStrategy.get.apply(0).srcChannel.getOrElse(-1)
      poke(c.io.inputFromTiles(srcY)(srcX)(srcChannel), srcX * 10000 + srcY * 100 + i + srcChannel)

      val dstX: Int = message.dstX
      val dstY: Int = message.dstY
      val dstChannel: Int = message.routingStrategy.get.last.dstChannel.getOrElse(-1)

      val size = message.routingStrategy.get.size
      if (i > size) {
        expect(c.io.outputToTiles(dstY)(dstX)(dstChannel),
          srcX * 10000 + srcY * 100 + i + srcChannel - size)
        println("From: (" + srcX + ", " + srcY + ") to (" + dstX + ", " + dstY +
          "); Expected: " + (srcX * 10000 + srcY * 100 + i + srcChannel - size).toString
          + "\tReceived: " + peek(c.io.outputToTiles(dstY)(dstX)(dstChannel)).toString())
      }
    }
    step(1)
  }
}

