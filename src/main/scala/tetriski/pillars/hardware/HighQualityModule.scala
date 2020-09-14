package tetriski.pillars.hardware

import chisel3.iotesters.PeekPokeTester
import chisel3.util._
import chisel3.{Bundle, Input, Module, Output, UInt, _}

class BlackCell extends Module {
  val io = IO(new Bundle {
    val currentG = Input(UInt(1.W))
    val currentP = Input(UInt(1.W))
    val preG = Input(UInt(1.W))
    val preP = Input(UInt(1.W))

    val outG = Output(UInt(1.W))
    val outP = Output(UInt(1.W))
  })

  io.outG := (io.currentP & io.preG) | io.currentG
  io.outP := io.currentP & io.preP
}

class GrayCell extends Module {
  val io = IO(new Bundle {
    val currentG = Input(UInt(1.W))
    val currentP = Input(UInt(1.W))
    val preG = Input(UInt(1.W))

    val outG = Output(UInt(1.W))
  })
  io.outG := (io.currentP & io.preG) | io.currentG
}

class Buffer extends Module {
  val io = IO(new Bundle {
    val currentG = Input(UInt(1.W))
    val currentP = Input(UInt(1.W))

    val outG = Output(UInt(1.W))
    val outP = Output(UInt(1.W))
  })

  io.outG := io.currentG
  io.outP := io.currentP
}

/** High quality adder: Sklansky
 *
 * @param w the data width which should be power of 2
 */
class HighQualityAdder(w: Int) extends Module {
  val io = IO(new Bundle {
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  def getWidth(): Int = {
    w
  }

  if (w % 2 != 0) {
    throw new Exception("The data width is not power of 2!")
  }
  val depth = log2Ceil(w)

  val gArray = (0 until w).map(i => io.inputs(0)(i, i) & io.inputs(1)(i, i))
  val pArray = (0 until w).map(i => io.inputs(0)(i, i) ^ io.inputs(1)(i, i))

  var GMap = Map[List[Int], Data]()
  var PMap = Map[List[Int], Data]()

  for (i <- 0 until w) {
    GMap += List(-1, i) -> gArray(i)
    PMap += List(-1, i) -> pArray(i)
  }

  for (row <- 0 until depth) {
    val groupModuleNum = 2 << row
    val halfGroup = groupModuleNum / 2
    for (col <- 0 until w) {
      val pos = List(row, col)
      val groupIndex = col % groupModuleNum
      if (groupIndex < halfGroup) {
        if (groupIndex < halfGroup / 2) {
          GMap += pos -> GMap(List(row - 1, col))
          PMap += pos -> PMap(List(row - 1, col))
        } else {
          val module = Module(new Buffer)
          module.io.currentG := GMap(List(row - 1, col))
          module.io.currentP := PMap(List(row - 1, col))
          GMap += pos -> module.io.outG
          PMap += pos -> module.io.outP
        }
      } else {
        if (col < groupModuleNum) {
          val module = Module(new GrayCell)
          module.io.currentG := GMap(List(row - 1, col))
          module.io.currentP := PMap(List(row - 1, col))
          module.io.preG := GMap(List(row - 1, col - (col % halfGroup) - 1))

          GMap += pos -> module.io.outG
          PMap += pos -> PMap(List(row - 1, col))
        } else {
          val module = Module(new BlackCell)
          module.io.currentG := GMap(List(row - 1, col))
          module.io.currentP := PMap(List(row - 1, col))
          module.io.preG := GMap(List(row - 1, col - (col % halfGroup) - 1))
          module.io.preP := PMap(List(row - 1, col - (col % halfGroup) - 1))

          GMap += pos -> module.io.outG
          PMap += pos -> module.io.outP
        }
      }
    }
  }

  var sum = (1 until w).map(i => pArray(w - i) ^ GMap(List(depth - 1, w - i - 1)).asUInt())
    .reduce(Cat(_, _))
  sum = Cat(sum, pArray(0))

  io.outs(0) := sum
}

object test {
  def main(args: Array[String]): Unit = {
    val topDesign = () => new HighQualityAdder(4)
    chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), topDesign)
    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), topDesign) {
      c => new HighQualityAdderTester(c)
    }

    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), () => new HighQualityAdder(8)) {
      c => new HighQualityAdderTester(c)
    }
  }
}

class HighQualityAdderTester(c: HighQualityAdder)
  extends PeekPokeTester(c) {
  poke(c.io.inputs(0), 14)
  poke(c.io.inputs(1), 13)

  println("The result of 13 + 14 with " + c.getWidth().toString
    + "-bit adder is: " + peek(c.io.outs(0)).toString())

  expect(c.io.outs(0), 27)
}