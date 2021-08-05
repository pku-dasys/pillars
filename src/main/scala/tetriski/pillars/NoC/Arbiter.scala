package tetriski.pillars.NoC

import chisel3.iotesters.PeekPokeTester
import chisel3.{Bundle, Input, Module, Output, Vec}
import chisel3._
import chisel3.util.log2Ceil

class Arbiter(size: Int, numChannel: Int, grantResourceLimit: Int, grantWidth: Int) extends Module {
  val io = IO(new Bundle() {
    val numGrants = Input(Vec(size, Vec(numChannel, UInt(log2Ceil(grantResourceLimit + 1).W))))
    val grants = Input(Vec(size, Vec(numChannel, Vec(grantResourceLimit, UInt(grantWidth.W)))))
    val winners = Output(Vec(size, Bool()))
  })

  //  io.winners.foreach(win => win := true.B)
  val winners = (0 until size).map(_ => Wire(Bool()))
  winners.foreach(win => win := true.B)
  io.winners := winners

  val outputResourceLimit = size

  val grantCounts = VecInit((0 until outputResourceLimit).map(_ =>
    VecInit((0 until size).map(_ => 0.U(log2Ceil(numChannel * size * grantResourceLimit).W)))))

  for (i <- 0 until size) {
    val grantValids = VecInit((0 until outputResourceLimit).map(_ =>
      VecInit((0 until numChannel).map(_ => 0.U(log2Ceil(numChannel * grantResourceLimit).W)))))

    grantValids.foreach(c => c.foreach(r => r := 0.U))
    for (channel <- 0 until numChannel) {
      for (gRes <- 0 until grantResourceLimit) {
        when(gRes.U < io.numGrants(i)(channel)) {
          grantValids(io.grants(i)(channel)(gRes))(channel) := true.B
        }
      }
    }
    (0 until outputResourceLimit).foreach(
      res => grantCounts(res)(i) := grantValids(res).reduce(_ + _)
    )
  }

  val usedRes = VecInit((0 until outputResourceLimit).map(_ =>
    VecInit((0 until size).map(_ => 0.U(log2Ceil(numChannel * size * grantResourceLimit).W)))))

  for (i <- 0 until size) {
    for (res <- 0 until outputResourceLimit) {
      for (index <- 0 until i) {
        when(winners(index)) {
          usedRes(res)(index + 1) := grantCounts(res)(index) + usedRes(res)(index)
        }.otherwise {
          usedRes(res)(index + 1) := usedRes(res)(index)
        }
      }
      for (res <- 0 until outputResourceLimit) {
        when((usedRes(res)(i) + grantCounts(res)(i)) > numChannel.U
          && grantCounts(res)(i) > 0.U) {
          winners(i) := false.B
        }
      }
    }
  }

}

object ArbiterTest extends App {
  val arbiter = () => new Arbiter(5, 2, NoCParam.grantNumLimit, NoCParam.getGrantWidth)
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), arbiter) {
    c => new ArbiterTester(c)
  }
}

class ArbiterTester(arbiter: Arbiter) extends PeekPokeTester(arbiter) {
  poke(arbiter.io.numGrants(0)(0), 2)
  poke(arbiter.io.grants(0)(0)(0), 1)
  poke(arbiter.io.grants(0)(0)(1), 3)
  poke(arbiter.io.grants(0)(0)(2), 2) // Inactive

  poke(arbiter.io.numGrants(0)(1), 2)
  poke(arbiter.io.grants(0)(1)(0), 1)
  poke(arbiter.io.grants(0)(1)(1), 2)

  poke(arbiter.io.numGrants(2)(1), 2)
  poke(arbiter.io.grants(2)(1)(0), 1)
  poke(arbiter.io.grants(2)(1)(1), 2)

  expect(arbiter.io.winners(0), true)
  expect(arbiter.io.winners(1), true)
  expect(arbiter.io.winners(2), false)
  expect(arbiter.io.winners(3), true)
  expect(arbiter.io.winners(4), true)

  println("Winners:")
  for (i <- 0 until 5) {
    print(peek(arbiter.io.winners(i)).toString() + "\t")
  }
  print("\n")

  step(10)

  poke(arbiter.io.numGrants(3)(0), 1)
  poke(arbiter.io.grants(3)(0)(0), 2)

  expect(arbiter.io.winners(0), true)
  expect(arbiter.io.winners(1), true)
  expect(arbiter.io.winners(2), false)
  expect(arbiter.io.winners(3), true)
  expect(arbiter.io.winners(4), true)

  println("Winners:")
  for (i <- 0 until 5) {
    print(peek(arbiter.io.winners(i)).toString() + "\t")
  }
  print("\n")

  step(10)

  poke(arbiter.io.numGrants(4)(1), 1)
  poke(arbiter.io.grants(4)(1)(0), 2)

  expect(arbiter.io.winners(0), true)
  expect(arbiter.io.winners(1), true)
  expect(arbiter.io.winners(2), false)
  expect(arbiter.io.winners(3), true)
  expect(arbiter.io.winners(4), false)

  println("Winners:")
  for (i <- 0 until 5) {
    print(peek(arbiter.io.winners(i)).toString() + "\t")
  }
  print("\n")

  step(10)

  poke(arbiter.io.numGrants(1)(1), 3)
  poke(arbiter.io.grants(1)(1)(0), 2)
  poke(arbiter.io.grants(1)(1)(1), 3)
  poke(arbiter.io.grants(1)(1)(2), 4)

  expect(arbiter.io.winners(0), true)
  expect(arbiter.io.winners(1), true)
  expect(arbiter.io.winners(2), false)
  expect(arbiter.io.winners(3), false)
  expect(arbiter.io.winners(4), false)

  println("Winners:")
  for (i <- 0 until 5) {
    print(peek(arbiter.io.winners(i)).toString() + "\t")
  }
  print("\n")

  step(10)

}