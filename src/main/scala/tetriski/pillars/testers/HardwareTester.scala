package tetriski.pillars.testers

import chisel3.{assert, iotesters}
import chisel3.iotesters.PeekPokeTester
import tetriski.pillars.hardware.PillarsConfig._
import tetriski.pillars.hardware._
import tetriski.pillars.util.SplitOrConcat

/** A tester of the PEBlock.
 * Since the configuration is set according to human computation, it is deprecated.
 *
 * @deprecated
 * @param c the top design
 */
class TopModule2PEUnitTest(c: TopModule) extends PeekPokeTester(c) {
  poke(c.io.inputs(0), 2)
  poke(c.io.inputs(1), 3)


  //because input data is poked on the falling edge, we should wait a cycle
  poke(c.io.II, 3)
  poke(c.io.enConfig, 1)

  //010 001 001 0001 010 001 001 0000// PE1(13) PE0(13)
  //PE0: 010 001 110 0000 //  mux0(3) mux1(3) register(3)(rf(1) -> out1(output),
  // rf(1) -> out0(to self), input -> rf(0)) alu(4)
  //PE1: 010 001 110 0001
  //0100011100000
  //save (a+b) in pe0.rf(0), to next cycle //5
  //save (b-a) in pe1.rf(0), to next cycle //1
  poke(c.io.configuration, 18622688)
  step(1)

  //001 100 101 0011 001 100 101 0011
  //PE0: 001 100 101 0011 //output rf(1)=7
  //PE1: 001 100 101 0011
  //0011001010011
  // pe0.rf(1) = pe0.rf(0) or a // 5 or 2 =7
  // pe1.rf(1) = pe1.rf(0) or b // 1 or 3 =3
  poke(c.io.configuration, 13264467)
  step(1)

  //000 000 100 0000 011 100 010 0000
  //PE0: 011 100 010 0000 //
  //PE1: 000 000 100 0000 //output rf(1) to pe0
  //0000001000000 0111000100000
  // pe0.rf(0) = pe0.rf(1) + pe1.rf(1) // 7 + 3 = 10
  poke(c.io.configuration, 527904)
  step(1)

  poke(c.io.en, 1)

  expect(c.out, 0)
  //  expect(c.io.configTest(0), 2272)
  //  expect(c.io.configTest(1), 2273)
  step(1)
  expect(c.out, 0)

  //  expect(c.io.configTest(0), 1619)
  //  expect(c.io.configTest(1), 1619)
  //  step(1)
  //  expect(c.out, 2) //0 or 2 due to SyncReadMem
  step(1)
  expect(c.out, 7)
  //  expect(c.io.configTest(0), 3616)
  //  expect(c.io.configTest(1), 64)
  //  step(1)
  //  expect(c.out, 8)// 1 + 7 due to SyncReadMem
  step(1)
  expect(c.out, 10)
  step(1)
  expect(c.out, 10)
}

/** A tester of a 2*2 TileBlock with 2 input ports and 1 output port.
 * The schedules have not been taken into consideration.
 *
 * @param c         the top design
 * @param bitstream the configuration
 */
class TopModuleAdresUnitTest(c: TopModule, bitstream: BigInt) extends PeekPokeTester(c) {

  println(bitstream.toString())

  poke(c.io.II, 1)
  poke(c.io.enConfig, 1)

  poke(c.io.configuration, bitstream)
  step(1)

  poke(c.io.en, 1)

  for (i <- 0 until 40) {
    poke(c.io.inputs(1), i)
    if (i > 2) {
      expect(c.out, 5 * (i - 2 + 4))
    }
    println((5 * (i - 2 + 4)).toString + " " + peek(c.out).toString())
    step(1)
  }
}

/** A tester of a 2*2 TileLSUBlock with 2 input ports and 1 output port.
 *
 * @param c         the top design
 * @param bitstream the configuration
 * @param schedules the schedules of modules
 */
class TopModuleLSUAdresUnitTest(c: TopModule, bitstream: BigInt, schedules: List[Int]) extends PeekPokeTester(c) {

  val inData = (1 to 128).toArray
  poke(c.io.en, 0)
  poke(c.io.II, 1)

  val base = 0

  //The identification number of LSU is explicitly set according to the mapping result.
  poke(c.io.startLSU(0), 1)
  poke(c.io.enqEnLSU(0), 1)
  poke(c.io.streamInLSU(0).valid, 0)
  poke(c.io.baseLSU(0), base)
  step(1)

  // push
  for (x <- inData) {
    poke(c.io.streamInLSU(0).valid, 1)
    expect(c.io.streamInLSU(0).valid, 1)
    poke(c.io.streamInLSU(0).bits, x)
    if (peek(c.io.streamInLSU(0).ready) == 0) {
      while (peek(c.io.streamInLSU(0).ready) == 0) {
        step(1)
      }
    } else {
      step(1)
    } // exit condition: (c.io.in.ready === true.B) and step()
  }
  poke(c.io.streamInLSU(0).valid, 0)

  // exec
  while (peek(c.io.idleLSU(0)) == 0) {
    step(1)
  }

  poke(c.io.enqEnLSU(0), 0)

  //Set the configuration and schedules in the pre-process.
  poke(c.io.enConfig, 1)
  poke(c.io.configuration, bitstream)

  var schedulesBigInt: BigInt = 0
  for (sche <- schedules.reverse) {
    schedulesBigInt = (schedulesBigInt << LOG_SCHEDULE_SIZE + LOG_SKEW_LENGTH + 1) + sche
  }

  poke(c.io.schedules, schedulesBigInt)
  step(1)

  //Start the activating process.
  poke(c.io.en, 1)
  step(5)
  var ref = 0
  for (i <- 1 until 128) {
    ref = ref + i
    expect(c.out, ref)
    println(ref.toString + " " + peek(c.out).toString())
    step(1)
  }
}

/** A tester of a 4*4 TileCompleteBlock with 4 input ports and 4 output port.
 *
 * @param c          the top design
 * @param bitstreams the configurations
 * @param schedules  the schedules of modules
 */
class TopModuleCompleteAdresUnitTest(c: TopModule, bitstreams: Array[BigInt], schedules: List[Int])
  extends PeekPokeTester(c) {

  /** Enters data into a LSU.
   *
   * @param numInLSU the identification number of this LSU
   * @param inData   the input data array
   */
  def enqData(numInLSU: Int, inData: Array[Int]): Unit = {
    poke(c.io.startLSU(numInLSU), 1)
    poke(c.io.enqEnLSU(numInLSU), 1)
    poke(c.io.streamInLSU(numInLSU).valid, 0)
    poke(c.io.baseLSU(numInLSU), base)
    step(1)

    // push
    for (x <- inData) {
      poke(c.io.streamInLSU(numInLSU).valid, 1)
      expect(c.io.streamInLSU(numInLSU).valid, 1)
      poke(c.io.streamInLSU(numInLSU).bits, x)
      if (peek(c.io.streamInLSU(numInLSU).ready) == 0) {
        while (peek(c.io.streamInLSU(numInLSU).ready) == 0) {
          step(1)
        }
      } else {
        step(1)
      } // exit condition: (c.io.in.ready === true.B) and step()
    }
    poke(c.io.streamInLSU(numInLSU).valid, 0)

    // exec
    while (peek(c.io.idleLSU(numInLSU)) == 0) {
      step(1)
    }

    poke(c.io.enqEnLSU(numInLSU), 0)
  }

  val inData = (10 to 100).toArray
  poke(c.io.en, 0)

  val base = 0
  //The identification number of LSU is explicitly set according to the mapping result.
  val numInLSU = 3

  enqData(numInLSU, inData)

  poke(c.io.II, 3)

  //Set the configurations and schedules in the pre-process.
  poke(c.io.enConfig, 1)

  var schedulesBigInt: BigInt = 0
  for (sche <- schedules.reverse) {
    schedulesBigInt = (schedulesBigInt << LOG_SCHEDULE_SIZE + LOG_SKEW_LENGTH + 1) + sche
  }

  poke(c.io.schedules, schedulesBigInt)

  poke(c.io.configuration, bitstreams(0))
  step(1)

  poke(c.io.configuration, bitstreams(1))
  step(1)

  poke(c.io.configuration, bitstreams(2))
  step(1)

  poke(c.io.en, 1)
  step(11)

  //Start the activating process.
  var ref = 0
  for (i <- 10 until 100) {
    ref = ref + i
    expect(c.io.outs(0), ref)
    println(ref.toString + " " + peek(c.io.outs(0)).toString())
    step(3)
  }
}

/** A tester of a simple dispatcher.
 *
 * @deprecated
 * @param c         the dispatcher
 * @param bitstream the configuration
 */
class DispatchUnitTest(c: DispatchT, bitstream: BigInt) extends PeekPokeTester(c) {
  poke(c.io.configuration, bitstream)
  println(bitstream.toString())

  expect(c.outt, 3616)
  step(1)
}

/** A tester of a load/store unit.
 * In this tester, the functions of LSU for storing and loading data are tested.
 * Since the schedule is needed in current version of LSU, this tester is deprecated.
 *
 * @deprecated
 * @param c the LSU
 */
class LoadStoreUnitTester(c: LoadStoreUnit) extends PeekPokeTester(c) {
  poke(c.io.en, 0)
  val idata = c.memWrapper.enq_mem.manip.mode match {
    case SplitOrConcat.Normal =>
      Array(BigInt("cccccccc", 16), BigInt("deadbeef", 16), BigInt("cdcdcdcd", 16))
    case SplitOrConcat.Split =>
      assert(c.memWrapper.enq_mem.manip.factor == 4)
      Array(BigInt("deadc0de" + "cdcdcdcd" + "deadbeef" + "cccccccc", 16)) // little endian
    case SplitOrConcat.Concat =>
      assert(c.memWrapper.enq_mem.manip.factor == 4)
      Array(
        BigInt("cc", 16), BigInt("cc", 16), BigInt("cc", 16), BigInt("cc", 16),
        BigInt("ef", 16), BigInt("be", 16), BigInt("ad", 16), BigInt("de", 16),
        BigInt("cd", 16), BigInt("cd", 16), BigInt("cd", 16), BigInt("cd", 16)
      ) // little endian
  }
  val odata = Array(BigInt("cccccccc", 16), BigInt("deadbeef", 16), BigInt("cdcdcdcd", 16))

  val base = 100

  poke(c.io.start, 1)
  poke(c.io.enqEn, 1)
  poke(c.io.base, base)
  step(1)

  // push
  for (x <- idata) {
    poke(c.io.streamIn.valid, 1)
    poke(c.io.streamIn.bits, x)
    if (peek(c.io.streamIn.ready) == 0) {
      while (peek(c.io.streamIn.ready) == 0) {
        step(1)
      }
    } else {
      step(1)
    } // exit condition: (c.io.in.ready === true.B) and step()
  }
  poke(c.io.streamIn.valid, 0)

  // exec
  while (peek(c.io.idle) == 0) {
    step(1)
  }

  poke(c.io.enqEn, 0)
  poke(c.io.en, 1)

  // read
  poke(c.io.configuration, 0)
  poke(c.addr, base)
  step(1)
  expect(c.out, odata(0))

  //  poke(c.io.configuration, 0)
  poke(c.addr, base + 2)
  step(1)
  expect(c.out, odata(2))

  //  poke(c.io.configuration, 0)
  poke(c.addr, base + 1)
  step(1)
  expect(c.out, odata(1))

  poke(c.addr, 17)
  step(1)
  expect(c.out, 0)

  poke(c.io.configuration, 1)
  poke(c.dataIn, 233)
  poke(c.addr, 17)
  step(1)
  expect(c.out, 0)

  poke(c.io.configuration, 0)
  poke(c.addr, 17)
  step(1)
  expect(c.out, 233)
}

/** A object invoking the tester of a load/store unit.
 * Since the schedule is needed in current version of LSU, this tester is deprecated.
 *
 * @deprecated
 */
object LSUTest extends App {
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"),
    () => new LoadStoreUnit(32)) { c => new LoadStoreUnitTester(c) }
}

/** A object generating the Verilog of a load/store unit.
 */
object LoadStoreUnitVerilog extends App {
  chisel3.Driver.execute(args, () => new LoadStoreUnit(32))
}

/** A tester of a multiplexer.
 * @param c the multiplexer
 */
class MultiplexerUnitTester(c: Multiplexer) extends PeekPokeTester(c) {
  poke(c.io.configuration, 1)

  for (i <- 0 until 40) {
    poke(c.input0, i)
    poke(c.input1, i + 1)
    expect(c.out, i + 1)
    step(1)
  }
}

/** A object invoking the tester of a multiplexer.
 */
object MuxTest extends App {
  chisel3.Driver.execute(args, () => new Multiplexer(6, 32))
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"),
    () => new Multiplexer(6, 32)) { c => new MultiplexerUnitTester(c) }
}

/** A tester of a synchronizer with skew = 3.
 * @param c the synchronizer
 */
class SynchronizerTester(c: Synchronizer) extends PeekPokeTester(c) {
  poke(c.io.skewing, 3)
  poke(c.io.input0, 2)
  poke(c.io.input1, 3)

  for (i <- 0 until 10) {
    poke(c.io.input0, i)
    poke(c.io.input1, i + 1)
    println(peek(c.io.skewedInput0).toString())
    println(peek(c.io.skewedInput1).toString())
    step(1)
  }
}

/** A object invoking the tester of a synchronizer.
 */
object SkewTest extends App {
  iotesters.Driver.execute(args, () => new Synchronizer(32)) { c => new SynchronizerTester(c) }
}