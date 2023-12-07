package pillars.testers

import pillars.hardware.PillarsConfig._
import pillars.hardware._
import pillars.util.SplitOrConcat

import chisel3.{Data, UInt, assert}
import chiseltest._
import chiseltest.iotesters.PeekPokeTester
import org.scalatest.flatspec.AnyFlatSpec

/** A tester of the Counter.
 *
 * @param c the Counter
 */
class CounterTester(c: Counter) extends PeekPokeTester(c) {
  val w = 8
  val init = 3
  val step = 2
  val end = 13
  val freq = 2
  val config: BigInt = (freq << (w * 3)) + (end << (w * 2)) + (step << w) + init

  val init2 = 1
  val step2 = 3
  val end2 = 16
  val freq2 = 2
  val config2: BigInt = (freq2 << (w * 3)) + (end2 << (w * 2)) + (step2 << w) + init2

  poke(c.io.II, 2)
  step(2)

  for (i <- 0 until 20) {
    if(i % 2 == 0){
      poke(c.io.en, true)
      poke(c.io.configuration, config)
    }else if(i % 2 == 1){
      if(i > 4){
        poke(c.io.en, true)
      }else{
        poke(c.io.en, false)
      }
      poke(c.io.configuration, config2)
    }
    println(peek(c.io.outs(0).asInstanceOf[UInt]).toString())
    step(1)
  }
}

object CTest extends App{
  org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
    behavior of "Counter"
    it should "work" in {
      test(new Counter(8)).runPeekPoke(new CounterTester(_))
    }
  })
}


/** A tester of the PEBlock.
 * Since the configuration is set according to human computation, it is deprecated.
 *
 * @deprecated
 * @param c the top design
 */
class TopModule2PEUnitTest(c: TopModule) extends PeekPokeTester(c) {
  poke(c.io.inputs(0).asInstanceOf[UInt], 2)
  poke(c.io.inputs(1).asInstanceOf[UInt], 3)


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

  expect(c.out.asInstanceOf[UInt], 0)
  //  expect(c.io.configTest(0), 2272)
  //  expect(c.io.configTest(1), 2273)
  step(1)
  expect(c.out.asInstanceOf[UInt], 0)

  //  expect(c.io.configTest(0), 1619)
  //  expect(c.io.configTest(1), 1619)
  //  step(1)
  //  expect(c.out, 2) //0 or 2 due to SyncReadMem
  step(1)
  expect(c.out.asInstanceOf[UInt], 7)
  //  expect(c.io.configTest(0), 3616)
  //  expect(c.io.configTest(1), 64)
  //  step(1)
  //  expect(c.out, 8)// 1 + 7 due to SyncReadMem
  step(1)
  expect(c.out.asInstanceOf[UInt], 10)
  step(1)
  expect(c.out.asInstanceOf[UInt], 10)
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
    poke(c.io.inputs(1).asInstanceOf[UInt], i)
    if (i > 2) {
      expect(c.out.asInstanceOf[UInt], 5 * (i - 2 + 4))
    }
    println((5 * (i - 2 + 4)).toString + " " + peek(c.out.asInstanceOf[UInt]).toString())
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

  var scheduleConfigs: Array[BigInt] = new Array[BigInt](II_UPPER_BOUND)
  for(ii <- 0 until II_UPPER_BOUND){
    scheduleConfigs(ii) = BigInt(0)
  }
  val sches = schedules.reverse
  for (j <- 0 until sches.size / II_UPPER_BOUND) {
    for(ii <- 0 until II_UPPER_BOUND){
      val sche = sches(j * II_UPPER_BOUND + ii)
      scheduleConfigs(ii) = (scheduleConfigs(ii) << LOG_SCHEDULE_SIZE + SKEW_WIDTH) + sche
    }
  }
//  for (sche <- schedules.reverse) {
//    schedulesBigInt = (schedulesBigInt << LOG_SCHEDULE_SIZE + LOG_SKEW_LENGTH + 1) + sche
//  }

  poke(c.io.schedules, scheduleConfigs(II_UPPER_BOUND - 1))
  step(1)

  //Start the activating process.
  poke(c.io.en, 1)
  step(5)
  var ref = 0
  for (i <- 1 until 128) {
    ref = ref + i
    expect(c.out.asInstanceOf[UInt], ref)
    println(ref.toString + " " + peek(c.out.asInstanceOf[UInt]).toString())
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

  var scheduleConfigs: Array[BigInt] = new Array[BigInt](II_UPPER_BOUND)
  for(ii <- 0 until II_UPPER_BOUND){
    scheduleConfigs(ii) = BigInt(0)
  }
  val sches = schedules.reverse
  for (j <- 0 until sches.size / II_UPPER_BOUND) {
    for(ii <- 0 until II_UPPER_BOUND){
      val sche = sches(j * II_UPPER_BOUND + ii)
      scheduleConfigs(ii) = (scheduleConfigs(ii) << LOG_SCHEDULE_SIZE + SKEW_WIDTH) + sche
    }
  }
  scheduleConfigs = scheduleConfigs.reverse

//  var schedulesBigInt: BigInt = 0
//  for (sche <- schedules.reverse) {
//    schedulesBigInt = (schedulesBigInt << LOG_SCHEDULE_SIZE + LOG_SKEW_LENGTH + 1) + sche
//  }

  poke(c.io.schedules, scheduleConfigs(0))
  poke(c.io.configuration, bitstreams(0))
  step(1)

  poke(c.io.schedules, scheduleConfigs(1))
  poke(c.io.configuration, bitstreams(1))
  step(1)

  poke(c.io.schedules, scheduleConfigs(2))
  poke(c.io.configuration, bitstreams(2))
  step(1)

  poke(c.io.en, 1)
  step(11)

  //Start the activating process.
  var ref = 0
  for (i <- 10 until 100) {
    ref = ref + i
    expect(c.io.outs(0).asInstanceOf[UInt], ref)
    println(ref.toString + " " + peek(c.io.outs(0).asInstanceOf[UInt]).toString())
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
  def safePoke(data: Data, value: BigInt): Unit ={
    if(USE_TOKEN){
      poke(data.asInstanceOf[TokenIO].data, value)
      poke(data.asInstanceOf[TokenIO].token, true)
    }else{
      poke(data.asInstanceOf[UInt], value)
    }
  }

  def safeExpect(data: Data, value: BigInt): Unit ={
    if(USE_TOKEN){
      expect(data.asInstanceOf[TokenIO].data, value)
      expect(data.asInstanceOf[TokenIO].token, true)
    }else{
      expect(data.asInstanceOf[UInt], value)
    }
  }

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
  safePoke(c.io.inputs(0), base)
  step(1)
  safeExpect(c.out, odata(0))

  //  poke(c.io.configuration, 0)
  safePoke(c.io.inputs(0), base + 2)
  step(1)
  safeExpect(c.out, odata(2))

  //  poke(c.io.configuration, 0)
  safePoke(c.io.inputs(0), base + 1)
  step(1)
  safeExpect(c.out, odata(1))

  safePoke(c.io.inputs(0), 17)
  step(1)
  safeExpect(c.out, 0)

  safePoke(c.io.configuration, 1)
  safePoke(c.io.inputs(1), 233)
  safePoke(c.io.inputs(0), 17)
  step(1)
  safeExpect(c.out, 0)

  safePoke(c.io.configuration, 0)
  safePoke(c.io.inputs(0), 17)
  step(1)
  safeExpect(c.out, 233)
}

/** A object invoking the tester of a load/store unit.
 * Since the schedule is needed in current version of LSU, this tester is deprecated.
 *
 * @deprecated
 */
object LSUTest extends App {
  org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
    behavior of "LoadStoreUnit"
    it should "work" in {
      test(new LoadStoreUnit(32)).runPeekPoke(new LoadStoreUnitTester(_))
    }
  })
}

/** A object generating the Verilog of a load/store unit.
 */
object LoadStoreUnitVerilog extends App {
  chisel3.emitVerilog(new LoadStoreUnit(32), args)
}

/** A tester of a multiplexer.
 *
 * @param c the multiplexer
 */
class MultiplexerUnitTester(c: Multiplexer) extends PeekPokeTester(c) {
  poke(c.io.configuration, 1)

  for (i <- 0 until 40) {
    poke(c.io.inputs(0).asInstanceOf[UInt], i)
    poke(c.io.inputs(1).asInstanceOf[UInt], i + 1)
    expect(c.out.asInstanceOf[UInt], i + 1)
    step(1)
  }
}

/** A object invoking the tester of a multiplexer.
 */
object MuxTest extends App {
  chisel3.emitVerilog(new Multiplexer(6, 32), args)
  org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
    behavior of "Multiplexer"
    it should "work" in {
      test(new Multiplexer(6, 32)).runPeekPoke(new MultiplexerUnitTester(_))
    }
  })
}

/** A tester of a synchronizer with skew = 3.
 *
 * @param c the synchronizer
 */
class SynchronizerTester(c: SkewSynchronizer) extends PeekPokeTester(c) {
  def safePoke(data: Data, value: BigInt): Unit ={
    if(USE_TOKEN){
      poke(data.asInstanceOf[TokenIO].data, value)
      poke(data.asInstanceOf[TokenIO].token, true)
    }else{
      poke(data.asInstanceOf[UInt], value)
    }
  }

  def safePeek(data: Data)={
    if(USE_TOKEN){
      val token = peek(data.asInstanceOf[TokenIO].token)
      if(token == 0){
        println("Invalid data!!!")
      }
      peek(data.asInstanceOf[TokenIO].data)
    }else{
      peek(data.asInstanceOf[UInt])
    }
  }


  poke(c.io.skewing, 3)
  safePoke(c.io.input0, 2)
  safePoke(c.io.input1, 3)

  for (i <- 0 until 10) {
    safePoke(c.io.input0, i)
    safePoke(c.io.input1, i + 1)
    println(safePeek(c.io.skewedInput0).toString())
    println(safePeek(c.io.skewedInput1).toString())
    step(1)
  }
}

/** A object invoking the tester of a synchronizer.
 */
object SkewTest extends App {
  org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
    behavior of "SkewSynchronizer"
    it should "work" in {
      test(new SkewSynchronizer(32)).runPeekPoke(new SynchronizerTester(_))
    }
  })
}
