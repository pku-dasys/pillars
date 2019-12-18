package tetriski.pillars

import chisel3.iotesters.PeekPokeTester

class TopModule2PEUnitTest(c: TopModule) extends PeekPokeTester(c) {
  //MixedVec don't support c.io.inputs(0) in poke
  poke(c.input_0, 2)
  poke(c.input_1, 3)
  //010 001 001 0001 010 001 001 0000// PE1(13) PE013)
  //PE0: 010 001 110 0000 //  mux0(3) mux1(3) register(3)(rf(1) -> out1(output), rf(1) -> out0(to self), input -> rf(0)) alu(4)
  //PE1: 010 001 110 0001
  //0100011100000
  //save (a+b) in pe0.rf(0), to next cycle //5
  //save (b-a) in pe1.rf(0), to next cycle //1
  poke(c.io.configuration, 18622688)
  expect(c.out, 0)
  expect(c.io.configTest(0), 2272)
  expect(c.io.configTest(1), 2273)
  step(1)
  expect(c.out, 0)
  //001 100 101 0011 001 100 101 0011
  //PE0: 001 100 101 0011 //output rf(1)=7
  //PE1: 001 100 101 0011
  //0011001010011
  // pe0.rf(1) = pe0.rf(0) or a // 5 or 2 =7
  // pe1.rf(1) = pe1.rf(0) or b // 1 or 3 =3
  poke(c.io.configuration, 13264467)
  expect(c.io.configTest(0), 1619)
  expect(c.io.configTest(1), 1619)
  //  step(1)
  //  expect(c.out, 2) //0 or 2 due to SyncReadMem
  step(1)
  expect(c.out, 7)
  //000 000 100 0000 011 100 010 0000
  //PE0: 011 100 010 0000 //
  //PE1: 000 000 100 0000 //output rf(1) to pe0
  //0000001000000 0111000100000
  // pe0.rf(0) = pe0.rf(1) + pe1.rf(1) // 7 + 3 = 10
  poke(c.io.configuration, 527904)
  expect(c.io.configTest(0), 3616)
  expect(c.io.configTest(1), 64)
  //  step(1)
  //  expect(c.out, 8)// 1 + 7 due to SyncReadMem
  step(1)
  expect(c.out, 10)
  step(1)
  expect(c.out, 10)
}

class TopModuleAdresUnitTest(c: TopModule, bitstream :BigInt) extends PeekPokeTester(c) {
  //MixedVec don't support c.io.inputs(0) in poke
//  poke(c.input_0, 2)
//  poke(c.input_1, 3)
  println(bitstream.toString())

  poke(c.io.configuration, bitstream)
  expect(c.io.configTest(0), 0)
  expect(c.io.configTest(1), 182341)

  for( i <- 0 until 40){
//    println("cycle "+ i.toString)
    poke(c.input_1, i)
    if(i > 12)
    expect(c.out, 5 * (i - 12 + 4))
    step(1)
  }
}

class DispatchUnitTest(c: DispatchT, bitstream :BigInt) extends PeekPokeTester(c) {
  //MixedVec don't support c.io.inputs(0) in poke
  poke(c.io.configuration, bitstream)
  println(bitstream.toString())

  expect(c.outt, 3616)
  step(1)
}
