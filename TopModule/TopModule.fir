;buildInfoPackage: chisel3, version: 3.1.8, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2019-07-08 17:44:42.884, builtAtMillis: 1562607882884
circuit TopModule : 
  module Adder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input_a : UInt<32>, flip input_b : UInt<32>, out : UInt<32>}
    
    node _T_11 = add(io.input_a, io.input_b) @[TopModule.scala 15:24]
    node _T_12 = tail(_T_11, 1) @[TopModule.scala 15:24]
    io.out <= _T_12 @[TopModule.scala 15:10]
    
  module Adder_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input_a : UInt<32>, flip input_b : UInt<32>, out : UInt<32>}
    
    node _T_11 = add(io.input_a, io.input_b) @[TopModule.scala 15:24]
    node _T_12 = tail(_T_11, 1) @[TopModule.scala 15:24]
    io.out <= _T_12 @[TopModule.scala 15:10]
    
  module Multiplier : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input_a : UInt<32>, flip input_b : UInt<32>, out : UInt<32>}
    
    node _T_11 = mul(io.input_a, io.input_b) @[TopModule.scala 26:24]
    io.out <= _T_11 @[TopModule.scala 26:10]
    
  module TopModule : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip input_0 : UInt<32>, flip input_1 : UInt<32>, out : UInt<32>}
    
    inst Adder of Adder @[TopModule.scala 38:57]
    Adder.clock <= clock
    Adder.reset <= reset
    inst Adder_1 of Adder_1 @[TopModule.scala 38:57]
    Adder_1.clock <= clock
    Adder_1.reset <= reset
    inst Multiplier of Multiplier @[TopModule.scala 40:55]
    Multiplier.clock <= clock
    Multiplier.reset <= reset
    Adder.io.input_b <= io.input_1 @[TopModule.scala 60:56]
    Adder_1.io.input_b <= io.input_1 @[TopModule.scala 60:56]
    Multiplier.io.input_b <= Adder.io.out @[TopModule.scala 62:56]
    io.out <= Adder_1.io.out @[TopModule.scala 58:32]
    Adder_1.io.input_a <= Multiplier.io.out @[TopModule.scala 62:56]
    Adder.io.input_a <= io.input_0 @[TopModule.scala 60:56]
    Multiplier.io.input_a <= io.input_0 @[TopModule.scala 60:56]
    
