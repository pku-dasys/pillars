module Adder( // @[:@3.2]
  input  [31:0] io_input_a, // @[:@6.4]
  input  [31:0] io_input_b, // @[:@6.4]
  output [31:0] io_out // @[:@6.4]
);
  wire [32:0] _T_11; // @[Flow.scala 17:24:@8.4]
  assign _T_11 = io_input_a + io_input_b; // @[Flow.scala 17:24:@8.4]
  assign io_out = io_input_a + io_input_b; // @[Flow.scala 17:10:@10.4]
endmodule
module Multiplier( // @[:@21.2]
  input  [31:0] io_input_a, // @[:@24.4]
  input  [31:0] io_input_b, // @[:@24.4]
  output [31:0] io_out // @[:@24.4]
);
  wire [63:0] _T_11; // @[Flow.scala 28:24:@26.4]
  assign _T_11 = io_input_a * io_input_b; // @[Flow.scala 28:24:@26.4]
  assign io_out = _T_11[31:0]; // @[Flow.scala 28:10:@27.4]
endmodule
module TopMo( // @[:@29.2]
  input         clock, // @[:@30.4]
  input         reset, // @[:@31.4]
  input  [31:0] io_input_0, // @[:@32.4]
  input  [31:0] io_input_1, // @[:@32.4]
  output [31:0] io_out // @[:@32.4]
);
  wire [31:0] Adder_io_input_a; // @[Flow.scala 40:57:@34.4]
  wire [31:0] Adder_io_input_b; // @[Flow.scala 40:57:@34.4]
  wire [31:0] Adder_io_out; // @[Flow.scala 40:57:@34.4]
  wire [31:0] Adder_1_io_input_a; // @[Flow.scala 40:57:@37.4]
  wire [31:0] Adder_1_io_input_b; // @[Flow.scala 40:57:@37.4]
  wire [31:0] Adder_1_io_out; // @[Flow.scala 40:57:@37.4]
  wire [31:0] Multiplier_io_input_a; // @[Flow.scala 42:55:@40.4]
  wire [31:0] Multiplier_io_input_b; // @[Flow.scala 42:55:@40.4]
  wire [31:0] Multiplier_io_out; // @[Flow.scala 42:55:@40.4]
  Adder Adder ( // @[Flow.scala 40:57:@34.4]
    .io_input_a(Adder_io_input_a),
    .io_input_b(Adder_io_input_b),
    .io_out(Adder_io_out)
  );
  Adder Adder_1 ( // @[Flow.scala 40:57:@37.4]
    .io_input_a(Adder_1_io_input_a),
    .io_input_b(Adder_1_io_input_b),
    .io_out(Adder_1_io_out)
  );
  Multiplier Multiplier ( // @[Flow.scala 42:55:@40.4]
    .io_input_a(Multiplier_io_input_a),
    .io_input_b(Multiplier_io_input_b),
    .io_out(Multiplier_io_out)
  );
  assign io_out = Adder_1_io_out; // @[Flow.scala 60:32:@46.4]
  assign Adder_io_input_a = io_input_0; // @[Flow.scala 62:56:@49.4]
  assign Adder_io_input_b = io_input_1; // @[Flow.scala 62:56:@43.4]
  assign Adder_1_io_input_a = io_input_1; // @[Flow.scala 62:56:@44.4]
  assign Adder_1_io_input_b = Multiplier_io_out; // @[Flow.scala 64:56:@47.4]
  assign Multiplier_io_input_a = io_input_0; // @[Flow.scala 62:56:@48.4]
  assign Multiplier_io_input_b = Adder_io_out; // @[Flow.scala 64:56:@45.4]
endmodule
