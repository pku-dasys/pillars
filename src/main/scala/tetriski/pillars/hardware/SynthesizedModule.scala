package tetriski.pillars.hardware

import chisel3.util._
import chisel3.{Bundle, Input, Module, Output, UInt, _}
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{ConstInfo, CounterInfo, CounterParameter, OpEnum, OpcodeTranslator}
import tetriski.pillars.mapping.{DFG, OpNode}
import tetriski.pillars.hardware.PillarsConfig._

/** A simple adder.
 *
 * @deprecated
 * @param w the data width
 */
class Adder(w: Int) extends Module {
  val io = IO(new Bundle {
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })
  val input_a = io.inputs(0)
  val input_b = io.inputs(1)
  val out = io.outs(0)

  out := input_a + input_b
}

/** A simple multiplier.
 *
 * @deprecated
 * @param w the data width
 */
class Multiplier(w: Int) extends Module {
  val io = IO(new Bundle {
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt((w).W))))
  })

  val input_a = io.inputs(0)
  val input_b = io.inputs(1)
  val out = io.outs(0)

  out := input_a * input_b
}

/** A simple dual-input module.
 *
 * @param w      the data width
 * @param opEnum opcode of this module
 */
class DualInputModule(w: Int, opEnum: OpEnum) extends Module {
  val io = IO(new Bundle {
    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt((w).W))))
  })

  val input_a = io.inputs(0)
  val input_b = io.inputs(1)
  val out = io.outs(0)

  val logic = opEnum match {
    case OpEnum.ADD => input_a + input_b
    case OpEnum.SUB => input_a - input_b
    case OpEnum.AND => input_a & input_b
    case OpEnum.OR => input_a | input_b
    case OpEnum.XOR => input_a ^ input_b
    case OpEnum.MUL => input_a * input_b
    case OpEnum.SLT => input_a.asSInt < input_b.asSInt
    case OpEnum.SHLL => (input_a << input_b(log2Up(w), 0).asUInt).asUInt()
    case OpEnum.SLTU => input_a < input_b
    case OpEnum.SHRL => (input_a >> input_b(log2Up(w), 0).asUInt).asUInt()
    case OpEnum.SHRA => (input_a.asSInt >> input_b(log2Up(w), 0).asUInt).asUInt
    case OpEnum.DIV => input_a / input_b
  }

  out := logic
}

/** A simple const unit.
 *
 * @param value the const value
 * @param w     the data width
 */
class SynthesizedConstUnit(value: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  val reg = RegInit(value.U(w.W))
  io.outs(0) := reg
}

/** A configuration-fixed counter.
 *
 * @param config   the configuration of counter
 * @param fireTime the firing time of this counter
 * @param w        the data width
 */
class SynthesizedCounter(config: BigInt, fireTime: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  val counter = Module(new Counter(w / 4))
  counter.io.en := false.B
  if (fireTime > 0) {
    val cycleReg = RegInit(0.U(log2Ceil(fireTime).W))
    when(cycleReg < fireTime.U) {
      cycleReg := cycleReg + 1.U
    }.otherwise {
      counter.io.en := true.B
    }
  } else {
    counter.io.en := true.B
  }
  counter.io.configuration := config.U
  io.outs(0) := counter.io.outs(0)
}

/** A chain of registers.
 *
 * @param num the num of registers
 * @param w   the data width
 */
class Registers(num: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val inputs = Input(MixedVec(Seq(UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  val regs = (0 until num).toArray.map(t => RegInit(0.U(w.W)))
  for (t <- 0 until num - 1) {
    regs(t + 1) := regs(t)
  }
  regs(0) := io.inputs(0)
  io.outs(0) := regs(num - 1)
}

/** A simple load unit.
 *
 * @param memData the prestored data in memory
 * @param w       the data width
 */
class SynthesizedLoadUnit(memData: Array[Int], w: Int) extends Module {
  val io = IO(new Bundle {
    val inputs = Input(MixedVec(Seq(UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  val mem = Mem(memData.size, UInt(w.W))
  for (i <- 0 until memData.size) {
    mem.write(i.U(w.W), memData(i).U(w.W))
  }
  io.outs(0) := mem.read(io.inputs(0))
}

/** A simple store unit.
 *
 * @param w the data width
 */
class SynthesizedStoreUnit(w: Int) extends Module {
  val io = IO(new Bundle {
    val bAddr = Input(UInt(w.W))
    val bDout = Output(UInt(w.W))

    val inputs = Input(MixedVec(Seq(UInt(w.W), UInt(w.W))))
    val outs = Output(MixedVec(Seq()))
  })

  val mem = Mem(PillarsConfig.MEM_DEPTH, UInt(w.W))

  mem.write(io.inputs(0), io.inputs(1))

  io.bDout := RegNext(mem.read(io.bAddr))

}

/** The synthesized design for a mapped DFG.
 * When using this module, the origin CGRA architecture plays the role of virtual overlay.
 * Some may call it "soft CGRA", which is used for quick and high-quality compiler.
 *
 * This Module is only tested when II = 1.
 *
 * @param dfg         a mapped DFG
 * @param constInfo   the const values of each const unit
 * @param counterInfo the paramters of each counter
 * @param memDatas    the data arrays in the memory of each load unit
 * @param w           the data width
 */
class SynthesizedModule(dfg: DFG, constInfo: ConstInfo, counterInfo: CounterInfo,
                        memDatas: Array[Array[Int]], w: Int) extends Module {
  /** Get the number of nodes with a opcode in the DFG.
   *
   * @param op the opcode
   */
  def getNum(op: OpEnum): Int = {
    dfg.opNodes.map(node => if (node.opcode == op) {
      1
    } else {
      0
    }).sum
  }

  /** Get a specified port of the module corresponding a specified node.
   *
   * @param index   the index of the node
   * @param operand the operand of the port
   * @param isInput a parameter indicating whether this port is a input port
   * @return the port
   */
  def getPort(index: Int, operand: Int = 0, isInput: Boolean = false): Data = {
    /** Add necessary registers if skew of a mapped node is not 0,
     * and get the port should be used for connection.
     *
     * @param node the node
     * @param port the original port
     * @return the port should be used for connection
     */
    def skewCheck(node: OpNode, port: Data): Data = {
      if (USE_RELATIVE_SKEW) {
        var operandUsed = operand
        if (node.commutated) {
          operandUsed = 1 - operandUsed
        }
        if (node.skew < 0 && operandUsed == 0) {
          val skewReg = Module(new Registers(-node.skew, w))
          port := skewReg.io.outs(0)
          skewReg.io.inputs(0)
        } else if (node.skew > 0 && operandUsed == 1) {
          val skewReg = Module(new Registers(node.skew, w))
          port := skewReg.io.outs(0)
          skewReg.io.inputs(0)
        } else {
          port
        }
      } else {
        val mod = 1 << LOG_SKEW_LENGTH
        val skew0 = node.skew % mod
        val skew1 = (node.skew - skew0) / mod
        if (operand == 0) {
          if (skew0 == 0) {
            port
          } else {
            val skewReg = Module(new Registers(skew0, w))
            port := skewReg.io.outs(0)
            skewReg.io.inputs(0)
          }
        } else if (operand == 1) {
          if (skew1 == 0) {
            port
          } else {
            val skewReg = Module(new Registers(skew1, w))
            port := skewReg.io.outs(0)
            skewReg.io.inputs(0)
          }
        } else {
          port
        }
      }
    }

    val node = dfg.opNodes(index)
    val op = node.opcode
    if (op == OpEnum.INPUT) {
      return inputs(index)
    } else if (op == OpEnum.OUTPUT) {
      return outputs(index)
    } else if (op == OpEnum.LOAD) {
      if (isInput) {
        return loadUnits(index).io.inputs(0)
      } else {
        return RegNext(loadUnits(index).io.outs(0))
      }
    } else if (op == OpEnum.CONST) {
      return constUnits(index).io.outs(0)
    }else if (op == OpEnum.INCR) {
      return counters(index).io.outs(0)
    } else if (op == OpEnum.STORE) {
      val port = storeUnits(index).io.inputs(operand)
      return skewCheck(node, port)
    } else {
      if (!OpcodeTranslator.aluOpcodeList.contains(op)) {
        throw new Exception("Opcode of " + node.name + " is not undefined during synthesizing!")
      }
      if (isInput) {
        val port = dualInputModule(index).io.inputs(operand)
        return skewCheck(node, port)
      } else {
        return dualInputModule(index).io.outs(0)
      }
    }
  }

  if (!dfg.synthesizable) {
    throw new Exception("DFG is not synthesizable!")
  }

  val inputNum = getNum(OpEnum.INPUT)
  val outputNum = getNum(OpEnum.OUTPUT)
  val addNum = getNum(OpEnum.ADD)
  val mulNum = getNum(OpEnum.MUL)
  val loadNum = getNum(OpEnum.LOAD)
  val storeNum = getNum(OpEnum.STORE)
  val constNum = getNum(OpEnum.CONST)

  val io = IO(new Bundle {
    val storeUnitMemAddrs = Input(MixedVec((1 to storeNum) map { i => UInt(w.W) }))
    val storeUnitMemDatas = Output(MixedVec((1 to storeNum) map { i => UInt(w.W) }))

    val inputs = Input(MixedVec((1 to inputNum) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to outputNum) map { i => UInt(w.W) }))
  })

  var dualInputModule = Map[Int, DualInputModule]()
  var constUnits = Map[Int, SynthesizedConstUnit]()
  var counters = Map[Int, SynthesizedCounter]()
  var loadUnits = Map[Int, SynthesizedLoadUnit]()
  var storeUnits = Map[Int, SynthesizedStoreUnit]()
  var inputs = Map[Int, Data]()
  var outputs = Map[Int, Data]()

  var constCount = 0
  var counterCount = 0
  var loadUnitCount = 0
  var storeUnitCount = 0
  var inputCount = 0
  var outputCount = 0
  for (i <- 0 until dfg.opNodes.size) {
    val node = dfg.opNodes(i)
    if (node.opcode == OpEnum.CONST) {
      val constValue = constInfo.configArray(0)(constCount)
      val module = Module(new SynthesizedConstUnit(constValue.toInt, w))
      constUnits += (i -> module)
      constCount += 1
    }else if(node.opcode == OpEnum.INCR){
      val config = counterInfo.configArray(0)(counterCount)
      val fireTime = node.latency
      val module = Module(new SynthesizedCounter(config.toInt, fireTime, w))
      counters += (i -> module)
      counterCount += 1
    } else if (node.opcode == OpEnum.LOAD) {
      val memData = memDatas(loadUnitCount)
      val module = Module(new SynthesizedLoadUnit(memData, w))
      loadUnits += (i -> module)
      loadUnitCount += 1
    } else if (node.opcode == OpEnum.STORE) {
      val module = Module(new SynthesizedStoreUnit(w))
      module.io.bAddr := io.storeUnitMemAddrs(storeUnitCount)
      io.storeUnitMemDatas(storeUnitCount) := module.io.bDout
      storeUnits += (i -> module)
      storeUnitCount += 1
    } else if (OpcodeTranslator.aluOpcodeList.contains(node.opcode)) {
      val module = Module(new DualInputModule(w, node.opcode))
      dualInputModule += (i -> module)
    } else if (node.opcode == OpEnum.INPUT) {
      val latency = node.latency
      if (latency > 0) {
        val inputRegs = Module(new Registers(latency, w))
        inputRegs.io.inputs(0) := io.inputs(inputCount)
        inputs += (i -> inputRegs.io.outs(0))
      } else {
        inputs += (i -> io.inputs(inputCount))
      }
      inputCount += 1
    } else if (node.opcode == OpEnum.OUTPUT) {
      val latency = node.latency
      val maxLatency = dfg.opNodes.map(n => if (n.opcode == OpEnum.OUTPUT) {
        n.latency
      } else {
        0
      }).max
      val latencyDiff = maxLatency - latency
      if (latencyDiff > 0) {
        val outputRegs = Module(new Registers(latencyDiff, w))
        io.outs(outputCount) := outputRegs.io.outs(0)
        outputs += (i -> outputRegs.io.inputs(0))
      } else {
        outputs += (i -> io.outs(outputCount))
      }
      outputCount += 1
    }
  }

  val regs = (0 until dfg.regNum).toArray.map(t => RegInit(0.U(w.W)))

  for (connect <- dfg.regConnect) {
    val source = connect._1
    for (i <- 0 until connect._2.size()) {
      val sink = connect._2.get(i)
      regs(sink) := regs(source)
    }
  }

  for (connect <- dfg.func2regMap) {
    val source = connect._1
    for (i <- 0 until connect._2.size()) {
      val sink = connect._2.get(i)
      regs(sink) := getPort(source)
    }
  }

  for (connect <- dfg.reg2funcMap) {
    val source = connect._1
    for (i <- 0 until connect._2.size()) {
      val sink = connect._2.get(i).get(0)
      val sinkOperand = connect._2.get(i).get(1)
      getPort(sink, sinkOperand, true) := regs(source)
    }
  }

  for (connect <- dfg.funcDirect2funcMap) {
    val source = connect._1
    for (i <- 0 until connect._2.size()) {
      val sink = connect._2.get(i).get(0)
      val sinkOperand = connect._2.get(i).get(1)
      getPort(sink, sinkOperand, true) := getPort(source)
    }
  }

}
