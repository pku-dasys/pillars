package tetriski.pillars.hardware

import chisel3.util._
import chisel3.{Bundle, Input, Module, Output, UInt, _}
import org.scalacheck.Prop.True
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{ConstInfo, OpEnum}
import tetriski.pillars.mapping.{DFG, OpNode}
import tetriski.pillars.util.{SimpleDualPortMem, SimpleDualPortSram, SinglePortSram}

import scala.collection.mutable.ArrayBuffer

/** A simple adder.
 *
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
    //    val aDin = Input(UInt(w.W))
    //    val aAddr = Input(UInt(w.W))
    //    val aWe = Output(Bool())

    val inputs = Input(MixedVec(Seq(UInt(w.W))))
    val outs = Output(MixedVec(Seq(UInt(w.W))))
  })

  //  val mem = Module(new SimpleDualPortSram(PillarsConfig.MEM_DEPTH, w))
  //  mem.io.b.en := true.B
  //  mem.io.b.addr := io.inputs(0)
  //  mem.io.a.din := io.aDin
  //  mem.io.a.we := io.aWe
  //  mem.io.a.addr := io.aAddr

  val mem = Mem(memData.size, UInt(w.W))
  for(i <- 0 until memData.size){
    mem.write(i.U(w.W), memData(i).U(w.W))
  }
  val dout = RegInit(0.U(w.W))

  dout := mem.read(io.inputs(0))
  io.outs(0) := dout
}

class SynthesizedModule(dfg: DFG, constInfo: ConstInfo, memDatas: Array[Array[Int]], w: Int) extends Module {
  def getNum(op: OpEnum): Int = {
    dfg.opNodes.map(node => if (node.opcode == op) {
      1
    } else {
      0
    }).sum
  }

  def getPort(index: Int, operand: Int = 0, isInput: Boolean = false): Data = {
    def skewCheck(node: OpNode, port: Data): Data = {
      if (node.skew < 0 && operand == 0) {
        val skewReg = Module(new Registers(-node.skew, w))
        port := skewReg.io.outs(0)
        skewReg.io.inputs(0)
      } else if (node.skew > 0 && operand == 1) {
        val skewReg = Module(new Registers(node.skew, w))
        port := skewReg.io.outs(0)
        skewReg.io.inputs(0)
      } else {
        port
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
        return loaders(index).io.inputs(0)
      } else {
        return loaders(index).io.outs(0)
      }
    } else if (op == OpEnum.CONST) {
      return constUnits(index).io.outs(0)
    } else if (op == OpEnum.ADD) {
      if (isInput) {
        val port = adders(index).io.inputs(operand)
        return skewCheck(node, port)
      } else {
        return adders(index).io.outs(0)
      }
    } else {
      if (op != OpEnum.MUL) {
        throw new Exception("Opcode of " + node.name + " is not undefined during synthesizing!")
      }
      if (isInput) {
        val port = multipliers(index).io.inputs(operand)
        return skewCheck(node, port)
      } else {
        return multipliers(index).io.outs(0)
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
  val constNum = getNum(OpEnum.CONST)

  val io = IO(new Bundle {
    val inputs = Input(MixedVec((1 to inputNum) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to outputNum) map { i => UInt(w.W) }))
  })

  var adders = Map[Int, Adder]()
  var multipliers = Map[Int, Multiplier]()
  var constUnits = Map[Int, SynthesizedConstUnit]()
  var loaders = Map[Int, SynthesizedLoadUnit]()
  var inputs = Map[Int, Data]()
  var outputs = Map[Int, Data]()

  var constCount = 0
  var loaderCount = 0
  var inputCount = 0
  var outputCount = 0
  for (i <- 0 until dfg.opNodes.size) {
    val node = dfg.opNodes(i)
    if (node.opcode == OpEnum.CONST) {
      val constValue = constInfo.constValArray(0)(constCount)
      val module = Module(new SynthesizedConstUnit(constValue, w))
      constUnits += (i -> module)
      constCount += 1
    } else if (node.opcode == OpEnum.LOAD) {
      val memData = memDatas(loaderCount)
      val module = Module(new SynthesizedLoadUnit(memData, w))
      loaders += (i -> module)
      loaderCount += 1
    } else if (node.opcode == OpEnum.ADD) {
      val module = Module(new Adder(w))
      adders += (i -> module)
    } else if (node.opcode == OpEnum.MUL) {
      val module = Module(new Multiplier(w))
      multipliers += (i -> module)
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

  for(connect <- dfg.funcDirect2funcMap){
    val source = connect._1
    for (i <- 0 until connect._2.size()) {
      val sink = connect._2.get(i).get(0)
      val sinkOperand = connect._2.get(i).get(1)
      getPort(sink, sinkOperand, true) := getPort(source)
    }
  }

}
