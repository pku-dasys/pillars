package pillars.examples

import pillars.core._
import pillars.mapping.thirdParty._
import pillars.archlib.TileLSUBlock
import pillars.hardware.TopModule
import pillars.archlib._
import pillars.core.{BlockTrait, OpEnum}
import pillars.core.OpEnum.OpEnum
import pillars.mapping.{DFG, DotReader, ILPMap, OmtMap, SearchMap}
import pillars.testers.{AppTestHelper, ApplicationTester}

import scala.util.Random
import java.lang.Math
import java.util.Date

import chisel3.iotesters

class SimpleBlockWithReg(name: String, isRegion: Boolean = false) extends BlockTrait {
  initName(name)
  addInPorts(Array("input_0","input_1"))
  addOutPorts(Array("out_0"))

  if(isRegion){
    setConfigRegion()
  }

  // Initialize ALU supporting ADD/SUB
  val aluOpList = List(OpEnum.ADD, OpEnum.SUB)
  val supBypass = false
  val aluParams = List(32) // 32 bit width
  val alu0 = new ElementAlu(name+"_ALU", aluOpList, supBypass, List(32))
  alu0.addInPorts(Array("input_A", "input_B"))
  alu0.addOutPorts(Array("out_0"))
  addElement(alu0)

  // A register file with 2 registers
  val rf0 = new ElementRF("rf0", List(1, 1, 1, 32))
  //port sequnces outs: 0: out_0
  //port sequnces inputs: 0: input_0
  rf0.addOutPorts(Array("out_0"))
  rf0.addInPorts(Array("input_0"))
  addElement(rf0)

  // Initialize internal connections
  addConnect(term("input_0") -> alu0 / "input_A")
  addConnect(term("input_1") -> alu0 / "input_B")
  addConnect(alu0 / "out_0" -> rf0 / "input_0")
  addConnect(rf0 / "out_0" -> term("out_0"))
}


object SimpleAddExample {
  def prepareRuntimeInfoAdd(dfg: DFG) = {
    val dataSize = 10
    val A = (0 until dataSize).map(i => i).toList
    val B = (0 until dataSize).map(i => i).toList
    val expectedRet = (0 until dataSize).map(i => A(i) + B(i))

    //Please make sure there are 2 operators with INPUT opcode in the DFG.
    val inputOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.INPUT).map(op => op.name)
    val inputToPort = List(InputToPort(inputOpNames(0), A), InputToPort(inputOpNames(1), B))

    val outputOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.OUTPUT).map(op => op.name)
    val outputFromPort = List(OutputFromPort(outputOpNames(0), expectedRet.toList))

    val emptyList = List()

    val runtimeInfo = RuntimeInfo(inputToPort, outputFromPort, emptyList
      , emptyList, emptyList, emptyList)

    runtimeInfo
  }


  def main(args: Array[String]): Unit = {
    val inputPort = 2
    val outputPort = 1
    val dataWidth = 32

    //Initialize the top block.
    val arch = new ArchitectureHierarchy()
    arch.addInPorts((0 until inputPort).map(i => s"input_$i").toArray)
    arch.addOutPorts((0 until outputPort).map(i => s"out_$i").toArray)

    val block = new SimpleBlockWithReg("Block0")
    arch.addBlock(block)

    (0 until inputPort).foreach(i =>
      arch.addConnect(arch.term(s"input_$i") -> block / s"input_$i"))
    (0 until outputPort).foreach(i =>
      arch.addConnect(block / s"out_$i" -> arch.term(s"out_$i")))
    arch.init()

    val dfgFilename = "./tutorial/Add.dot"
    val II = 2
    val MRRG = arch.getMRRG(II)
    val dfg = DotReader.loadDot(dfgFilename, II)
    val mappingResultFilename = s"./tutorial/Add_ii$II"

    object Solver extends Enumeration {
      val Gurobi, Search, Z3Prover = Value
    }
    val solver = Solver.Gurobi
    val separatedPR = true
    val scheduleControl = true

    var startTime = new Date().getTime()
    solver match {
      case Solver.Gurobi => ILPMap.mapping(dfg, MRRG, filename = mappingResultFilename, separatedPR = separatedPR, scheduleControl = scheduleControl, skewLimit = 4, latencyLimit = 15)
      case Solver.Search => SearchMap.mapping(dfg, MRRG, mappingResultFilename, scheduleControl = scheduleControl, skewLimit = 4)
      case Solver.Z3Prover => OmtMap.mapping(dfg, MRRG, filename = mappingResultFilename, separatedPR = separatedPR, scheduleControl = scheduleControl, skewLimit = 4, latencyLimit = 15)
    }
    var endTime = new Date().getTime()
    println("Mapping runtime: " + (endTime - startTime))

    prepareRuntimeInfoAdd(dfg)
    JsonParser.writeJson(prepareRuntimeInfoAdd(dfg), "runtime.json")
    val runtimeInfo = JsonParser.readJson("runtime.json", print = true)

    //Simulation settings.
    val simulationHelper = new SimulationHelper(arch)
    val resultFilename = s"./tutorial/Add_ii$II" + "_r.txt"
    simulationHelper.init(resultFilename, runtimeInfo, II, print = true)

    val appTestHelper = new AppTestHelper(II)
    val moduleInfoFilename = s"./tutorial/Add_ii$II" + "_i.txt"
    appTestHelper.init(arch, simulationHelper, moduleInfoFilename, runtimeInfo, print = true)

    //Generate the top design.
    val connect = new Connect(arch.connectArray)
    val hardwareGenerator = new HardwareGenerator(arch, connect)
    val topDesign = () => new TopModule(hardwareGenerator.pillarsModuleInfo,
      hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)

    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), topDesign) {
      c => new AddTester(c, appTestHelper)
    }
  }
}

/** A tester for matrix multiplication.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class AddTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c, appTestHelper) {
  poke(c.io.en, 0)
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)
  checkPortOutsWithInput(testII)
}
