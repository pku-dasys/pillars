package pillars.examples

import java.util.Date
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester
import pillars.archlib.TileLSUBlock
import pillars.core._
import pillars.hardware.{PillarsConfig, SynthesizedModule, TopModule}
import pillars.mapping.{DFG, DotReader, ILPMap, OmtMap, SearchMap}
import pillars.testers.{AppTestHelper, ApplicationTester}

import scala.util.Random

/** An end2end tutorial of Pillars.
 * Example: a DFG contains vector addition and vector reverse.
 * In the simulation, we can get A(i) + B(i) form output port every II cycle during activating process,
 * and get A.reverse during post-process.
 */
object Tutorial {
  def main(args: Array[String]): Unit = {
    /** Prepare runtime information manually.
     *
     * @param dfg     the data-flow graph
     * @param numSRAM the number of SRAM in a CGRA tile
     * @return the runtime information
     */
    def prepareRuntimeInfo(dfg: DFG, numSRAM: Int) = {
      val dataSize = 50
//            val VectorA = (0 until dataSize).toArray
//            val VectorB = (100 until dataSize + 100).toArray
      val VectorA = (0 until dataSize).map(_ => Math.abs(scala.util.Random.nextInt() % 1000)).toArray
      val VectorB = (0 until dataSize).map(_ => Math.abs(scala.util.Random.nextInt() % 1000)).toArray

      //Input random indexes into the mapped input port in CGRA,
      // and get A(index) + B(index) from the mapped output port.
      val inputIndexes = Random.shuffle((0 until dataSize).toList)
//            val inputIndexes = (0 until dataSize).toList.reverse
      val expectedRet = (0 until dataSize).map(i => VectorA(inputIndexes(i)) + VectorB(inputIndexes(i)))

      //The base address of A and B in SRAM of an LSU.
      //To simplify the problem, we assume both A and B are stored
      //in all SRAMs belonging to 4 LSUs in the targeted architecture.
      val a_base = 0
      val b_base = dataSize

      //The value of const operators.
      val const0 = a_base
      val const1 = b_base
      val const2 = dataSize - 1
      val const3 = a_base
      val const4 = a_base
      val constVals = Array(const0, const1, const2, const3, const4)

      val constOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.CONST).map(op => op.name)
      val constValue = (0 until constOpNames.size).map(i => ConstValue(constOpNames(i), constVals(i))).toList

      //Operator incr0 should generate (j <- 0 until dataSize).
      //So the parameter of the counter is (init = 0, step = 1, end = dataSize, freq = 1)
      val counterOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.INCR).map(op => op.name)
      val counterConfig = List(CounterConfig(counterOpNames(0), 0, 1, dataSize, 1))

      //In this simple tutorial, A and B are put into all LSUs.
      //But you can put them into partial LSUs according to the mapping results,
      // just like what in the ApplicationExamples.
      //Because the PEs in a row share an LSU, the number of LSUs is rowNum.
      val inputToSRAM = (0 until numSRAM).map(i => InputToSRAM(i, a_base, VectorA.toList)).toList :::
        (0 until numSRAM).map(i => InputToSRAM(i, b_base, VectorB.toList)).toList

      val outputFromSRAM = List(OutputFromSRAM(3, a_base, VectorA.reverse.toList))

      //Please make sure there are 2 operators with INPUT opcode in the DFG.
      val inputOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.INPUT).map(op => op.name)
      //      val inputToPortData = List(inputI, inputJ)
      //      val inputToPort = (0 until inputOpNames.size).map(i => InputToPort(inputOpNames(i),
      //        inputToPortData(i).toList)).toList
      val inputToPort = List(InputToPort(inputOpNames(0),
        inputIndexes))

      val outputOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.OUTPUT).map(op => op.name)
      //      val outputFromPort = List(OutputFromPort(outputOpNames(0), outResult.toList))
      val outputFromPort = List(OutputFromPort(outputOpNames(0), expectedRet.toList))

      val runtimeInfo = RuntimeInfo(inputToPort, outputFromPort, inputToSRAM
        , outputFromSRAM, constValue, counterConfig)

      runtimeInfo
    }

    /** Test synthesizing wire-fixed RTL.
     * !!NOTE: Only work when II = 1 and using ILP mapper!!
     *
     * @param mappedDfg        the mapped data-flow graph
     * @param simulationHelper a class that helps users to automatically generate simulation codes
     * @param dataWidth        the data width of the architecture
     * @param runtimeInfo      the runtime information
     */
    def testSynthesize(mappedDfg: DFG, simulationHelper: SimulationHelper, dataWidth: Int, runtimeInfo: RuntimeInfo) = {

      val constInfo = simulationHelper.constInfo
      val counterInfo = simulationHelper.counterInfo
      val inputToSRAM = runtimeInfo.inputToSRAM
      val size = mappedDfg.fixedMapSRAM.map(p => p._2).toSet.size
      val inputSArray = (0 until size).map(i => inputToSRAM.filter(d => d.SRAMID == i).map(j => j.data)
        .reduce(_ ::: _).toArray).toArray

      val outputFromSRAM = runtimeInfo.outputFromSRAM


      val synthesizedDesign = () =>
        new SynthesizedModule(mappedDfg, constInfo, counterInfo, inputSArray, dataWidth)

      chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), synthesizedDesign)

      val inputToPort = runtimeInfo.inputToPort
      val outputFromPort = runtimeInfo.outputFromPort
      iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), synthesizedDesign) {
        c =>
          new SynthesizedModuleTester(c, inputToPort(0).data.toArray, outputFromPort(0).expectedData.toArray,
            outputFromSRAM(0).expectedData.toArray, simulationHelper.getOutputCycle())
      }
    }

    val rowNum = 4
    val colNum = 4
    val inputPort = 4
    val outputPort = 4
    val dataWidth = 32

    //Initialize the top block.
    val arch = new ArchitectureHierarchy()
    arch.addInPorts((0 until inputPort).map(i => s"input_$i").toArray)
    arch.addOutPorts((0 until outputPort).map(i => s"out_$i").toArray)

    val tile = new TileLSUBlock("tile_0", colNum, rowNum, inputPort, outputPort,
      useMuxBypass = false, complex = true, isToroid = false, useCounter = true, dataWidth = dataWidth)
    arch.addBlock(tile)

    (0 until inputPort).foreach(i =>
      arch.addConnect(arch.term(s"input_$i") -> tile / s"input_$i"))
    (0 until outputPort).foreach(i =>
      arch.addConnect(tile / s"out_$i" -> arch.term(s"out_$i")))
    arch.init()

    //Get MRRG and mapping.
    //You can also use dumpMRRG(targetedII, filename) to save the MRRG,
    // and use loadTXT(mrrgFilename) to load the MRRG.
    val II = 1
    val MRRG = arch.getMRRG(II)
    val dfgFilename = "tutorial/Vadd_Reverse_.dot"
//    val dfgFilename = "dfg/accum/accum.dot"
    val dfg = DotReader.loadDot(dfgFilename, II)
    val mappingResultFilename = s"tutorial/ii$II"

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

    //Generate the top design.
    val connect = new Connect(arch.connectArray)
    val hardwareGenerator = new HardwareGenerator(arch, connect)
    val topDesign = () => new TopModule(hardwareGenerator.pillarsModuleInfo,
      hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)

    //Generate the RTL codes.
    chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), topDesign)

    //Simulate with the mapping result.
    JsonParser.writeJson(prepareRuntimeInfo(dfg, rowNum), "runtime.json")
    val runtimeInfo = JsonParser.readJson("runtime.json")

    //Simulation settings.
    val simulationHelper = new SimulationHelper(arch)
    val resultFilename = s"tutorial/ii$II" + "_r.txt"
    simulationHelper.init(resultFilename, runtimeInfo, II)

    val appTestHelper = new AppTestHelper(II)
    val moduleInfoFilename = s"tutorial/ii$II" + "_i.txt"
    appTestHelper.init(arch, simulationHelper, moduleInfoFilename, runtimeInfo)

    //    JsonParser.dumpRuntimeInfo(simulationHelper, appTestHelper, dfg)


    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), topDesign) {
      c => new VaddReverseTester(c, appTestHelper)
    }

//    if (II == 1) {
//      testSynthesize(dfg, simulationHelper, dataWidth, runtimeInfo)
//    }

  }
}

/** A tester for synthesized design of matrix multiplication.
 *
 * @param c                  the synthesized design
 * @param input              the input data in input port
 * @param outResult          the expected data in output port
 * @param expectedDataInSRAM the expected data in SRAM
 * @param outputCycle        the cycle we can obtain the last result
 */
class SynthesizedModuleTester(c: SynthesizedModule, input: Array[Int], outResult: Array[Int],
                              expectedDataInSRAM: Array[Int], outputCycle: Int) extends PeekPokeTester(c) {

  val dataSize = input.size
  val T = dataSize + outputCycle
  println("Checking the results from the output port(s) of the synthesized module.")
  for (t <- 0 until T) {
    if (t < dataSize) {
      poke(c.io.inputs(0), input(t))
    }
    if (t >= outputCycle) {
      expect(c.io.outs(0), outResult(t - outputCycle))
      println(outResult(t - outputCycle).toString + " " + peek(c.io.outs(0)).toString())
    }
    step(1)
  }

  println("Checking the results from the SRAM(s) of the synthesized module.")
  //Wait reverse finished
  step(10)
  for (t <- 0 until expectedDataInSRAM.size) {
    poke(c.io.storeUnitMemAddrs(0), t)
    step(1)
    expect(c.io.storeUnitMemDatas(0), expectedDataInSRAM(t))
    println(expectedDataInSRAM(t).toString + " " + peek(c.io.storeUnitMemDatas(0)))
  }
}

/** A tester for matrix multiplication.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class VaddReverseTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c, appTestHelper) {

  poke(c.io.en, 0)
  inputData()
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)
  checkPortOutsWithInput(testII)

  //Wait reverse finished
  step(10)
  checkLSUData()
}
