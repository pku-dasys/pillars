package tetriski.pillars.examples

import chisel3.iotesters
import tetriski.pillars.archlib.TileLSUBlock
import tetriski.pillars.core.{ArchitctureHierarchy, Connect, HardwareGenerator, SimulationHelper}
import tetriski.pillars.hardware.TopModule
import tetriski.pillars.mapping.{DotReader, ILPMap, Scheduler}
import tetriski.pillars.testers.{AppTestHelper, ApplicationTester}

import scala.collection.mutable.ArrayBuffer

/** An end2end tutorial of Pillars.
 * Example: matrix multiplication: C = A X B, where A is a M * 2 matrix, and B is a 2 * N matrix.
 * In the simulation, we can get C(i)(j) every II cycle.
 */
object Tutorial {
  def main(args: Array[String]): Unit = {
    val rowNum = 4
    val colNum = 6
    val inputPort = 6
    val outputPort = 6
    val dataWidth = 32

    //Initialize the top block.
    val arch = new ArchitctureHierarchy()
    arch.addInPorts((0 until inputPort).map(i => s"input_$i").toArray)
    arch.addOutPorts((0 until outputPort).map(i => s"out_$i").toArray)

    val tile = new TileLSUBlock("tile_0", colNum, rowNum, inputPort, outputPort,
      useMuxBypass = false, dataWidth = dataWidth)
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
    //        val dfgFilename = "DOT/cap/cap.dot"
    val dfgFilename = "tutorial/MM.dot"
    val DFG = DotReader.loadDot(dfgFilename, II)
    val mappingResultFilename = s"tutorial/ii$II"
    val scheduleControl = true
    ILPMap.mapping(DFG, MRRG, filename = mappingResultFilename, separatedPR = true,
      scheduleControl = scheduleControl, skewLimit = 1, latencyLimit = 16)
    //    if (!scheduleControl) {
    //      Scheduler.schedule(DFG, MRRG, filename = mappingResultFilename, II = II)
    //    }

    //Generate the top design.
    val connect = new Connect(arch.connectArray)
    val hardwareGenerator = new HardwareGenerator(arch, connect)
    val topDesign = () => new TopModule(hardwareGenerator.pillarsModuleInfo,
      hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)

    //Generate the RTL codes.
    chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), topDesign)

    //Simulate with the mapping result.

    //Matrix multiplication: C = A X B, where A is a M * 2 matrix, and B is a 2 * N matrix.
    val M = 4
    val N = 3
    val matrixA = Array.ofDim[Int](M, 2)
    val matrixB = Array.ofDim[Int](2, N)
    val matrixC = Array.ofDim[Int](M, N)
    val flattenedAB = new Array[Int](M * 2 + 2 * N)

    for (i <- 0 until M)
      for (j <- 0 until 2) {
        matrixA(i)(j) = i * 2 + j
        flattenedAB(i * 2 + j) = matrixA(i)(j)
      }
    for (i <- 0 until 2)
      for (j <- 0 until N) {
        matrixB(i)(j) = i * N + j
        flattenedAB(i * N + j + M * 2) = matrixB(i)(j)
      }
    for (i <- 0 until M)
      for (j <- 0 until N) {
        var sum = 0
        for (k <- 0 until 2) {
          sum += matrixA(i)(k) * matrixB(k)(j)
        }
        matrixC(i)(j) = sum
      }

    //The base address of A and B in SRAM of an LSU.
    val a_base = 0
    val b_base = M * 2

    //The value of const operators.
    val const0 = 2
    val const3 = 0
    val const5 = a_base
    val const11 = b_base
    val const13 = N * 0
    val const20 = 1
    val const22 = a_base
    val const28 = b_base
    val const31 = N * 1
    val constVals = Array(const0, const3, const5, const11, const13,
      const20, const22, const28, const31)


    //Simulation settings.
    val simulationHelper = new SimulationHelper(arch)
    val resultFilename = s"tutorial/ii$II" + "_r.txt"
    val infoFilename = s"tutorial/ii$II" + "_i.txt"
    simulationHelper.init(resultFilename)
    simulationHelper.setConst(constVals, II)

    val constInfo = simulationHelper.getConstInfo()
    val schedules = simulationHelper.getSchedules()
    val bitStreams = arch.genConfig(infoFilename, II, constInfo)
    val appTestHelper = new AppTestHelper(bitStreams, schedules, II)

    //In this simple tutorial, A and B are put into all LSUs.
    //But you can put them into partial LSUs according to the mapping results,
    // just like what in the ApplicationExamples.
    var inputDataMap = Map[List[Int], Array[Int]]()
    //Because the PEs in a row share an LSU, the number of LSUs is rowNum.
    (0 until rowNum).foreach(i =>
      inputDataMap = inputDataMap + (List(i, a_base) -> flattenedAB))
    appTestHelper.addInData(inputDataMap)

    //Set cycles when we can put data through the input ports or get the result from the output ports,
    // which can be obtained from "*_r.txt"
    appTestHelper.setPortCycle(simulationHelper)

    //Input i, j in the corresponding input ports,
    //and verify C(i, j) in the corresponding output port.

    //Please make sure that the name of those ports are "input_[0-9]+" or "out_[0-9]+"
    // when using simulationHelper.outPorts and simulationHelper.inputPorts.
    val portI = simulationHelper.inputPorts(0)
    val portJ = simulationHelper.inputPorts(1)
    val portResult = simulationHelper.outPorts(0)

    val inputI = new ArrayBuffer[Int]()
    val inputJ = new ArrayBuffer[Int]()
    val outResult = new ArrayBuffer[Int]()
    for (i <- 0 until M)
      for (j <- 0 until N) {
        inputI.append(i)
        inputJ.append(j)
        outResult.append(matrixC(i)(j))
      }
    appTestHelper.setInputPortData(Map(portI -> inputI.toArray, portJ -> inputJ.toArray))
    appTestHelper.setOutPortRefs(Map(portResult -> outResult.toArray))

    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), topDesign) {
      c => new MatrixMulTester(c, appTestHelper)
    }
  }
}

/** A tester for matrix multiplication.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class MatrixMulTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c, appTestHelper) {

  poke(c.io.en, 0)
  inputData()
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)
  checkPortOutsWithInput(testII)

}

