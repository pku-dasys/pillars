package tetriski.pillars.examples

import chisel3.iotesters
import tetriski.pillars.archlib.TileLSUBlock
import tetriski.pillars.archlib.STDNOC_Block
import tetriski.pillars.core.{ArchitctureHierarchy, Connect, HardwareGenerator, OpEnum, SimulationHelper}
import tetriski.pillars.hardware.{SynthesizedModule, TopModule}
import tetriski.pillars.mapping.{DotReader, ILPMap, Scheduler}
import tetriski.pillars.testers.{AppTestHelper, ApplicationTester}

import scala.collection.mutable.ArrayBuffer
import chisel3.iotesters.PeekPokeTester
import tetriski.pillars.examples.ApplicationExamples.simulationHelper

/** An end2end_test tutorial of Pillars.
 * Example: matrix multiplication: C = A X B, where A is a M * 2 matrix, and B is a 2 * N matrix.
 * In the simulation, we can get C(i)(j) every II cycle.
 */
object Tutorial {
  def main(args: Array[String]): Unit = {
    val rowNum = 4
    val colNum = 4
    val inputPort = 2
    val outputPort = 2
    val dataWidth = 16

    //Initialize the top block.
    val arch = new ArchitctureHierarchy()
    arch.addInPorts((0 until inputPort).map(i => s"input_$i").toArray)
    arch.addOutPorts((0 until outputPort).map(i => s"out_$i").toArray)

    //    val tile = new TileLSUBlock("tile_0", colNum, rowNum, inputPort, outputPort,
    //      useMuxBypass = false, complex = true, dataWidth = dataWidth)
    val tile = new STDNOC_Block("tile_0", colNum, rowNum, inputPort, outputPort,
      useMuxBypass = false, complex = true, dataWidth = dataWidth)
    arch.addBlock(tile)

    (0 until inputPort).foreach(i =>
      arch.addConnect(arch.term(s"input_$i") -> tile / s"input_$i"))
    (0 until outputPort).foreach(i =>
      arch.addConnect(tile / s"out_$i" -> arch.term(s"out_$i")))
    arch.init()

    arch.dumpArchitecture()
    //Get MRRG and mapping.
    //You can also use dumpMRRG(targetedII, filename) to save the MRRG,
    // and use loadTXT(mrrgFilename) to load the MRRG.
    val II = 4
    //    val MRRG = arch.getMRRG(II)
    //            val dfgFilename = "dfg/cap/cap.dot"
    val dfgFilename = "tutorial/MM.dot"
    val DFG = DotReader.loadDot(dfgFilename, II)
    val mappingResultFilename = s"tutorial/ii$II"
    val scheduleControl = true
    //    ILPMap.mapping(DFG, MRRG, filename = mappingResultFilename, separatedPR = true,
    //      scheduleControl = scheduleControl, skewLimit = 4, latencyLimit = 15)

    //Generate the top design.
    println("arch.ConnectArray: " + arch.connectArray)
    val connect = new Connect(arch.connectArray)
    connect.dumpConnect()
    //    print("connect: "+ connect)
    val hardwareGenerator = new HardwareGenerator(arch, connect)
    val topDesign = () => new TopModule(hardwareGenerator.pillarsModuleInfo,
      hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)

    //Generate the RTL codes.
    chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), topDesign)

    //Simulate with the mapping result.


    //Matrix multiplication: C = A X B, where A is a M * 2 matrix, and B is a 2 * N matrix.
    //        val M = 4
    //        val N = 3
    //        val matrixA = Array.ofDim[Int](M, 2)
    //        val matrixB = Array.ofDim[Int](2, N)
    //        val matrixC = Array.ofDim[Int](M, N)
    //        val flattenedAB = new Array[Int](M * 2 + 2 * N)
    val M = 20
    val arrayA = new Array[Int](M)
    val arrayB = new Array[Int](M)
    val arrayC = new Array[Int](M)
    val flattenedAB = new Array[Int](M * 2 + 1)

    //        for (i <- 0 until M)
    //          for (j <- 0 until 2) {
    //            matrixA(i)(j) = i * 2 + j
    //            flattenedAB(i * 2 + j) = matrixA(i)(j)
    //          }
    //        for (i <- 0 until 2)
    //          for (j <- 0 until N) {
    //            matrixB(i)(j) = i * N + j
    //            flattenedAB(i * N + j + M * 2) = matrixB(i)(j)
    //          }
    //        for (i <- 0 until M)
    //          for (j <- 0 until N) {
    //            var sum = 0
    //            for (k <- 0 until 2) {
    //              sum += matrixA(i)(k) * matrixB(k)(j)
    //            }
    //            matrixC(i)(j) = sum
    //          }


    //expected
    var outDataRefArray = Array[Int](M)

    for (i <- 0 until M) {
      arrayA(i) = i * 2 + 5
      arrayB(i) = i * 3
      flattenedAB(i) = arrayA(i)
      flattenedAB(i + M) = arrayB(i)
      arrayC(i) = arrayA(i) + arrayB(i)
      outDataRefArray =  outDataRefArray :+  arrayC(i)
    }
    flattenedAB(2*M) = 1 //loop start
    println("FlattenedAB")
    for (i <- 0 until 2*M){
      println(" " + flattenedAB(i))
    }
    val outDataArrays = Array(outDataRefArray)
    //The base address of A and B in SRAM of an LSU.
    val a_base = 0
    val b_base = M

    //The value of const operators.
    //        val const0 = 2
    //        val const3 = 0
    //        val const5 = a_base
    //        val const11 = b_base
    //        val const13 = N * 0
    //        val const20 = 1
    //        val const22 = a_base
    //        val const28 = b_base
    //        val const31 = N * 1
    //        val constVals = Array(const0, const3, const5, const11, const13,
    //          const20, const22, const28, const31)
//    val const14 = 0
//    val const13 = 2 * M * 4 //byte address
//    val const11 = 2 * M * 4 //byte address
//    val const4 = (2*M + 1) *4
//    val const6 = 0
//    val const1 = M // trip count
//    val const9 = M * 4
    val const1 = 1
    val const2 = M //trip count
    val const4 = (2*M + 1) *4 // store addr
    val const6 = M * 4 //array b base
    val const7 = 2 * M * 4 //temporary fix for loop start store byte
    val const9 = 0
    val const11 = 2 * M * 4 // loop start
    val const12 = 0
    val const14 = 2 //LS
    val const15 = 2 //LS
    val const16 = 2 //LS
    val const19 = 2 * M * 4 // loops start store
    val const20 = 0
    val const20017 = 254 //exit
    val const20018 = 1

    // order of constvals should match with order of const in ii_r_xx.txt file
    val constVals = Array(const1, const2, const4, const6, const7,const9, const11,const12, const14, const15, const16, const19, const20, const20017,const20018)

    //Simulation settings.
    val simulationHelper = new SimulationHelper(arch)
    val resultFilename = s"tutorial/ii$II" + "_r_array_add.txt"
    val infoFilename = s"tutorial/ii$II" + "_i_array_add.txt"
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
    //        val portI = simulationHelper.inputPorts(0)
    //        val portJ = simulationHelper.inputPorts(1)
    //        val portResult = simulationHelper.outPorts(0)

    //        val inputI = new ArrayBuffer[Int]()
    //        val inputJ = new ArrayBuffer[Int]()
    //        val outResult = new ArrayBuffer[Int]()
    for (i <- 0 until M) {
      //          for (j <- 0 until N) {
      //            inputI.append(i)
      //            inputJ.append(j)
      //            outResult.append(arrayC(i))
    }

//    val inData0 = (0 until M).map(i => scala.util.Random.nextInt()).toArray
//    val inDataArrays = Array(inData0)
//
//    val dataWithAddr = simulationHelper.getDataWithAddr(dataSize = M,
//      inDataArrays = inDataArrays, outDataArrays = outDataArrays)
//    val outDatas = dataWithAddr(1).asInstanceOf[Map[List[Int], Array[Int]]]
//
//    appTestHelper.addOutData(outDatas)
    //        appTestHelper.setInputPortData(Map(portI -> inputI.toArray, portJ -> inputJ.toArray))
    //        appTestHelper.setOutPortRefs(Map(portResult -> outResult.toArray))

    //        val loaderNum = DFG.opNodes.map(node => if (node.opcode == OpEnum.LOAD) {
    //          1
    //        } else {
    //          0
    //        }).sum
    //        val synthesizedDesign = () =>
    //          new SynthesizedModule(DFG, constInfo, (0 until loaderNum).map(t => flattenedAB).toArray, dataWidth)

    //    chisel3.Driver.execute(Array("-td", "tutorial/RTL/"), synthesizedDesign)
    //    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), synthesizedDesign) {
    //      c =>
    //        new SynthesizedModuleTester(c, inputI.toArray, inputJ.toArray,
    //          outResult.toArray, appTestHelper.getOutputCycle())
    //    }

    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), topDesign) {
      c => new ArrayAddTester(c, appTestHelper)
    }


  }
}

///** A tester for synthesized design of matrix multiplication.
// *
// * @param c           the synthesized design
// * @param inputI      the input data in input port I
// * @param inputJ      the input data in input port J
// * @param outResult   the expected data in output port
// * @param outputCycle the cycle we can obtain the last result
// */
//class SynthesizedModuleTester(c: SynthesizedModule, inputI: Array[Int], inputJ: Array[Int],
//                              outResult: Array[Int], outputCycle: Int) extends PeekPokeTester(c) {
//
//  val dataSize = inputI.size
//  val T = dataSize + outputCycle
//  for (t <- 0 until T) {
//    if (t < dataSize) {
//      poke(c.io.inputs(0), inputI(t))
//      poke(c.io.inputs(1), inputJ(t))
//    }
//    if (t >= outputCycle) {
//      expect(c.io.outs(0), outResult(t - outputCycle))
//      println(outResult(t - outputCycle).toString + " " + peek(c.io.outs(0)).toString())
//    }
//    step(1)
//  }
//}

///** A tester for matrix multiplication.
// *
// * @param c             the top design
// * @param appTestHelper the class which is helpful when creating testers
// */
//class MatrixMulTester(c: TopModule, appTestHelper: AppTestHelper)
//  extends ApplicationTester(c, appTestHelper) {
//
//  poke(c.io.en, 0)
//  inputData()
//  val testII = appTestHelper.getTestII()
//  inputConfig(testII)
//  poke(c.io.en, 1)
////  checkPortOutsWithInput(testII)
//
//}
/** A tester for matrix multiplication.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class ArrayAddTester(c: TopModule, appTestHelper: AppTestHelper)
  extends ApplicationTester(c, appTestHelper) {
  val M = 20
  val arrayA = new Array[Int](M)
  val arrayB = new Array[Int](M)
  val arrayC = new Array[Int](M)

  for (i <- 0 until M) {
    arrayA(i) = i * 2 + 5
    arrayB(i) = i * 3
    arrayC(i) = arrayA(i) + arrayB(i)
  }

  val onevariableArray = new Array[Int](1)
  onevariableArray(0) = 0

  poke(c.io.en, 0)
  inputData()
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)
  step(2)
  //poke(c.io.en, 0)
  //enqData(1, onevariableArray ,  2 * 20)
  //poke(c.io.en, 1)
  step(90)
//  checkLSUData()/
  val const4 = (2*M + 1) //*4
  deqData(0,arrayC,const4)
//  checkPortOutsWithInput(testII)

}
