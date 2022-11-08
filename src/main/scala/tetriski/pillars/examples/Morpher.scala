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

import scala.io.Source

/** An end2end_test for Morpher generated mapping and test data
 */
object Morpher {
  def main(args: Array[String]): Unit = {
    val rowNum = 4
    val colNum = 4
    val inputPort = 2
    val outputPort = 2
    val dataWidth = 32
    val execCycles = 500
    var kernel_folder = s"tutorial/"
    var kernel_name = s"array_add"

    if(args.size == 2){
      kernel_folder = args(0)
      kernel_name = args(1)
      println("Kernel subfolder: " + kernel_folder)
      println("Kernel name: " + kernel_name)
    }

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

    //Generate the top design.
//    println("arch.ConnectArray: " + arch.connectArray)
    val connect = new Connect(arch.connectArray)
//    connect.dumpConnect()
    val hardwareGenerator = new HardwareGenerator(arch, connect)
    val topDesign = () => new TopModule(hardwareGenerator.pillarsModuleInfo,
      hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)

    //Generate the RTL codes.
    chisel3.Driver.execute(Array("-td", kernel_folder + "RTL/"), topDesign)

    //Simulate with the mapping result.

    val mappedIIFilename = kernel_folder + s"mapped_ii.txt"
    val mappedIIarr = Source.fromFile(mappedIIFilename).getLines().toArray
    val II = mappedIIarr(0).toInt


    val resultFilename = kernel_folder + kernel_name + s"_r.txt"
    val infoFilename = kernel_folder + kernel_name + s"_i.txt"
    simulationHelper.initNew(resultFilename)
    simulationHelper.setConst(simulationHelper.constArray.toArray, II)

    val constInfo = simulationHelper.getConstInfo()
    val schedules = simulationHelper.getSchedules()
    val bitStreams = arch.genConfig(infoFilename, II, constInfo)
    val appTestHelper = new AppTestHelper(bitStreams, schedules, II)


    val testDataFilename = kernel_folder + kernel_name + s"_trace_0.txt"
    val dataLayoutFilename = kernel_folder + kernel_name + s"_mem_alloc.txt"
    val dataMemDetailsFilename = kernel_folder + s"datamem_details.txt"
    simulationHelper.createDataMap(appTestHelper,testDataFilename, dataLayoutFilename, dataMemDetailsFilename)


    //Set cycles when we can put data through the input ports or get the result from the output ports,
    // which can be obtained from "*_r.txt"
    appTestHelper.setPortCycle(simulationHelper)



    iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), topDesign) {
      c => new MorpherTester(c, appTestHelper, execCycles)
    }

  }
}




/** A tester with morpher generated test data.
 *
 * @param c             the top design
 * @param appTestHelper the class which is helpful when creating testers
 */
class MorpherTester(c: TopModule, appTestHelper: AppTestHelper, execCycles: Int)
  extends ApplicationTester(c, appTestHelper) {

  println("-------------- Simulation begin --------------")
  poke(c.io.en, 0)
  inputData()
  val testII = appTestHelper.getTestII()
  inputConfig(testII)
  poke(c.io.en, 1)
  step(execCycles)
  checkLSUData2()

  println("-------------- Simulation end --------------")
}
