package tetriski.pillars.NoC

import java.io.PrintWriter

import chisel3.iotesters
import tetriski.pillars.archlib.TileLSUBlock
import tetriski.pillars.core._
import tetriski.pillars.hardware.TopModule
import tetriski.pillars.mapping.{DotReader, ILPMap}
import tetriski.pillars.testers.AppTestHelper

import scala.io.Source

object NoC4CGRA extends App {
  def getArch(supDiv: Boolean) = {
    val rowNum = 2
    val colNum = 2
    val inputPort = 1
    val outputPort = 1
    val dataWidth = NoCParam.payloadSize

    val arch = new ArchitctureHierarchy()
    arch.addInPorts((0 until inputPort).map(i => s"input_$i").toArray)
    arch.addOutPorts((0 until outputPort).map(i => s"out_$i").toArray)

    var opList = List(OpEnum.ADD, OpEnum.SUB)
    if (supDiv) {
      opList = List(OpEnum.ADD, OpEnum.MUL, OpEnum.SUB, OpEnum.SHLA, OpEnum.SHRA, OpEnum.DIV)
    }
    val tile = new TileLSUBlock("tile_0", colNum, rowNum, inputPort, outputPort,
      useMuxBypass = false, complex = false, isToroid = false, basicOpList = opList, dataWidth = dataWidth)
    arch.addBlock(tile)

    (0 until inputPort).foreach(i =>
      arch.addConnect(arch.term(s"input_$i") -> tile / s"input_$i"))
    (0 until outputPort).foreach(i =>
      arch.addConnect(tile / s"out_$i" -> arch.term(s"out_$i")))


    arch.init()
    arch
  }

  def getTileMap() = {
    var TileMap = Map[(Int, Int), () => TopModule]()
    for (y <- 0 until NoCParam.ySize) {
      for (x <- 0 until NoCParam.xSize) {
        var supDiv = false
        if (x == 1 && y == 1) {
          supDiv = true
        }
        val arch = getArch(supDiv)
        val connect = new Connect(arch.connectArray)
        val hardwareGenerator = new HardwareGenerator(arch, connect)
        val tile = () => new TopModule(hardwareGenerator.pillarsModuleInfo,
          hardwareGenerator.connectMap, hardwareGenerator.regionList, NoCParam.payloadSize)
        TileMap += (y, x) -> tile
      }
    }
    TileMap
  }

  def mapping(arch: ArchitctureHierarchy, dfgFilename: String, mappingResultFilename: String, II: Int): Unit = {
    val MRRG = arch.getMRRG(II)
    val DFG = DotReader.loadDot(dfgFilename, II)
    val scheduleControl = true
    ILPMap.mapping(DFG, MRRG, filename = mappingResultFilename, separatedPR = true,
      scheduleControl = scheduleControl, skewLimit = 4, latencyLimit = 15)
  }

  def genConfig(): Unit ={
    val II = 1
    var arch = getArch(false)

    var dfgFilename = "NoCTester/add.dot"
    var mappingResultFilename = s"NoCTester/add/ii$II"
    mapping(arch, dfgFilename, mappingResultFilename, II)

    var simulationHelper = new SimulationHelper(arch)
    var resultFilename = mappingResultFilename + "_r.txt"
    var infoFilename = mappingResultFilename + "_i.txt"
    simulationHelper.init(resultFilename)


    for (y <- 0 until NoCParam.ySize) {
      for (x <- 0 until NoCParam.xSize) {
        if (!(x == 1 && y == 1)) {
          val constVals = Array(x + y)
          simulationHelper.setConst(constVals, II)
          val appTestHelper = new AppTestHelper(II)
          appTestHelper.init(arch, simulationHelper, infoFilename, null)

          val bitStreamWriter = new PrintWriter("NoCTester/config/bitstream/" + y.toString
            + "_" + x.toString + ".hex")
          bitStreamWriter.println(appTestHelper.getBitStreams()(0).toString(16))
          bitStreamWriter.close()

          val scheduleWriter = new PrintWriter("NoCTester/config/schedule/" + y.toString
            + "_" + x.toString + ".hex")
          scheduleWriter.println(appTestHelper.getSchedulesBigInt()(0).toString(16))
          scheduleWriter.close()
        }
      }
    }

    arch = getArch(true)
    dfgFilename = "NoCTester/div.dot"
    mappingResultFilename = s"NoCTester/div/ii$II"
    mapping(arch, dfgFilename, mappingResultFilename, II)

    simulationHelper = new SimulationHelper(arch)
    resultFilename = mappingResultFilename + "_r.txt"
    infoFilename = mappingResultFilename + "_i.txt"
    simulationHelper.init(resultFilename)

    val constVals = Array(6, 2)
    simulationHelper.setConst(constVals, II)
    val appTestHelper = new AppTestHelper(II)
    appTestHelper.init(arch, simulationHelper, infoFilename, null)

    val bitStreamWriter = new PrintWriter("NoCTester/config/bitstream/" + 1.toString
      + "_" + 1.toString + ".hex")
    bitStreamWriter.println(appTestHelper.getBitStreams()(0).toString(16))
    bitStreamWriter.close()

    val scheduleWriter = new PrintWriter("NoCTester/config/schedule/" + 1.toString
      + "_" + 1.toString + ".hex")
    scheduleWriter.println(appTestHelper.getSchedulesBigInt()(0).toString(16))
    scheduleWriter.close()
  }

  genConfig()

  var bitStreamMap = Map[(Int, Int), BigInt]()
  var scheduleMap = Map[(Int, Int), BigInt]()

  for(y <- 0 until NoCParam.ySize){
    for(x <- 0 until NoCParam.xSize){
      val bitStreamSource = Source.fromFile("NoCTester/config/bitstream/" + y.toString
        + "_" + x.toString + ".hex")
      bitStreamMap += (y, x) -> BigInt(bitStreamSource.getLines().toArray.apply(0), 16)
      bitStreamSource.close()

      val scheduleSource = Source.fromFile("NoCTester/config/schedule/" + y.toString
        + "_" + x.toString + ".hex")
      scheduleMap += (y, x) -> BigInt(scheduleSource.getLines().toArray.apply(0), 16)
      scheduleSource.close()
    }
  }



  val tileMap = getTileMap()

  val largeDesign = () => new MultiTileCGRA(tileMap, bitStreamMap, scheduleMap)

  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator", "-tmvf", "--threads 12"), largeDesign) {
    c => new NocMeshCGRATester(c)
  }
}
