package tetriski.pillars.examples

import chisel3.iotesters
import tetriski.pillars.archlib.TileCompleteBlock
import tetriski.pillars.core._
import tetriski.pillars.hardware.{TopModule, TopModuleWrapper}
import tetriski.pillars.testers._

object ApplicationExamples {

  val arch = new ArchitctureHierarchy()
  //The order of ports should be same as TopModule
  arch.addOutPorts(Array("out_0", "out_1", "out_2", "out_3"))
  arch.addInPorts(Array("input_0", "input_1", "input_2", "input_3"))

  val dataWidth = 32

  val tile = new TileCompleteBlock("tile_0", 4, 4, 4, 4, dataWidth = dataWidth,
    useMuxBypass = true, isFullArch = false, isReduceArch = false)

  arch.addBlock(tile)

  arch.addConnect(List(List("input_0"), List("tile_0/", "input_0")))
  arch.addConnect(List(List("input_1"), List("tile_0/", "input_1")))
  arch.addConnect(List(List("input_2"), List("tile_0/", "input_2")))
  arch.addConnect(List(List("input_3"), List("tile_0/", "input_3")))

  arch.addConnect(List(List("tile_0/", "out_0"), List("out_0")))
  arch.addConnect(List(List("tile_0/", "out_1"), List("out_1")))
  arch.addConnect(List(List("tile_0/", "out_2"), List("out_2")))
  arch.addConnect(List(List("tile_0/", "out_3"), List("out_3")))

  arch.init()

  val connectArray = arch.connectArray

  val connect = new Connect(connectArray)

  connect.dumpConnect()

  val cp = new HardwareGeneration(arch, connect)

  val simulationHelper = new SimulationtionHelper(arch)

  def dumpArch(targetII: Int, filename: String = null): Unit = {
    arch.blockMap("tile_0").dumpMRRG(targetII, filename)
    arch.dumpArchitcture()
  }

  def dumpWrapperVerilog(): Unit ={
    //Verilog generation
    chisel3.Driver.execute(Array("--no-check-comb-loops", "-td", "WrapperTest"),
      () => new TopModuleWrapper(cp.pillarsModuleInfo, cp.connectMap,
        cp.configList, dataWidth))
  }

  def exampleVadd(): Unit = {
    val dataSize = 50
    val inData0 = (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    val inData1 = (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    //    var inData0 = (50 until dataSize + 50).toArray
    //    var inData1 = (100 until dataSize + 100).toArray
    var outDataRefArray = Array[Int]()
    var ref = 0
    for (i <- 0 until inData0.size) {
      ref = inData0(i) + inData1(i)
      outDataRefArray = outDataRefArray :+ ref
    }
    val inDataArrays = Array(inData0, inData1)
    val outDataArrays = Array(outDataRefArray)

    def testVadd(resultFilename: String, infoFilename: String, testII: Int, constVals: Array[Int],
                 addrArray: Array[Int], throughput: Int, outputCycle: Int, useWrapper: Boolean = false): Unit = {
      simulationHelper.init(resultFilename)
      simulationHelper.setConst(constVals, testII)
      val constInfo = simulationHelper.getConstInfo()
      val schedules = simulationHelper.getSchedules()
      val dataWithAddr = simulationHelper.getDataWithAddr(dataSize = dataSize,
        inDataArrays = inDataArrays, outDataArrays = outDataArrays)

      val inDatas = dataWithAddr(0).asInstanceOf[Map[List[Int], Array[Int]]]
      val refLSUOutDatas = dataWithAddr(1).asInstanceOf[Map[List[Int], Array[Int]]]

      val bitStreams = arch.genConfig(infoFilename, testII, constInfo)

      val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

      appTestHelper.addInData(inDatas)
      appTestHelper.addOutData(refLSUOutDatas)
      appTestHelper.setOutputCycle(outputCycle)

      if (useWrapper) {
        iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
          () => new TopModuleWrapper(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
          c => new VaddWrapperTester(c, appTestHelper)
        }
      } else {
        iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
          () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
          c => new VaddTester(c, appTestHelper)
        }
      }
    }

    //********     II = 1     ********
    var testII = 1
    var outputCycle = dataSize * (testII + 3)

    var infoFilename = "app_mapping_results/vadd/ii1_i.txt"
    var resultFilename = "app_mapping_results/vadd/ii1_r.txt"

    var a_base, b_base, c_base = 0
    var constVals = Array(a_base, b_base, c_base, 1)
    var addrVals = Array(a_base, b_base, c_base)
    var throughput = 1

//    testVadd(resultFilename, infoFilename, testII, constVals, addrVals, throughput, outputCycle)
    testVadd(resultFilename, infoFilename, testII, constVals, addrVals, throughput, outputCycle)
    //********     II = 1     ********

//    //********     II = 2     ********
//    testII = 2
//    outputCycle = dataSize * (testII + 3)
//
//    infoFilename = "app_mapping_results/vadd/ii2_i.txt"
//    resultFilename = "app_mapping_results/vadd/ii2_r.txt"
//
//    a_base = 0
//    b_base = 0
//    c_base = 0
//    constVals = Array(a_base, b_base, c_base, 1)
//    addrVals = Array(a_base, b_base, c_base)
//    throughput = 1
//
//    testVadd(resultFilename, infoFilename, testII, constVals, addrVals, throughput, outputCycle)
    //********     II = 2     ********

    //********     II = 3     ********
//    testII = 3
//    outputCycle = dataSize * (testII + 3)
//
//    infoFilename = "app_mapping_results/vadd/ii3_i.txt"
//    resultFilename = "app_mapping_results/vadd/ii3_r.txt"
//
//    a_base = 0
//    b_base = dataSize
//    c_base = 0
//    constVals = Array(a_base, b_base, c_base, 1)
//    addrVals = Array(a_base, b_base, c_base)
//    throughput = 1
//
//    testVadd(resultFilename, infoFilename, testII, constVals, addrVals, throughput, outputCycle)
    //********     II = 3     ********
  }

  def exampleSum(): Unit = {

    val dataSize = 50
    val inData = (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
//        var inData = (0 until dataSize).toArray

    var outPortRefArray = Array[Int]()
    var outPortRef = 0
    for (data <- inData) {
      outPortRef = outPortRef + data
      outPortRefArray = outPortRefArray :+ outPortRef
    }
    val inDataArrays = Array(inData)
    val outPortRefArrays = Array(outPortRefArray)

    def testSum(resultFilename: String, infoFilename: String, testII: Int, constVals: Array[Int],
                addrArray: Array[Int], throughput: Int, useWrapper: Boolean = false): Unit = {

      simulationHelper.init(resultFilename)
      val outputCycle = simulationHelper.getOutputCycle()
      simulationHelper.setConst(constVals, testII)
      val constInfo = simulationHelper.getConstInfo()
      val schedules = simulationHelper.getSchedules()
      val dataWithAddr = simulationHelper.getDataWithAddr(addrArray = addrArray,
        inDataArrays = inDataArrays, refDataArrays = outPortRefArrays)

      val inDatas = dataWithAddr(0).asInstanceOf[Map[List[Int], Array[Int]]]
      val outPortRefs = dataWithAddr(2).asInstanceOf[Map[Int, Array[Int]]]

      val bitStreams = arch.genConfig(infoFilename, testII, constInfo)

      val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

      appTestHelper.addInData(inDatas)
      appTestHelper.setOutPortRefs(outPortRefs)
      appTestHelper.setOutputCycle(outputCycle)


      if (useWrapper) {
        iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
          () => new TopModuleWrapper(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
          c => new SumWrapperTester(c, appTestHelper)
        }
      } else {
        iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
          () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
          c => new SumTester(c, appTestHelper)
        }
      }
    }

    //********     II = 1     ********
    var testII = 1

    var infoFilename = "app_mapping_results/sum/ii1_i.txt"
    var resultFilename = "app_mapping_results/sum/ii1_r.txt"

    var a_base = 0
    var constVals = Array(a_base, 1)
    var addrVals = Array(a_base)
    var throughput = 1

    //    testSum(resultFilename, infoFilename, testII, constVals, addrVals, throughput, useWrapper = true)
    testSum(resultFilename, infoFilename, testII, constVals, addrVals, throughput)
    //********     II = 1     ********

    //********     II = 2     ********
    testII = 2

    infoFilename = "app_mapping_results/sum/ii2_i.txt"
    resultFilename = "app_mapping_results/sum/ii2_r.txt"

    a_base = 0
    constVals = Array(a_base, 1)
    addrVals = Array(a_base)
    throughput = 1

    testSum(resultFilename, infoFilename, testII, constVals, addrVals, throughput)
    //********     II = 2     ********

    //********     II = 3     ********
    testII = 3

    infoFilename = "app_mapping_results/sum/ii3_i.txt"
    resultFilename = "app_mapping_results/sum/ii3_r.txt"

    a_base = 0
    constVals = Array(a_base, 1)
    addrVals = Array(a_base)
    throughput = 1

    testSum(resultFilename, infoFilename, testII, constVals, addrVals, throughput)
    //********     II = 3     ********
  }

  def exampleAccum(): Unit = {

    val dataSize = 50
    val inDataA = (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    val inDataB = (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    val inDataC = (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray

//        var inDataA = (2 until dataSize + 2).toArray
//        var inDataB = (2 until dataSize + 2).toArray
//        var inDataC = (2 until dataSize + 2).toArray

    var refArray = Array[Int]()
    var outPortRefArray = Array[Int]()
    var ref = 0
    var outPortRef = 0

    for (i <- 0 until inDataA.size - 1) {
      ref = (inDataA(i) + inDataB(i + 1)) * inDataC(i)
      refArray = refArray :+ ref

      outPortRef = outPortRef + ref
      outPortRefArray = outPortRefArray :+ outPortRef
    }
    val inDataArrays = Array(inDataA, inDataB, inDataC)
    val outDataRefArrays = Array(refArray)
    val outPortRefArrays = Array(outPortRefArray)

    def testAccum(resultFilename: String, infoFilename: String, testII: Int,
                  constVals: Array[Int], addrArray: Array[Int], throughput: Int): Unit = {

      simulationHelper.init(resultFilename)
      val outputCycle = simulationHelper.getOutputCycle()
      simulationHelper.setConst(constVals, testII)
      val constInfo = simulationHelper.getConstInfo()
      val schedules = simulationHelper.getSchedules()
      val dataWithAddr = simulationHelper.getDataWithAddr(addrArray = addrArray,
        inDataArrays = inDataArrays, outDataArrays = outDataRefArrays, refDataArrays = outPortRefArrays)

      val inDatas = dataWithAddr(0).asInstanceOf[Map[List[Int], Array[Int]]]
      val outDatas = dataWithAddr(1).asInstanceOf[Map[List[Int], Array[Int]]]
      val outPortRefs = dataWithAddr(2).asInstanceOf[Map[Int, Array[Int]]]

      val bitStreams = arch.genConfig(infoFilename, testII, constInfo)

      val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

      appTestHelper.addInData(inDatas)
      appTestHelper.addOutData(outDatas)
      appTestHelper.setOutPortRefs(outPortRefs)
      appTestHelper.setOutputCycle(outputCycle)
      appTestHelper.setThroughput(throughput)


      iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
        c => new AccumTester(c, appTestHelper)
      }
    }

    //********     II = 1     ********
    var testII = 1

    var infoFilename = "app_mapping_results/accum/ii1_i.txt"
    var resultFilename = "app_mapping_results/accum/ii1_r.txt"
    var a_base, b_base, c_base = 0
    var constVals = Array(1, a_base, b_base, 1, c_base)
    var addrVals = Array(a_base, b_base, c_base, c_base)
    var throughput = 1

    testAccum(resultFilename, infoFilename, testII, constVals, addrVals, throughput)
    //********     II = 1     ********

    //********     II = 2     ********
    testII = 2

    infoFilename = "app_mapping_results/accum/ii2_i.txt"
    resultFilename = "app_mapping_results/accum/ii2_r.txt"
    a_base = 0
    b_base = dataSize
    c_base = 0
    constVals = Array(1, a_base, b_base, 1, c_base)
    addrVals = Array(a_base, b_base, c_base, c_base)
    throughput = 1

    testAccum(resultFilename, infoFilename, testII, constVals, addrVals, throughput)
    //********     II = 2     ********

    //********     II = 3     ********
    testII = 3

    infoFilename = "app_mapping_results/accum/ii3_i.txt"
    resultFilename = "app_mapping_results/accum/ii3_r.txt"
    a_base = 0
    b_base = 0
    c_base = dataSize
    constVals = Array(1, a_base, b_base, 1, c_base)
    addrVals = Array(a_base, b_base, c_base, c_base)
    throughput = 1

    testAccum(resultFilename, infoFilename, testII, constVals, addrVals, throughput)
    //********     II = 3     ********
  }

  def exampleCap(): Unit = {

    val dataSize = 50
//    val inDataA = (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
//    val inDataC = Array(3)
//    val inDataM = (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray

    val inDataA = (0 until dataSize).toArray
    val inDataC = Array(3)
    val inDataM = (0 until dataSize).toArray


    var refArray = Array[Int]()
    var ref = 0

    for (i <- 0 until inDataA.size) {
      ref = (((inDataA(i) * 3 * inDataC(0)) >> 2 ) * inDataC(0)) * (((inDataM(i) * 3 * inDataA(i)) >> 2 ) * inDataA(i))
      refArray = refArray :+ ref
    }
    val inDataArrays = Array(inDataA, inDataC, inDataM)
    val outDataRefArrays = Array(refArray)

    def testCap(resultFilename: String, infoFilename: String, testII: Int,
                  constVals: Array[Int], addrArray: Array[Int], throughput: Int, outputCycle: Int): Unit = {

      simulationHelper.init(resultFilename)
      simulationHelper.setConst(constVals, testII)
      val constInfo = simulationHelper.getConstInfo()
      val schedules = simulationHelper.getSchedules()
      val dataWithAddr = simulationHelper.getDataWithAddr(addrArray = addrArray,
        inDataArrays = inDataArrays, outDataArrays = outDataRefArrays)

      val inDatas = dataWithAddr(0).asInstanceOf[Map[List[Int], Array[Int]]]
      val outDatas = dataWithAddr(1).asInstanceOf[Map[List[Int], Array[Int]]]

      val bitStreams = arch.genConfig(infoFilename, testII, constInfo)

      val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

      appTestHelper.addInData(inDatas)
      appTestHelper.addOutData(outDatas)
      appTestHelper.setOutputCycle(outputCycle)
      appTestHelper.setThroughput(throughput)


      iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
        c => new CapTester(c, appTestHelper)
      }
    }

    //********     II = 1     ********
    var testII = 1
    var outputCycle = dataSize * (testII + 2)

    var infoFilename = "app_mapping_results/cap/ii1_i.txt"
    var resultFilename = "app_mapping_results/cap/ii1_r.txt"
    var a_base, m_base, c1_addr, b_base = 0
    var constVals = Array(a_base, 3, c1_addr, 2, m_base, 2, b_base, 1)
    var addrVals = Array(a_base, c1_addr, m_base, b_base)
    var throughput = 1

    testCap(resultFilename, infoFilename, testII, constVals, addrVals, throughput, outputCycle)
    //********     II = 1     ********

    //********     II = 2     ********
    testII = 2
    outputCycle = dataSize * (testII + 2)

    infoFilename = "app_mapping_results/cap/ii2_i.txt"
    resultFilename = "app_mapping_results/cap/ii2_r.txt"

    a_base = 0
    c1_addr = 0
    m_base = 0
    b_base = 0

    constVals = Array(a_base, 3, c1_addr, 2, m_base, 2, b_base, 1)
    addrVals = Array(a_base, c1_addr, m_base, b_base)
    throughput = 1

    testCap(resultFilename, infoFilename, testII, constVals, addrVals, throughput, outputCycle)
    //********     II = 2     ********

    //********     II = 3     ********
    testII = 3
    outputCycle = dataSize * (testII + 2)

    infoFilename = "app_mapping_results/cap/ii3_i.txt"
    resultFilename = "app_mapping_results/cap/ii3_r.txt"

    a_base = 0
    c1_addr = 0
    m_base = dataSize
    b_base = 0

    constVals = Array(a_base, 3, c1_addr, 2, m_base, 2, b_base, 1)
    addrVals = Array(a_base, c1_addr, m_base, b_base)
    throughput = 1

    testCap(resultFilename, infoFilename, testII, constVals, addrVals, throughput, outputCycle)
    //********     II = 3     ********
  }
}
