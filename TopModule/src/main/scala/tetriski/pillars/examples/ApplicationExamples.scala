package tetriski.pillars.examples

import chisel3.{Module, iotesters}
import tetriski.pillars.archlib.TileCompleteBlock
import tetriski.pillars.core.{ArchitctureHierarchy, Connect, ConstInfo, HardwareGeneration, ModuleTrait}
import tetriski.pillars.hardware.{TopModule, TopModuleWrapper}
import tetriski.pillars.testers.{AppTestHelper, SumTester, SumWrapperTester, VaddTester, VaddWrapperTester}

object ApplicationExamples {

  val arch = new ArchitctureHierarchy()
  //The order of ports should be same as TopModule
  arch.addOutPorts(Array("out_0", "out_1", "out_2", "out_3"))
  arch.addInPorts(Array("input_0", "input_1", "input_2", "input_3"))

  val tile = new TileCompleteBlock("tile_0", 4, 4, 4, 4)

  arch.addBlock(tile)

  arch.addConnect(List(List("input_0"),List("tile_0/", "input_0")))
  arch.addConnect(List(List("input_1"),List("tile_0/", "input_1")))
  arch.addConnect(List(List("input_2"),List("tile_0/", "input_2")))
  arch.addConnect(List(List("input_3"),List("tile_0/", "input_3")))

  arch.addConnect(List(List("tile_0/","out_0"),List("out_0")))
  arch.addConnect(List(List("tile_0/","out_1"),List("out_1")))
  arch.addConnect(List(List("tile_0/","out_2"),List("out_2")))
  arch.addConnect(List(List("tile_0/","out_3"),List("out_3")))

  arch.init()

  val targetII = 2

  arch.blockMap("tile_0").dumpMRRG(targetII)

  arch.dumpArchitcture()

  val connectArray = arch.connectArray

  val connect = new Connect(connectArray)

  connect.dumpConnect()

  val cp = new HardwareGeneration(arch, connect)

  val dataWidth = 32

  def exampleVadd(): Unit ={
    //********     II = 1     ********
    var outputCycle = 220
    var testII = 1
    var constInfo = new ConstInfo(testII)
    constInfo.addConst(arch("tile_0")("pe_3_0").getModule("const0").getModuleID(), 0, 1)
    constInfo.addConst(arch("tile_0")("pe_3_3").getModule("const0").getModuleID(), 0, 1)
    constInfo.addConst(arch("tile_0")("pe_1_0").getModule("const0").getModuleID(), 0, 1)
    constInfo.addConst(arch("tile_0")("pe_2_0").getModule("const0").getModuleID(), 0, 1)

    var fileName = "app_mapping_results/vadd_ii1.txt"

    arch.resetSchedules()
    arch("tile_0")("pe_2_2").getModule("alu0").setSkew(2, 0)
    arch("tile_0")("lsu_3").getModule("loadStoreUnit").setSkew(-3, 0)
    var schedules = arch.getSchedules()

    var inData0 = (0 to 100).map(i => scala.util.Random.nextInt()).toArray
    var inData1 = (0 to 100).map(i => scala.util.Random.nextInt()).toArray

    var numLSU0 = 0
    var numLSU1 = 1
    var outNumLSU = 3
    var base = 0
    var inDatas = Map(
      List(numLSU0, base) -> inData0,
      List(numLSU1, base) -> inData1)

//    var outPortNum = 2
    var refList = List[Int]()
    var ref = 0
    for(i <- 0 until inData0.size){
      ref = inData0(i) + inData1(i)
      refList = refList :+ ref
    }

    var refOutDatas = Map(List(outNumLSU, base) -> refList.toArray)

//    var outPortRefs = Map(outPortNum -> refList.toArray)

    testVadd(constInfo, schedules, fileName, inDatas, refOutDatas, testII, outputCycle)
    //********     II = 1     ********
  }

  def exampleSum(): Unit ={

    //********     II = 1     ********
    var outputCycle = 4
    var testII = 1
    var constInfo = new ConstInfo(testII)
    constInfo.addConst(arch("tile_0")("pe_0_1").getModule("const0").getModuleID(), 0, 1)
    constInfo.addConst(arch("tile_0")("pe_0_2").getModule("const0").getModuleID(), 0, 1)

    var fileName = "app_mapping_results/sum_ii1.txt"

    arch.resetSchedules()
    arch("tile_0")("pe_0_0").getModule("alu0").setWaitCycle(2, 0)
    var schedules = arch.getSchedules()

    var inData = (0 to 100).map(i => scala.util.Random.nextInt()).toArray

    var numLSU = 0
    var base = 0
    var inDatas = Map(List(numLSU, base) -> inData)

    var outPortNum = 2
    var refList = List[Int]()
    var ref = 0
    for(data <- inData){
      ref = ref + data
      refList = refList :+ ref
    }

    var outPortRefs = Map(outPortNum -> refList.toArray)

    testSum(constInfo, schedules, fileName, inDatas, testII, outputCycle, outPortRefs)
    //********     II = 1     ********

    //********     II = 2     ********
    outputCycle = 8
    testII = 2
    constInfo = new ConstInfo(testII)
    constInfo.addConst(arch("tile_0")("pe_1_2").getModule("const0").getModuleID(), 0, 1)
    constInfo.addConst(arch("tile_0")("pe_0_2").getModule("const0").getModuleID(), 1, 1)

    fileName = "app_mapping_results/sum_ii2.txt"

    arch.resetSchedules()
    arch("tile_0")("pe_1_0").getModule("alu0").setWaitCycle(5, 1)
//    println(arch("tile_0")("pe_1_0").getModule("alu0").getSchedule())
    schedules = arch.getSchedules()

    inData = (0 to 100).map(i => scala.util.Random.nextInt()).toArray

    numLSU = 1
    base = 0
    inDatas = Map(List(numLSU, base) -> inData)

    outPortNum = 0
    refList = List[Int]()
    ref = 0
    for(data <- inData){
      ref = ref + data
      refList = refList :+ ref
    }

    outPortRefs = Map(outPortNum -> refList.toArray)

    testSum(constInfo, schedules, fileName, inDatas, testII, outputCycle, outPortRefs)
    //********     II = 2     ********


    //********     II = 3     ********
    outputCycle = 9
    testII = 3
    constInfo = new ConstInfo(testII)
    constInfo.addConst(arch("tile_0")("pe_1_0").getModule("const0").getModuleID(), 1, 1)
    constInfo.addConst(arch("tile_0")("pe_0_2").getModule("const0").getModuleID(), 1, 1)

    fileName = "app_mapping_results/sum_ii3.txt"

    arch.resetSchedules()
    arch("tile_0")("pe_1_1").getModule("alu0").setWaitCycle(6, 0)
    schedules = arch.getSchedules()

    inData = (0 to 100).map(i => scala.util.Random.nextInt()).toArray

    numLSU = 3
    base = 0
    inDatas = Map(List(numLSU, base) -> inData)

    outPortNum = 3
    refList = List[Int]()
    ref = 0
    for(data <- inData){
      ref = ref + data
      refList = refList :+ ref
    }

    outPortRefs = Map(outPortNum -> refList.toArray)

    testSum(constInfo, schedules, fileName, inDatas, testII, outputCycle, outPortRefs)
//    testSumWrapper(constInfo, schedules, fileName, inDatas, testII, outputCycle, outPortRefs)
    //********     II = 3     ********
  }

  def testSumWrapper(constInfo: ConstInfo, schedules: List[Int], fileName: String, inDatas: Map[List[Int], Array[Int]],
              testII: Int, outputCycle: Int, outPortRefs: Map[Int, Array[Int]]): Unit ={

    val bitStreams = arch.genConfig(fileName, testII, constInfo)

    val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

    appTestHelper.addInData(inDatas)
    appTestHelper.setOutPortRefs(outPortRefs)
    appTestHelper.setOutputCycle(outputCycle)

    //Verilog generation
    chisel3.Driver.execute(Array("--no-check-comb-loops", "-td","WrapperTest"),
      () => new TopModuleWrapper(cp.pillarsModuleInfo, cp.connectMap,
        cp.configList, dataWidth, appTestHelper))

    iotesters.Driver.execute(Array("--no-check-comb-loops","-tgvo", "on", "-tbn" ,"verilator"),
      () => new TopModuleWrapper(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth, appTestHelper)) {
      c => new SumWrapperTester(c, appTestHelper)
    }

  }

  def testSum(constInfo: ConstInfo, schedules: List[Int], fileName: String, inDatas: Map[List[Int], Array[Int]],
              testII: Int, outputCycle: Int, outPortRefs: Map[Int, Array[Int]]): Unit ={

    val bitStreams = arch.genConfig(fileName, testII, constInfo)

    val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

    appTestHelper.addInData(inDatas)
    appTestHelper.setOutPortRefs(outPortRefs)
    appTestHelper.setOutputCycle(outputCycle)


    iotesters.Driver.execute(Array("--no-check-comb-loops","-tgvo", "on", "-tbn" ,"verilator"),
      () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
      c => new SumTester(c, appTestHelper)
    }
  }

  def testVadd(constInfo: ConstInfo, schedules: List[Int], fileName: String, inDatas: Map[List[Int], Array[Int]],
               refOutDatas: Map[List[Int], Array[Int]], testII: Int, outputCycle: Int): Unit ={
    val bitStreams = arch.genConfig(fileName, testII, constInfo)


    val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

    appTestHelper.addInData(inDatas)
    appTestHelper.addOutData(refOutDatas)
    appTestHelper.setOutputCycle(outputCycle)

    //Verilog generation
    chisel3.Driver.execute(Array("--no-check-comb-loops", "-td","WrapperTest"),
      () => new TopModuleWrapper(cp.pillarsModuleInfo, cp.connectMap,
        cp.configList, dataWidth, appTestHelper))

    iotesters.Driver.execute(Array("--no-check-comb-loops","-tgvo", "on", "-tbn" ,"verilator"),
      () => new TopModuleWrapper(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth, appTestHelper)) {
      c => new VaddWrapperTester(c, appTestHelper)
    }

//    iotesters.Driver.execute(Array("--no-check-comb-loops","-tgvo", "on", "-tbn" ,"verilator"),
//      () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
//      c => new VaddTester(c, appTestHelper)
//    }
  }
}
