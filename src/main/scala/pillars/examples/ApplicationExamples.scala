package pillars.examples

import pillars.archlib.TileCompleteBlock
import pillars.core._
import pillars.hardware.{TopModule, TopModuleWrapper, TopModuleWrapperSolid}
import pillars.testers._

import chiseltest._
import firrtl.transforms.DontCheckCombLoopsAnnotation
import org.scalatest.flatspec.AnyFlatSpec

/** Some examples showing how to test applications with a 4*4 TileCompleteBlock.
  * We suggest users employ functions and classes in this object.
  * Some pre-generated mapping results for accum, cap, sum and vadd are provided.
  * One can modify the parameters when creating TileCompleteBlock to create different hardware and test.
  */
object ApplicationExamples {

  val arch = new ArchitectureHierarchy()
  //The order of ports should be same as TopModule
  arch.addOutPorts(Array("out_0", "out_1", "out_2", "out_3"))
  arch.addInPorts(Array("input_0", "input_1", "input_2", "input_3"))

  val dataWidth = 32

  val tile = new TileCompleteBlock(
    "tile_0",
    4,
    4,
    4,
    4,
    dataWidth = dataWidth,
    useMuxBypass = true,
    isFullArch = false,
    isReduceArch = false
  )

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

  val hardwareGenerator = new HardwareGenerator(arch, connect)

  val simulationHelper = new SimulationHelper(arch)

  /** Dump MRRG and a description file in JSON of the targeted architecture.
    *
    * @param targetedII the targeted II
    * @param filename the file name to save MRRG
    */
  def dumpArch(targetedII: Int, filename: String = null): Unit = {
    arch.blockMap("tile_0").dumpMRRG(targetedII, filename)
    arch.dumpArchitecture()
  }

  /** Dump generated Verilog of the targeted architecture.
    */
  def dumpWrapperVerilog(): Unit = {
    //Verilog generation
    chisel3.emitVerilog(
      new TopModuleWrapper(
        hardwareGenerator.pillarsModuleInfo,
        hardwareGenerator.connectMap,
        hardwareGenerator.regionList,
        dataWidth
      ),
      Array("--no-check-comb-loops", "-td", "WrapperTest")
    )
  }

  /** An example for testing vadd when II = 1-3.
    */
  def exampleVadd(): Unit = {
    val dataSize = 50
    //prepare the input and expected data
    val inData0 =
      (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    val inData1 =
      (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
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

    /** Uses simulationHelper and appTestHelper to generate Chisel PeekPokeTester for testing vadd.
      *
      * @param resultFilename the file name of result TXT
      * @param infoFilename   the file name of information TXT
      * @param testII         the targeted II
      * @param constVals      an array of const values
      * @param addrArray      an array of the address of input/output data
      * @param throughput     a parameter indicating the throughput of mapping result
      * @param outputCycle    the cycle we can obtain the result
      * @param useWrapper     a parameter indicating whether we use TopModuleWrapper for testing
      */
    def testVadd(
        resultFilename: String,
        infoFilename: String,
        testII: Int,
        constVals: Array[Int],
        addrArray: Array[Int],
        throughput: Int,
        outputCycle: Int,
        useWrapper: Boolean = false
    ): Unit = {
      simulationHelper.init(resultFilename)
      simulationHelper.setConst(constVals, testII)
      val constInfo = simulationHelper.getConstInfo()
      val dataWithAddr = simulationHelper.getDataWithAddr(
        dataSize = dataSize,
        inDataArrays = inDataArrays,
        outDataArrays = outDataArrays
      )

      val inDatas = dataWithAddr(0).asInstanceOf[Map[List[Int], Array[Int]]]
      val refLSUOutDatas = dataWithAddr(1)
        .asInstanceOf[Map[List[Int], Array[Int]]]

      val bitStreams = arch.genConfig(infoFilename, testII, constInfo)
      //NOTE: getSchedules should be called after genConfig if ALUs are allowed to perform bypass.
      val schedules = arch.getSchedules()

      val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

      appTestHelper.addInData(inDatas)
      appTestHelper.addOutData(refLSUOutDatas)
      appTestHelper.setOutputCycle(outputCycle)
      appTestHelper.setThroughput(throughput)

      if (useWrapper) {
        org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
          it should "work" in {
            test(
                new TopModuleWrapper(
                  hardwareGenerator.pillarsModuleInfo,
                  hardwareGenerator.connectMap,
                  hardwareGenerator.regionList,
                  dataWidth
                )
            )
            .withAnnotations(Seq(DontCheckCombLoopsAnnotation, VerilatorBackendAnnotation))
            .runPeekPoke(new VaddWrapperTester(_, appTestHelper))
          }
        })
      } else {
        org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
          it should "work" in {
            test(
                new TopModule(
                  hardwareGenerator.pillarsModuleInfo,
                  hardwareGenerator.connectMap,
                  hardwareGenerator.regionList,
                  dataWidth
                )
            )
            .withAnnotations(Seq(DontCheckCombLoopsAnnotation, VerilatorBackendAnnotation))
            .runPeekPoke(new VaddTester(_, appTestHelper))
          }
        })
      }
    }

    //********     II = 1     ********
    var testII = 1
    var outputCycle = dataSize * (testII + 3)

    var infoFilename = "app-mapping-results/vadd/ii1_i.txt"
    var resultFilename = "app-mapping-results/vadd/ii1_r.txt"

    var a_base, b_base, c_base = 0
    var constVals = Array(a_base, b_base, c_base, 1)
    var addrVals = Array(a_base, b_base, c_base)
    var throughput = 1

    testVadd(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput,
      outputCycle
    )
    //********     II = 1     ********

    //********     II = 2     ********
    testII = 2
    outputCycle = dataSize * (testII + 3)

    infoFilename = "app-mapping-results/vadd/ii2_i.txt"
    resultFilename = "app-mapping-results/vadd/ii2_r.txt"

    a_base = 0
    b_base = 0
    //Since a & c are both use the same LSU, the storage space of them cannot overlap.
    c_base = dataSize
    constVals = Array(a_base, b_base, c_base, 1)
    addrVals = Array(a_base, b_base, c_base)
    throughput = 1

    testVadd(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput,
      outputCycle
    )
    //********     II = 2     ********

    //********     II = 3     ********
    testII = 3
    outputCycle = dataSize * (testII + 3)

    infoFilename = "app-mapping-results/vadd/ii3_i.txt"
    resultFilename = "app-mapping-results/vadd/ii3_r.txt"

    a_base = 0
    //Since a & b are both use the same LSU, the storage space of them cannot overlap.
    b_base = dataSize
    c_base = 0
    constVals = Array(a_base, b_base, c_base, 1)
    addrVals = Array(a_base, b_base, c_base)
    throughput = 1

    testVadd(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput,
      outputCycle
    )
    //********     II = 3     ********
  }

  /** An example for testing sum when II = 1-3.
    */
  def exampleSum(): Unit = {

    val dataSize = 50
    //prepare the input and expected data
    val inData =
      (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    //        var inData = (0 until dataSize).toArray

    var outPortRefArray = Array[Int]()
    var outPortRef = 0
    for (data <- inData) {
      outPortRef = outPortRef + data
      outPortRefArray = outPortRefArray :+ outPortRef
    }
    val inDataArrays = Array(inData)
    val outPortRefArrays = Array(outPortRefArray)

    /** Uses simulationHelper and appTestHelper to generate Chisel PeekPokeTester for testing sum.
      * In this example, the outputCycle is automatically inferred by simulationHelper.
      *
      * @param resultFilename the file name of result TXT
      * @param infoFilename   the file name of information TXT
      * @param testII         the targeted II
      * @param constVals      an array of const values
      * @param addrArray      an array of the address of input/output data
      * @param throughput     a parameter indicating the throughput of mapping result
      * @param useWrapper     a parameter indicating whether we use TopModuleWrapper for testing
      */
    def testSum(
        resultFilename: String,
        infoFilename: String,
        testII: Int,
        constVals: Array[Int],
        addrArray: Array[Int],
        throughput: Int,
        useWrapper: Boolean = false
    ): Unit = {

      simulationHelper.init(resultFilename)
      val outputCycle = simulationHelper.getOutputCycle()
      simulationHelper.setConst(constVals, testII)
      val constInfo = simulationHelper.getConstInfo()
      val dataWithAddr = simulationHelper.getDataWithAddr(
        addrArray = addrArray,
        inDataArrays = inDataArrays,
        refDataArrays = outPortRefArrays
      )

      val inDatas = dataWithAddr(0).asInstanceOf[Map[List[Int], Array[Int]]]
      val outPortRefs = dataWithAddr(2).asInstanceOf[Map[Int, Array[Int]]]

      val bitStreams = arch.genConfig(infoFilename, testII, constInfo)
      //NOTE: getSchedules should be called after genConfig if ALUs are allowed to perform bypass.
      val schedules = arch.getSchedules()

      val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

      appTestHelper.addInData(inDatas)
      appTestHelper.setOutPortRefs(outPortRefs)
//      appTestHelper.setOutputCycle(outputCycle)
      appTestHelper.setThroughput(throughput)

      appTestHelper.setPortCycle(simulationHelper)

      if (useWrapper) {
        org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
          it should "work" in {
            test(
                new TopModuleWrapper(
                  hardwareGenerator.pillarsModuleInfo,
                  hardwareGenerator.connectMap,
                  hardwareGenerator.regionList,
                  dataWidth
                )
            )
            .withAnnotations(Seq(DontCheckCombLoopsAnnotation, VerilatorBackendAnnotation))
            .runPeekPoke(new SumWrapperTester(_, appTestHelper))
          }
        })
      } else {
        org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
          it should "work" in {
            test(
                new TopModule(
                  hardwareGenerator.pillarsModuleInfo,
                  hardwareGenerator.connectMap,
                  hardwareGenerator.regionList,
                  dataWidth
                )
            )
            .withAnnotations(Seq(DontCheckCombLoopsAnnotation, VerilatorBackendAnnotation))
            .runPeekPoke(new SumTester(_, appTestHelper))
          }
        })
      }
    }

    //********     II = 1     ********
    var testII = 1

    var infoFilename = "app-mapping-results/sum/ii1_i.txt"
    var resultFilename = "app-mapping-results/sum/ii1_r.txt"

    var a_base = 0
    var constVals = Array(a_base, 1)
    var addrVals = Array(a_base)
    var throughput = 1

//        testSum(resultFilename, infoFilename, testII, constVals, addrVals, throughput, useWrapper = true)
    testSum(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput
    )
    //********     II = 1     ********

    //********     II = 2     ********
    testII = 2

    infoFilename = "app-mapping-results/sum/ii2_i.txt"
    resultFilename = "app-mapping-results/sum/ii2_r.txt"

    a_base = 0
    constVals = Array(a_base, 1)
    addrVals = Array(a_base)
    throughput = 1

    testSum(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput
    )
    //********     II = 2     ********

    //********     II = 3     ********
    testII = 3

    infoFilename = "app-mapping-results/sum/ii3_i.txt"
    resultFilename = "app-mapping-results/sum/ii3_r.txt"

    a_base = 0
    constVals = Array(a_base, 1)
    addrVals = Array(a_base)
    throughput = 1

    testSum(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput
    )
    //********     II = 3     ********
  }

  /** An example for testing accumulate when II = 1-3.
    */
  def exampleAccum(): Unit = {

    val dataSize = 50
    //prepare the input and expected data
    val inDataA =
      (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    val inDataB =
      (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    val inDataC =
      (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray

//    var inDataA = (2 until dataSize + 2).toArray
//    var inDataB = (10 until dataSize + 10).toArray
//    var inDataC = (20 until dataSize + 20).toArray

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

    /** Uses simulationHelper and appTestHelper to generate Chisel PeekPokeTester for testing accumulate.
      * In this example, the outputCycle is automatically inferred by simulationHelper.
      *
      * @param resultFilename the file name of result TXT
      * @param infoFilename   the file name of information TXT
      * @param testII         the targeted II
      * @param constVals      an array of const values
      * @param addrArray      an array of the address of input/output data
      * @param throughput     a parameter indicating the throughput of mapping result
      */
    def testAccum(
        resultFilename: String,
        infoFilename: String,
        testII: Int,
        constVals: Array[Int],
        addrArray: Array[Int],
        throughput: Int
    ): Unit = {

      simulationHelper.init(resultFilename)
      val outputCycle = simulationHelper.getOutputCycle()
      simulationHelper.setConst(constVals, testII)
      val constInfo = simulationHelper.getConstInfo()
      val dataWithAddr = simulationHelper.getDataWithAddr(
        addrArray = addrArray,
        inDataArrays = inDataArrays,
        outDataArrays = outDataRefArrays,
        refDataArrays = outPortRefArrays
      )

      val inDatas = dataWithAddr(0).asInstanceOf[Map[List[Int], Array[Int]]]
      val outDatas = dataWithAddr(1).asInstanceOf[Map[List[Int], Array[Int]]]
      val outPortRefs = dataWithAddr(2).asInstanceOf[Map[Int, Array[Int]]]

      val bitStreams = arch.genConfig(infoFilename, testII, constInfo)
      //NOTE: getSchedules should be called after genConfig if ALUs are allowed to perform bypass.
      val schedules = arch.getSchedules()

      val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

      appTestHelper.addInData(inDatas)
      appTestHelper.addOutData(outDatas)
      appTestHelper.setOutPortRefs(outPortRefs)
      appTestHelper.setOutputCycle(outputCycle)
      appTestHelper.setThroughput(throughput)

      org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
        it should "work" in {
          test(
              new TopModule(
                hardwareGenerator.pillarsModuleInfo,
                hardwareGenerator.connectMap,
                hardwareGenerator.regionList,
                dataWidth
              )
          )
          .withAnnotations(Seq(DontCheckCombLoopsAnnotation, VerilatorBackendAnnotation))
          .runPeekPoke(new AccumTester(_, appTestHelper))
        }
      })
    }

    //********     II = 1     ********
    var testII = 1

    var infoFilename = "app-mapping-results/accum/ii1_i.txt"
    var resultFilename = "app-mapping-results/accum/ii1_r.txt"
    var a_base, b_base, c_base = 0
    var constVals = Array(1, a_base, b_base, 1, c_base)
    var addrVals = Array(a_base, b_base, c_base, c_base)
    var throughput = 1

    testAccum(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput
    )
    //********     II = 1     ********

    //********     II = 2     ********
    testII = 2

    infoFilename = "app-mapping-results/accum/ii2_i.txt"
    resultFilename = "app-mapping-results/accum/ii2_r.txt"
    a_base = 0
    b_base = 0
    //Since a & c are both use the same LSU, the storage space of them cannot overlap.
    c_base = dataSize
    constVals = Array(1, a_base, b_base, 1, c_base)
    addrVals = Array(a_base, b_base, c_base, c_base)
    throughput = 1

    testAccum(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput
    )
    //********     II = 2     ********

    //********     II = 3     ********
    testII = 3

    infoFilename = "app-mapping-results/accum/ii3_i.txt"
    resultFilename = "app-mapping-results/accum/ii3_r.txt"
    a_base = 0
    b_base = 0
    //Since a & c are both use the same LSU, the storage space of them cannot overlap.
    c_base = dataSize
    constVals = Array(1, a_base, b_base, 1, c_base)
    addrVals = Array(a_base, b_base, c_base, c_base)
    throughput = 1

    testAccum(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput
    )
    //********     II = 3     ********
  }

  /** An example for testing cap when II = 1-3.
    */
  def exampleCap(): Unit = {

    val dataSize = 50
    //prepare the input and expected data
    val inDataA =
      (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray
    val inDataC = Array(3)
    val inDataM =
      (0 until dataSize).map(i => scala.util.Random.nextInt()).toArray

    //    val inDataA = (0 until dataSize).toArray
    //    val inDataC = Array(3)
    //    val inDataM = (0 until dataSize).toArray

    var refArray = Array[Int]()
    var ref = 0

    for (i <- 0 until inDataA.size) {
      ref =
        (((inDataA(i) * 3 * inDataC(0)) >> 2) * inDataC(0)) * (((inDataM(i) * 3 * inDataA(
          i
        )) >> 2) * inDataA(i))
      refArray = refArray :+ ref
    }
    val inDataArrays = Array(inDataA, inDataC, inDataM)
    val outDataRefArrays = Array(refArray)

    /** Uses simulationHelper and appTestHelper to generate Chisel PeekPokeTester for testing accumulate.
      *
      * @param resultFilename the file name of result TXT
      * @param infoFilename   the file name of information TXT
      * @param testII         the targeted II
      * @param constVals      an array of const values
      * @param addrArray      an array of the address of input/output data
      * @param throughput     a parameter indicating the throughput of mapping result
      * @param outputCycle    the cycle we can obtain the result
      */
    def testCap(
        resultFilename: String,
        infoFilename: String,
        testII: Int,
        constVals: Array[Int],
        addrArray: Array[Int],
        throughput: Int,
        outputCycle: Int
    ): Unit = {

      simulationHelper.init(resultFilename)
      simulationHelper.setConst(constVals, testII)
      val constInfo = simulationHelper.getConstInfo()
      val dataWithAddr = simulationHelper.getDataWithAddr(
        addrArray = addrArray,
        inDataArrays = inDataArrays,
        outDataArrays = outDataRefArrays
      )

      val inDatas = dataWithAddr(0).asInstanceOf[Map[List[Int], Array[Int]]]
      val outDatas = dataWithAddr(1).asInstanceOf[Map[List[Int], Array[Int]]]

      val bitStreams = arch.genConfig(infoFilename, testII, constInfo)
      //NOTE: getSchedules should be called after genConfig if ALUs are allowed to perform bypass.
      val schedules = arch.getSchedules()

      val appTestHelper = new AppTestHelper(bitStreams, schedules, testII)

      appTestHelper.addInData(inDatas)
      appTestHelper.addOutData(outDatas)
      appTestHelper.setOutputCycle(outputCycle)
      appTestHelper.setThroughput(throughput)

      org.scalatest.run(new AnyFlatSpec with ChiselScalatestTester {
        it should "work" in {
          test(
              new TopModule(
                hardwareGenerator.pillarsModuleInfo,
                hardwareGenerator.connectMap,
                hardwareGenerator.regionList,
                dataWidth
              )
          )
          .withAnnotations(Seq(DontCheckCombLoopsAnnotation, VerilatorBackendAnnotation))
          .runPeekPoke(new CapTester(_, appTestHelper))
        }
      })
    }

    //********     II = 1     ********
    var testII = 1
    var outputCycle = dataSize * (testII + 2)

    var infoFilename = "app-mapping-results/cap/ii1_i.txt"
    var resultFilename = "app-mapping-results/cap/ii1_r.txt"
    var a_base, m_base, c1_addr, b_base = 0
    var constVals = Array(a_base, 3, c1_addr, 2, m_base, 2, b_base, 1)
    var addrVals = Array(a_base, c1_addr, m_base, b_base)
    var throughput = 1

    testCap(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput,
      outputCycle
    )
    //********     II = 1     ********

    //********     II = 2     ********
    testII = 2
    outputCycle = dataSize * (testII + 2)

    infoFilename = "app-mapping-results/cap/ii2_i.txt"
    resultFilename = "app-mapping-results/cap/ii2_r.txt"

    a_base = 0
    c1_addr = 0
    m_base = 0
    b_base = 0

    constVals = Array(a_base, 3, c1_addr, 2, m_base, 2, b_base, 1)
    addrVals = Array(a_base, c1_addr, m_base, b_base)
    throughput = 1

    testCap(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput,
      outputCycle
    )
    //********     II = 2     ********

    //********     II = 3     ********
    testII = 3
    outputCycle = dataSize * (testII + 2)

    infoFilename = "app-mapping-results/cap/ii3_i.txt"
    resultFilename = "app-mapping-results/cap/ii3_r.txt"

    a_base = 0
    c1_addr = 0
    m_base = 0
    b_base = 0

    constVals = Array(a_base, 3, c1_addr, 2, m_base, 2, b_base, 1)
    addrVals = Array(a_base, c1_addr, m_base, b_base)
    throughput = 1

    testCap(
      resultFilename,
      infoFilename,
      testII,
      constVals,
      addrVals,
      throughput,
      outputCycle
    )
    //********     II = 3     ********
  }
}
