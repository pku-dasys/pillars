package tetriski.pillars.examples

import chisel3.iotesters
import tetriski.pillars.archlib._
import tetriski.pillars.core.{ArchitectureHierarchy, Connect, ConstInfo, HardwareGenerator}
import tetriski.pillars.hardware.TopModule
import tetriski.pillars.mapping.{DFG, DotReader}
import tetriski.pillars.testers.{TopModule2PEUnitTest, TopModuleAdresUnitTest, TopModuleCompleteAdresUnitTest, TopModuleLSUAdresUnitTest}

/** Some unpacked examples showing how to test hardware generated by elements and blocks.
 * Some codes are unpacked, and they are replaced by functions and classes in ApplicationExamples.
 * Only critical schedules of modules are set by human decision.
 * It is worth noting that the option "--no-check-comb-loops" is needed when using PEs with two additional
 * bypass multiplexers.
 */
object HardwareExamples {
  /** A example for testing a PEBlock.
   * Since the configuration is set according to human computation, it is deprecated.
   *
   * @deprecated
   */
  def example2PE(): Unit = {

    val arch = new ArchitectureHierarchy()
    //The order of ports should be same as TopModule
    arch.addOutPorts(Array("output"))
    arch.addInPorts(Array("input_0", "input_1"))

    val pe0 = new PEBlock("pe0")
    val pe1 = new PEBlock("pe1")

    arch.addBlock(pe0)
    arch.addBlock(pe1)

    arch.addConnect(List(List("input_0"), List("pe0/", "input_0")))
    arch.addConnect(List(List("input_0"), List("pe0/", "input_1")))
    arch.addConnect(List(List("input_1"), List("pe0/", "input_2")))
    arch.addConnect(List(List("input_1"), List("pe1/", "input_0")))
    arch.addConnect(List(List("input_1"), List("pe1/", "input_1")))
    arch.addConnect(List(List("input_0"), List("pe1/", "input_2")))
    arch.addConnect(List(List("pe1/", "out_0"), List("pe0/", "input_3")))
    arch.addConnect(List(List("pe0/", "out_0"), List("pe1/", "input_3")))
    arch.addConnect(List(List("pe0/", "out_0"), List("output")))

    arch.init()

    arch.dumpArchitecture()

    val connectArray = arch.connectArray
    val connect = new Connect(connectArray)

    connect.dumpConnect()

    val hardwareGenerator = new HardwareGenerator(arch, connect)

    val dataWidth = 32

    //Verilog generation
    chisel3.Driver.execute(Array(""),
      () => new TopModule(hardwareGenerator.pillarsModuleInfo,
        hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth))

    //Run tester
    iotesters.Driver.execute(Array("-tgvo", "on"),
      () => new TopModule(hardwareGenerator.pillarsModuleInfo,
        hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)) {
      c => new TopModule2PEUnitTest(c)
    }
  }

  /** A example for testing a BlockMesh.
   * A correct MRRG is generated.
   */
  def exampleBlockMesh(): Unit = {
    val arch = new ArchitectureHierarchy()
    arch.addOutPorts(Array("out_0"))
    arch.addInPorts(Array("input_0", "input_1"))
    val blockMesh = new BlockMesh("blockMesh")
    arch.addBlock(blockMesh)
    arch.addConnect(arch.term("input_0") -> blockMesh / "in0")
    arch.addConnect(arch.term("input_1") -> blockMesh / "in1")
    arch.addConnect(blockMesh / "out0" -> arch.term("out_0"))
    arch.init()
    blockMesh.dumpMRRG(2, "BlockMesh.txt")
  }

  /** A example for testing a 2*2 TileBlock with 2 input ports and 1 output port.
   * "internalNodeinfo_simple.txt" is a pre-generated information TXT
   * when mapping a simple DFG with add and mul.
   */
  def exampleAdres(): Unit = {
    val arch = new ArchitectureHierarchy()
    //The order of ports should be same as TopModule
    arch.addOutPorts(Array("output"))
    arch.addInPorts(Array("input_0", "input_1"))

    val dataWidth = 32
    val tile = new TileBlock("tile_0", 2, 2, 2, 1, dataWidth = dataWidth)

    arch.addBlock(tile)

    arch.addConnect(List(List("input_0"), List("tile_0/", "input_0")))
    arch.addConnect(List(List("input_1"), List("tile_0/", "input_1")))
    arch.addConnect(List(List("tile_0/", "out_0"), List("output")))

    arch.init()

    arch.blockMap("tile_0").dumpMRRG(1)

    arch.dumpArchitecture()

    val connectArray = arch.connectArray

    val connect = new Connect(connectArray)

    connect.dumpConnect()

    val hardwareGenerator = new HardwareGenerator(arch, connect)


    //Verilog generation
    chisel3.Driver.execute(Array("--no-check-comb-loops", "-td", "ADRESv0"),
      () => new TopModule(hardwareGenerator.pillarsModuleInfo,
        hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth))

    val constInfo = new ConstInfo(1)
    constInfo.addConfig(arch("tile_0")("pe_0_0").getElement("const0").getModuleID(), 0, 4)
    constInfo.addConfig(arch("tile_0")("pe_1_0").getElement("const0").getModuleID(), 0, 5)

    val bitStreams = arch.genConfig("app-mapping-results/hardware-test/internalNodeinfo_simple.txt",
      1, constInfo)

    val bitStream = bitStreams(0)


    //Run tester
    iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
      () => new TopModule(hardwareGenerator.pillarsModuleInfo,
        hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)) {
      c => new TopModuleAdresUnitTest(c, bitStream)
    }
  }

  /** A example for testing a 2*2 TileLSUBlock with 2 input ports and 1 output port.
   * The functions of LSU are tested in this example.
   * "internalNodeinfo_lsu.txt" is a pre-generated information TXT
   * when mapping the DFG of sum.
   */
  def exampleLSUAdres(): Unit = {
    val arch = new ArchitectureHierarchy()
    //The order of ports should be same as TopModule
    arch.addOutPorts(Array("output"))
    arch.addInPorts(Array("input_0", "input_1"))

    val dataWidth = 32
    val tile = new TileLSUBlock("tile_0", 2, 2, 2, 1, dataWidth = dataWidth)

    arch.addBlock(tile)


    arch.addConnect(List(List("input_0"), List("tile_0/", "input_0")))
    arch.addConnect(List(List("input_1"), List("tile_0/", "input_1")))
    arch.addConnect(List(List("tile_0/", "out_0"), List("output")))

    arch.init()

    arch.blockMap("tile_0").dumpMRRG(1)

    arch.dumpArchitecture()

    val connectArray = arch.connectArray

    val connect = new Connect(connectArray)
    connect.dumpConnect()

    val hardwareGenerator = new HardwareGenerator(arch, connect)

    //Verilog generation
    chisel3.Driver.execute(Array("--no-check-comb-loops", "-td", "ADRESv1"),
      () => new TopModule(hardwareGenerator.pillarsModuleInfo,
        hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth))

    val constInfo = new ConstInfo(1)
    constInfo.addConfig(arch("tile_0")("pe_0_1").getElement("const0").getModuleID(), 0, 0)
    constInfo.addConfig(arch("tile_0")("pe_1_1").getElement("const0").getModuleID(), 0, 1)

    val bitStreams = arch.genConfig("app-mapping-results/hardware-test/internalNodeinfo_lsu.txt",
      1, constInfo)

    val bitStream = bitStreams(0)

    println(bitStream)

    arch("tile_0")("pe_0_0").getElement("alu0").setFireTime(3, 0)
    arch("tile_0")("pe_0_1").getElement("alu0").setFireTime(1, 0)
    arch("tile_0")("pe_1_1").getElement("alu0").setFireTime(1, 0)

    val schedules = arch.getSchedules()

    iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
      () => new TopModule(hardwareGenerator.pillarsModuleInfo,
        hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)) {
      c => new TopModuleLSUAdresUnitTest(c, bitStream, schedules)
    }
  }

  /** A example for testing a 4*4 TileCompleteBlock with 4 input ports and 4 output port.
   * The functions of global RF are tested in this example.
   * "internalNodeinfo_complete.txt" is a pre-generated information TXT
   * when mapping the DFG of sum.
   */
  def exampleCompleteAdres(): Unit = {
    val arch = new ArchitectureHierarchy()
    //The order of ports should be same as TopModule
    arch.addOutPorts(Array("out_0", "out_1", "out_2", "out_3"))
    arch.addInPorts(Array("input_0", "input_1", "input_2", "input_3"))

    val dataWidth = 32
    val tile = new TileCompleteBlock("tile_0", 4, 4, 4, 4, dataWidth = dataWidth)

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

    val targetedII = 3

    arch.blockMap("tile_0").dumpMRRG(targetedII)

    arch.dumpArchitecture()

    val connectArray = arch.connectArray

    val connect = new Connect(connectArray)

    connect.dumpConnect()

    val hardwareGenerator = new HardwareGenerator(arch, connect)


    //Verilog generation
    chisel3.Driver.execute(Array("--no-check-comb-loops", "-td", "ADRESv2"),
      () => new TopModule(hardwareGenerator.pillarsModuleInfo, hardwareGenerator.connectMap,
        hardwareGenerator.regionList, dataWidth))

    val constInfo = new ConstInfo(targetedII)
    constInfo.addConfig(arch("tile_0")("pe_0_3").getElement("const0").getModuleID(), 0, 1)
    constInfo.addConfig(arch("tile_0")("pe_3_1").getElement("const0").getModuleID(), 1, 1)

    val bitStreams = arch.genConfig("app-mapping-results/hardware-test/internalNodeinfo_complete.txt",
      targetedII, constInfo)

    arch("tile_0")("pe_3_0").getElement("alu0").setFireTime(4, 1)

    val schedules = arch.getSchedules()

    //Run tester
    iotesters.Driver.execute(Array("--no-check-comb-loops", "-tgvo", "on", "-tbn", "verilator"),
      () => new TopModule(hardwareGenerator.pillarsModuleInfo,
        hardwareGenerator.connectMap, hardwareGenerator.regionList, dataWidth)) {
      c => new TopModuleCompleteAdresUnitTest(c, bitStreams, schedules)
    }
  }
}
