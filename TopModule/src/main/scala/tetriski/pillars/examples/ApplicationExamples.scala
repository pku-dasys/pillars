package tetriski.pillars.examples

import chisel3.iotesters
import tetriski.pillars.archlib.TileCompleteBlock
import tetriski.pillars.core.{ArchitctureHierarchy, Connect, ConstInfo, HardwareGeneration, ModuleTrait}
import tetriski.pillars.hardware.TopModule
import tetriski.pillars.testers.{SumTester, TopModuleCompleteAdresUnitTest}

object ApplicationExamples {
  def exampleSum(): Unit ={
    var arch = new ArchitctureHierarchy()
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

    val targetII = 3

    arch.blockMap("tile_0").dumpMRRG(targetII)

    arch.dumpArchitcture()

    val connectArray = arch.connectArray

    val connect = new Connect(connectArray)

    connect.dumpConnect()

    val cp = new HardwareGeneration(arch, connect)

    val dataWidth = 32

    //Verilog generation
    chisel3.Driver.execute(Array("--no-check-comb-loops", "-td","ADRESv2"),
      () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth))

    val constInfo = new ConstInfo(targetII)
    constInfo.addConst(arch("tile_0")("pe_1_0").getModule("const0").getModuleID(), 1, 1)
    constInfo.addConst(arch("tile_0")("pe_0_2").getModule("const0").getModuleID(), 1, 1)

    val bitStreams = arch.genConfig("internalNodeinfo_complete.txt", targetII, constInfo)

    arch("tile_0")("pe_1_1").getModule("alu0").setWaitCycle(4, 0)

    val waitCycles = arch.aluArray.map(alu => alu.asInstanceOf[ModuleTrait].getWaitCycles().toList).reduce(_++_)

    iotesters.Driver.execute(Array("--no-check-comb-loops","-tgvo", "on", "-tbn" ,"verilator"),
      () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
      c => new SumTester(c, bitStreams, waitCycles)
    }
  }
}
