package tetriski.pillars

import chisel3.iotesters

import tetriski.pillars.archlib.{PEBlock, TileBlock, TileCompleteBlock, TileLSUBlock}
import tetriski.pillars.core.{ArchitctureHierarchy, Connect, HardwareGeneration, ModuleTrait, ConstInfo}
import tetriski.pillars.hardware.TopModule
import tetriski.pillars.testers.{TopModule2PEUnitTest, TopModuleAdresUnitTest,
  TopModuleCompleteAdresUnitTest, TopModuleLSUAdresUnitTest}



object Pillars {
  def main(args: Array[String]): Unit = {

    def example2PE(): Unit ={
      val arch = new ArchitctureHierarchy()
      //The order of ports should be same as TopModule
      arch.addOutPorts(Array("output"))
      arch.addInPorts(Array("input_0", "input_1"))

      val pe0 = new PEBlock("pe0")
      val pe1 = new PEBlock("pe1")

      arch.addBlock(pe0)
      arch.addBlock(pe1)

      arch.addConnect(List(List("input_0"),List("pe0/", "input_0")))
      arch.addConnect(List(List("input_0"),List("pe0/", "input_1")))
      arch.addConnect(List(List("input_1"),List("pe0/", "input_2")))
      arch.addConnect(List(List("input_1"),List("pe1/", "input_0")))
      arch.addConnect(List(List("input_1"),List("pe1/", "input_1")))
      arch.addConnect(List(List("input_0"),List("pe1/", "input_2")))
      arch.addConnect(List(List("pe1/","out_0"),List("pe0/", "input_3")))
      arch.addConnect(List(List("pe0/","out_0"),List("pe1/", "input_3")))
      arch.addConnect(List(List("pe0/","out_0"),List("output")))

      arch.init()

      //arch("pe0").dumpMRRG()

      arch.dumpArchitcture()

      val connectArray = arch.connectArray

      val connect = new Connect(connectArray)
      //val test = connect.getConnect()

      connect.dumpConnect()

      val cp = new HardwareGeneration(arch, connect)

      //println(cp.connectMap)
      val dataWidth = 32

      //Verilog generation
      chisel3.Driver.execute(args, () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth))

      //Run tester
      iotesters.Driver.execute(Array("-tgvo", "on", "-tbn" ,"verilator"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
        c => new TopModule2PEUnitTest(c)
      }
    }

    def exampleAdres(): Unit ={
      val arch = new ArchitctureHierarchy()
      //The order of ports should be same as TopModule
      arch.addOutPorts(Array("output"))
      arch.addInPorts(Array("input_0", "input_1"))

      val tile = new TileBlock("tile_0", 2, 2, 2, 1)

      arch.addBlock(tile)


      arch.addConnect(List(List("input_0"),List("tile_0/", "input_0")))
      arch.addConnect(List(List("input_1"),List("tile_0/", "input_1")))
      arch.addConnect(List(List("tile_0/","out_0"),List("output")))

      arch.init()

      arch.blockMap("tile_0").dumpMRRG(1)

      arch.dumpArchitcture()

      val connectArray = arch.connectArray

      //println(connectArray)
      val connect = new Connect(connectArray)
      //val test = connect.getConnect()

      connect.dumpConnect()

      val cp = new HardwareGeneration(arch, connect)

//      println(cp.connectMap)

      val dataWidth = 32

      //Verilog generation
      chisel3.Driver.execute(Array("--no-check-comb-loops", "-td","ADRESv0"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth))

      val constInfo = new ConstInfo(1)
      constInfo.addConst(arch("tile_0")("pe_0_0").getModule("const0").getModuleID(), 0, 4)
      constInfo.addConst(arch("tile_0")("pe_1_0").getModule("const0").getModuleID(), 0, 5)

      val bitStreams = arch.genConfig("internalNodeinfo_simple.txt", 1, constInfo)


      val bitStream = bitStreams(0)

//      println(bitStream)


//
//      iotesters.Driver.execute(Array( "--no-check-comb-loops","-tiac", "-tiwv"),
      //      () => new DispatchT(191, List(47, 47, 3, 47, 47))) {
//        c => new DispatchUnitTest(c, bitStream)
//      }


      //Run tester
      iotesters.Driver.execute(Array( "--no-check-comb-loops","-tgvo", "on", "-tbn" ,"verilator"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
        c => new TopModuleAdresUnitTest(c, bitStream)
      }
    }

    def exampleLSUAdres(): Unit ={
      val arch = new ArchitctureHierarchy()
      //The order of ports should be same as TopModule
      arch.addOutPorts(Array("output"))
      arch.addInPorts(Array("input_0", "input_1"))

      val tile = new TileLSUBlock("tile_0", 2, 2, 2, 1)

      arch.addBlock(tile)


      arch.addConnect(List(List("input_0"),List("tile_0/", "input_0")))
      arch.addConnect(List(List("input_1"),List("tile_0/", "input_1")))
      arch.addConnect(List(List("tile_0/","out_0"),List("output")))

      arch.init()

      arch.blockMap("tile_0").dumpMRRG(1)

      arch.dumpArchitcture()

      val connectArray = arch.connectArray

      //println(connectArray)
      val connect = new Connect(connectArray)
      //val test = connect.getConnect()

      connect.dumpConnect()

      val cp = new HardwareGeneration(arch, connect)

//      println(cp.connectMap)

      val dataWidth = 32

      //Verilog generation
      chisel3.Driver.execute(Array("--no-check-comb-loops", "-td","ADRESv1"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth))

      val constInfo = new ConstInfo(1)
      constInfo.addConst(arch("tile_0")("pe_0_1").getModule("const0").getModuleID(), 0, 1)
      constInfo.addConst(arch("tile_0")("pe_1_1").getModule("const0").getModuleID(), 0, 1)

      val bitStreams = arch.genConfig("internalNodeinfo_lsu.txt", 1, constInfo)

      val bitStream = bitStreams(0)

      println(bitStream)

      arch("tile_0")("pe_1_1").getModule("alu0").setWaitCycle(1, 0)
      arch("tile_0")("pe_0_0").getModule("alu0").setWaitCycle(3, 0)

      val waitCycles = arch.aluArray.map(alu => alu.asInstanceOf[ModuleTrait].getWaitCycles().toList).reduce(_++_)


      //
      //      iotesters.Driver.execute(Array( "--no-check-comb-loops","-tiac", "-tiwv"),
      //      () => new DispatchT(191, List(47, 47, 3, 47, 47))) {
      //        c => new DispatchUnitTest(c, bitStream)
      //      }


      //Run tester
//      iotesters.Driver.execute(Array( "--no-check-comb-loops","-tiac", "-tiwv"),
      //      () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, 32)) {
      iotesters.Driver.execute(Array( "--no-check-comb-loops","-tgvo", "on", "-tbn" ,"verilator"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
         c => new TopModuleLSUAdresUnitTest(c, bitStream, waitCycles)
      }
    }

    def exampleCompleteAdres(): Unit ={
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

      //println(connectArray)
      val connect = new Connect(connectArray)
      //val test = connect.getConnect()

      connect.dumpConnect()

      val cp = new HardwareGeneration(arch, connect)

      //      println(cp.connectMap)

      val dataWidth = 32

      //Verilog generation
      chisel3.Driver.execute(Array("--no-check-comb-loops", "-td","ADRESv2"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth))

      val constInfo = new ConstInfo(targetII)
      constInfo.addConst(arch("tile_0")("pe_1_0").getModule("const0").getModuleID(), 1, 1)
      constInfo.addConst(arch("tile_0")("pe_0_2").getModule("const0").getModuleID(), 1, 1)

//      arch.genConfig("internalNodeinfo_complete.txt", 1)
      val bitStreams = arch.genConfig("internalNodeinfo_complete.txt", targetII, constInfo)

//      arch("tile_0")("pe_0_3").getModule("const0").updateConfigArray(1)
//      arch("tile_0")("pe_0_0").getModule("const0").updateConfigArray(1)



//      val bitStream = arch.getConfigBitStream()
//
//      println(bitStream)

//      arch("tile_0")("pe_0_2").getModule("alu0").setWaitCycle(1)
      arch("tile_0")("pe_1_1").getModule("alu0").setWaitCycle(4, 0)

      val waitCycles = arch.aluArray.map(alu => alu.asInstanceOf[ModuleTrait].getWaitCycles().toList).reduce(_++_)


      //
      //      iotesters.Driver.execute(Array( "--no-check-comb-loops","-tiac", "-tiwv"),
      //      () => new DispatchT(191, List(47, 47, 3, 47, 47))) {
      //        c => new DispatchUnitTest(c, bitStream)
      //      }


      //Run tester
            //iotesters.Driver.execute(Array( "--no-check-comb-loops","-tiac", "-tiwv"),
      // () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, 32)) {
      iotesters.Driver.execute(Array("--no-check-comb-loops","-tgvo", "on", "-tbn" ,"verilator"),
        () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, dataWidth)) {
        c => new TopModuleCompleteAdresUnitTest(c, bitStreams, waitCycles)
      }
    }

//    example2PE()
//    exampleAdres()
//    exampleLSUAdres()
    exampleCompleteAdres()

  }
}


