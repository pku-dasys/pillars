package tetriski.pillars

import chisel3.iotesters

import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}

import chisel3.util.log2Up

import scala.collection.mutable.Queue


object Pillars {
  def main(args: Array[String]): Unit = {

    def example2PE(): Unit ={
      var arch = new ArchitctureHierarchy()
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

      //Verilog generation
      chisel3.Driver.execute(args, () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, 32))

      //Run tester
      iotesters.Driver.execute(args, () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, 32)) {
        c => new TopModule2PEUnitTest(c)
      }
    }

    def exampleAdres(): Unit ={
      var arch = new ArchitctureHierarchy()
      //The order of ports should be same as TopModule
      arch.addOutPorts(Array("output"))
      arch.addInPorts(Array("input_0", "input_1"))

      val tile = new TileBlock("tile_0", 2, 2, 2, 1)

      arch.addBlock(tile)


      arch.addConnect(List(List("input_0"),List("tile_0/", "input_0")))
      arch.addConnect(List(List("input_1"),List("tile_0/", "input_1")))
      arch.addConnect(List(List("tile_0/","out_0"),List("output")))

      arch.init()

      arch.blockMap("tile_0").dumpMRRG()

      arch.dumpArchitcture()

      val connectArray = arch.connectArray

      //println(connectArray)
      val connect = new Connect(connectArray)
      //val test = connect.getConnect()

      connect.dumpConnect()

      val cp = new HardwareGeneration(arch, connect)

      println(cp.connectMap)

      //Verilog generation
      chisel3.Driver.execute(Array("--no-check-comb-loops"), () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, 32))

      arch("tile_0")("pe_0_0").getModule("const0").updateConfigArray(17)

      arch.genConfig("internalNodeinfo.txt")

      val bitStream = arch.getConfigBitStream()


      //Run tester
      iotesters.Driver.execute(Array("--no-check-comb-loops","--backend-name", "firrtl"), () => new TopModule(cp.pillarsModuleInfo, cp.connectMap, cp.configList, 32)) {
        c => new TopModuleAdresUnitTest(c, bitStream)
      }
    }

    example2PE()
    exampleAdres()

  }
}


