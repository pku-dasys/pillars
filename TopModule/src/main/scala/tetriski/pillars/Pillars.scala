package tetriski.pillars
import tetriski.pillars.archlib.BlockChain
import tetriski.pillars.core.ArchitctureHierarchy
import tetriski.pillars.examples.{ApplicationExamples, HardwareExamples}

object Pillars {
  def main(args: Array[String]): Unit = {

    HardwareExamples.exampleBlockChain()
    HardwareExamples.exampleAdres()
    HardwareExamples.exampleLSUAdres()
    HardwareExamples.exampleCompleteAdres()

//    ApplicationExamples.dumpWrapperVerilog()
//    ApplicationExamples.dumpArch(2, "MRRG/cgra-mrrg-4m4-ii2.txt")

    ApplicationExamples.exampleSum()
    ApplicationExamples.exampleVadd()
    ApplicationExamples.exampleAccum()
    ApplicationExamples.exampleCap()

  }
}


