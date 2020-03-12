package tetriski.pillars
import tetriski.pillars.examples.{ApplicationExamples, HardwareExamples}

object Pillars {
  def main(args: Array[String]): Unit = {

    HardwareExamples.exampleAdres()
    HardwareExamples.exampleLSUAdres()
    HardwareExamples.exampleCompleteAdres()
//
//    ApplicationExamples.dumpWrapperVerilog()
//    ApplicationExamples.dumpArch(3, "MRRG/cgra-mrrg-4m4-ii3.txt")

    ApplicationExamples.exampleSum()
    ApplicationExamples.exampleVadd()
    ApplicationExamples.exampleAccum()
    ApplicationExamples.exampleCap()

  }
}


