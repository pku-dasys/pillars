package tetriski.pillars

import tetriski.pillars.examples.{ApplicationExamples, HardwareExamples}

/** Main object testing some examples.
 */
object Pillars {
  def main(args: Array[String]): Unit = {

    HardwareExamples.exampleBlockMesh()
    HardwareExamples.exampleAdres()
    HardwareExamples.exampleLSUAdres()
    HardwareExamples.exampleCompleteAdres()

//    ApplicationExamples.dumpWrapperVerilog()
//    ApplicationExamples.dumpArch(1, "mrrg/cgra-mrrg-4m4-ii1.txt")

    ApplicationExamples.exampleSum()
    ApplicationExamples.exampleVadd()
    ApplicationExamples.exampleAccum()
    ApplicationExamples.exampleCap()

  }
}


