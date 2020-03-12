package tetriski.pillars.Mapping

import tetriski.pillars.core._
import java.io._
object testILP {
  var dfgnames = Array(//"accum", "cap", "conv2", "conv3", "mac", "mac2",
    //"matrixmultiply", "mults1", "mults2", "nomem1",
    "simple", "simple2", "sum")
  dfgnames = Array("mac2","mults1")

  var mrrgnames = Array("full", "reduce", "simple", "full-simple")
  mrrgnames = Array("reduce","simple")
  def main(args: Array[String]) = {
    val fw = new FileWriter("test_mapping_result.txt")
//    fw.write("CGRA-ME:\n")
//    for (mrrgname <- mrrgnames) {
//      for (i <- 1 to 2) {
//        val mrrg = new MRRG()
//        mrrg.loadtxt("MRRG_CGRAME/cgra-mrrg-4m4-ii" + i + "-" + mrrgname + ".txt")
//        for (dfgname <- dfgnames) {
//          val dfg = DotReader.loadDot("DOT_CGRAME/" + dfgname + ".dot")
//          println("mapping " + dfgname + " to arch " + mrrgname + " ii = " + i)
//          val time = ILPMap.mapping(dfg, mrrg, fw = fw)
//          if(time == -1) {
//            fw.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " fail")
//          } else {
//            fw.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " success, time = " + time)
//          }
//          fw.write("\n")
//          fw.flush()
//        }
//      }
//    }
    fw.write("Pillars:\n")
    for (mrrgname <- mrrgnames) {
      for (i <- 2 to 2) {
        val mrrg = new MRRG()
        mrrg.loadtxt("MRRG/cgra-mrrg-4m4-ii" + i + "-" + mrrgname + ".txt")
        for (dfgname <- dfgnames) {
          val dfg = DotReader.loadDot("DOT_CGRAME/" + dfgname + ".dot")
          println("mapping " + dfgname + " to arch " + mrrgname + " ii = " + i)
          val time = ILPMap.mapping(dfg, mrrg, fw = fw)
          if(time == -1) {
            fw.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " fail")
          } else {
            fw.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " success, time = " + time)
          }
          fw.write("\n")
          fw.flush()
        }
      }
    }
    fw.close()
  }
}