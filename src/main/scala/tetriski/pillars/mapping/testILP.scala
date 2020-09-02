package tetriski.pillars.mapping

import tetriski.pillars.core._
import java.io._

/** This object is used for experimental study.
 *
 * @deprecated
 */
object testILP {
  var dfgnames = Array("accum", "cap", "conv2", "conv3", "mac", "mac2",
    "matrixmultiply", "mults1", "mults2", "nomem1",
    "simple", "simple2", "sum")
  //  dfgnames = Array("mults1", "mults2", "nomem1",
  //    "simple", "simple2", "sum")

  var mrrgnames = Array("full", "reduce", "simple", "full-simple")

  //  mrrgnames = Array("simple", "reduce")
  def main(args: Array[String]) = {
    val fw_c = new FileWriter("cgrame_mapping_result.txt")
    val fw_p = new FileWriter("pillars_mapping_result.txt")
    //    fw_c.write("CGRA-ME:\n")
    //    for (mrrgname <- mrrgnames) {
    //      for (i <- 1 to 2) {
    //        val mrrg = new MRRG()
    //        mrrg.loadtxt("MRRG_CGRAME/cgra-mrrg-4m4-ii" + i + "-" + mrrgname + ".txt")
    //        for (dfgname <- dfgnames) {
    //          val dfg = DotReader.loadDot("DOT_CGRAME/" + dfgname + ".dot")
    //          println("mapping " + dfgname + " to arch " + mrrgname + " ii = " + i)
    //          val time = ILPMap.mapping(dfg, mrrg, fw_c = fw_c)
    //          if(time == -1) {
    //            fw_c.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " fail")
    //          } else {
    //            fw_c.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " success, time = " + time)
    //          }
    //          fw_c.write("\n")
    //          fw_c.flush()
    //        }
    //      }
    //    }
    fw_p.write("Pillars:\n")
    fw_c.write("CGRA-ME:\n")

    for (i <- 1 to 2) {
      for (dfgname <- dfgnames) {
        val dfg = DotReader.loadDot("DOT_CGRAME/" + dfgname + ".dot")
        for (mrrgname <- mrrgnames) {
          val mrrg_p = new MRRG()
          val mrrg_c = new MRRG()
          mrrg_p.loadTXT("mrrg/cgra-mrrg-4m4-ii" + i + "-" + mrrgname + ".txt")
          mrrg_c.loadTXT("mrrg-cgrame/cgra-mrrg-4m4-ii" + i + "-" + mrrgname + ".txt")
          val time_p = ILPMap.mapping(dfg, mrrg_p, fw = fw_p)
          if (time_p == -1) {
            fw_p.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " fail")
          } else {
            fw_p.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " success, time = " + time_p)
          }
          fw_p.write("\n")
          fw_p.flush()

          val time_c = ILPMap.mapping(dfg, mrrg_c, fw = fw_c)
          if (time_c == -1) {
            fw_c.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " fail")
          } else {
            fw_c.write(mrrgname + " ii = " + i + " dfg = " + dfgname + " success, time = " + time_c)
          }
          fw_c.write("\n")
          fw_c.flush()
        }
      }
    }
    fw_c.close()
    fw_p.close()
  }
}