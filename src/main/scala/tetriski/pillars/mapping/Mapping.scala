package tetriski.pillars.mapping

import tetriski.pillars.core.MRRG

/** A standalone mapping example.
 */
object Mapping {
  //  def main(args: Array[String]) = {
  //    if(args.length != 2) {
  //      println("args size error!")
  //    } else {
  //      var dfg : DFG = DotReader.loadDot(args(0))
  //      //dfg.printDFG()
  //      println("dfg loaded!")
  //      var mrrg : MRRG = new MRRG()
  //      mrrg.loadtxt(args(1))
  //      println("mrrg loaded!")
  //      ILPMap.mapping(dfg, mrrg)
  //      Scheduler.schedule(dfg, mrrg)
  //    }
  //  }
  def main(args: Array[String]): Unit = {
    //    System.out.println(System.getProperty("java.library.path"))
    val dfgFilename = "dfg/vadd/vadd.dot"
//    val dfgFilename = "tutorial/MM.dot"
//    val mrrgFilename = "MRRG/cgra-mrrg-4m4-ii1.txt"
    val mrrgFilename = "mrrg/cgra-mrrg-4m4-ii1.txt"
    val outFilename = "tutorial/ii1"
    val dfg: DFG = DotReader.loadDot(dfgFilename)
    //dfg.printDFG()
    println("dfg loaded!")
    val mrrg: MRRG = new MRRG()
    mrrg.loadTXT(mrrgFilename)
    println("mrrg loaded!")
    ILPMap.mapping(dfg, mrrg, filename = outFilename)
    //Scheduler.schedule(dfg, mrrg, filename = outFilename, II = 1)
  }
}
