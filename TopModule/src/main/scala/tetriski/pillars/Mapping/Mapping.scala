package tetriski.pillars.Mapping

import tetriski.pillars.core.MRRG

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
    var dfgFilename = "DOT/vadd/vadd.dot"
    var mrrgFilename = "MRRG/cgra-mrrg-4m4-ii1.txt"
    var outFilename = "app_mapping_results/vadd/ii1"
    var dfg: DFG = DotReader.loadDot(dfgFilename)
    //dfg.printDFG()
    println("dfg loaded!")
    var mrrg: MRRG = new MRRG()
    mrrg.loadtxt(mrrgFilename)
    println("mrrg loaded!")
    ILPMap.mapping(dfg, mrrg, filename = outFilename)
    Scheduler.schedule(dfg, mrrg, filename = outFilename, II = 1)
  }
}
