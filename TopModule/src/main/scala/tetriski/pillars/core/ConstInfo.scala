package tetriski.pillars.core

import scala.collection.mutable.ArrayBuffer

class ConstInfo(maxII: Int) {
  val constIDArray = new ArrayBuffer[ArrayBuffer[Int]]()
  val constValArray = new ArrayBuffer[ArrayBuffer[Int]]()

  for(i <- 0 until maxII){
    constIDArray.append(new ArrayBuffer[Int]())
    constValArray.append(new ArrayBuffer[Int]())
  }

  def addConst(constID: Int, II: Int, constVal: Int): Unit ={
    constIDArray(II).append(constID)
    constValArray(II).append(constVal)
  }
}
