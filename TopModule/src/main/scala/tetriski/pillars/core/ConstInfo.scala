package tetriski.pillars.core

import scala.collection.mutable.ArrayBuffer

class ConstInfo(initII: Int = 0) {
  val constIDArray = new ArrayBuffer[ArrayBuffer[Int]]()
  val constValArray = new ArrayBuffer[ArrayBuffer[Int]]()
  var testII = initII

  for(i <- 0 until testII){
    constIDArray.append(new ArrayBuffer[Int]())
    constValArray.append(new ArrayBuffer[Int]())
  }

  def addConst(constID: Int, II: Int, constVal: Int): Unit ={
    constIDArray(II).append(constID)
    constValArray(II).append(constVal)
  }

  def reset(newTestII: Int): Unit ={
    constIDArray.clear()
    constValArray.clear()
    testII = newTestII
    for(i <- 0 until testII){
      constIDArray.append(new ArrayBuffer[Int]())
      constValArray.append(new ArrayBuffer[Int]())
    }
  }
}
