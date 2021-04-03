package tetriski.pillars.core

import scala.collection.mutable.ArrayBuffer

/** A class containing basic parameters (configs), the corresponding RCs and identification number of counters.
 *
 * @param initII the targeted II
 * @param w      the data width
 */
class CounterInfo(initII: Int = 0, w: Int) extends ConstInfo(initII) {



  val ParamArray = new ArrayBuffer[ArrayBuffer[CounterParameter]]()

  for (i <- 0 until targetedII) {
    ParamArray.append(new ArrayBuffer[CounterParameter]())
  }

  def getConfig(p : CounterParameter): BigInt = {
    (p.freq << (w * 3)) + (p.end << (w * 2)) + (p.step << w) + p.init
  }

  def addConfig(ID: Int, RC: Int, p : CounterParameter): Unit ={
    ParamArray(RC).append(p)
    addConfig(ID, RC, getConfig(p))
  }

  override def reset(newTestII: Int){
    ParamArray.clear()
    for (i <- 0 until newTestII) {
      ParamArray.append(new ArrayBuffer[CounterParameter]())
    }
    super.reset(newTestII)
  }
}

class CounterParameter {
  var freq = 0
  var end = 0
  var step = 0
  var init = 0

  def this(freq: Int, end: Int, step: Int, init: Int) {
    this()
    this.freq = freq
    this.end = end
    this.step = step
    this.init = init
  }

}