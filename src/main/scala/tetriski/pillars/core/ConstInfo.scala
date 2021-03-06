package tetriski.pillars.core

import scala.collection.mutable.ArrayBuffer

/** A class containing const values, the corresponding RCs and identification number of const units.
 *
 * @param initII the targeted II
 */
class ConstInfo(initII: Int = 0) {
  /** A two dimensional array.
   * We can get a array of the identification numbers of const units in a reconfiguration cycle
   * with constIDArray(RC).
   */
  val constIDArray = new ArrayBuffer[ArrayBuffer[Int]]()

  /** A two dimensional array.
   * We can get a array of values of const units in a reconfiguration cycle
   * with constValArray(RC).
   */
  val constValArray = new ArrayBuffer[ArrayBuffer[Int]]()

  /** A targeted II.
   */
  var targetedII = initII

  for (i <- 0 until targetedII) {
    constIDArray.append(new ArrayBuffer[Int]())
    constValArray.append(new ArrayBuffer[Int]())
  }

  /** Add a item of const information.
   *
   * @param constID  the identification number of a const unit
   * @param RC       the reconfiguration cycle
   * @param constVal the const value
   */
  def addConst(constID: Int, RC: Int, constVal: Int): Unit = {
    constIDArray(RC).append(constID)
    constValArray(RC).append(constVal)
  }

  /** Reset values in this class.
   */
  def reset(newTestII: Int): Unit = {
    constIDArray.clear()
    constValArray.clear()
    targetedII = newTestII
    for (i <- 0 until targetedII) {
      constIDArray.append(new ArrayBuffer[Int]())
      constValArray.append(new ArrayBuffer[Int]())
    }
  }
}
