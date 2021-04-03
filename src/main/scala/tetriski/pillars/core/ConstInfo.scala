package tetriski.pillars.core

import scala.collection.mutable.ArrayBuffer

/** A class containing config values, the corresponding RCs
 * and identification number of modules with constant config.
 *
 * @param initII the targeted II
 */
class ConstInfo(initII: Int = 0) {
  /** A two dimensional array.
   * We can get a array of the identification numbers of const units in a reconfiguration cycle
   * with constIDArray(RC).
   */
  val IDArray = new ArrayBuffer[ArrayBuffer[Int]]()

  /** A two dimensional array.
   * We can get a array of values of const units in a reconfiguration cycle
   * with constValArray(RC).
   */
  val configArray = new ArrayBuffer[ArrayBuffer[BigInt]]()

  /** A targeted II.
   */
  var targetedII = initII

  for (i <- 0 until targetedII) {
    IDArray.append(new ArrayBuffer[Int]())
    configArray.append(new ArrayBuffer[BigInt]())
  }

  /** Add a item of config information.
   *
   * @param ID  the identification number of a module with constant config
   * @param RC       the reconfiguration cycle
   * @param configVal the config value
   */
  def addConfig(ID: Int, RC: Int, configVal: BigInt): Unit = {
    IDArray(RC).append(ID)
    configArray(RC).append(configVal)
  }

  /** Reset values in this class.
   */
  def reset(newTestII: Int): Unit = {
    IDArray.clear()
    configArray.clear()
    targetedII = newTestII
    for (i <- 0 until targetedII) {
      IDArray.append(new ArrayBuffer[Int]())
      configArray.append(new ArrayBuffer[BigInt]())
    }
  }
}
