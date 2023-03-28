package pillars.core

import OpEnum.OpEnum
import pillars.archlib.ElementCounter

import scala.collection.mutable.ArrayBuffer
import pillars.hardware.PillarsConfig._

/** Basic trait of elements and blocks.
 * It helps to produce an in-memory model for both behavior and structure.
 */
trait BasicTrait extends Ports {
  /** The MRRG corresponding to this element.
   */
  var mrrg = new MRRG()

  /** The type identification number of the module represented by this abstract model.
   */
  var typeID = -1

  /** The module identification number of the module represented by this abstract model.
   */
  var moduleID = -1

  /** The width of the module represented by this abstract model.
   */
  var width = -1

  /** The name of this abstract model.
   */
  var name = ""

  /** The opcodes supported by the module represented by this abstract model.
   */
  var supOps = new ArrayBuffer[OpEnum]

  /** The parameters of the module represented by this abstract model.
   */
  var params = new ArrayBuffer[Int]

  /** The configuration bits of the module represented by this abstract model.
   */
  var configBit = 0

  /** The configuration of the module represented by this abstract model.
   */
  var configArray = new ArrayBuffer[Int]

  /** The fire times of the module represented by this abstract model in different reconfiguration cycles.
   */
  var fireTimes = new Array[Int](II_UPPER_BOUND)

  /** The skews of the module represented by this abstract model in different reconfiguration cycles.
   */
  var skews = new Array[Int](II_UPPER_BOUND)

  /** Get a port of this element.
   *
   * @param portName the name of this port
   * @return a valid port
   */
  def /(portName: String): ValidPort = {
    if (!(getInPorts().toSet.contains(portName) || getOutPorts().toSet.contains(portName))) {
      System.err.println(s"Invalid port name $name / $portName!")
    }
    new ValidPort(name, portName)
  }

  /** Initialize MRRG.
   *
   * @return the MRRG
   */
  def initMRRG(): MRRG = {

    for (inPort <- inPorts) {
      val node = new NodeMRRG(inPort)
      mrrg.addNode(node)
    }
    for (outPort <- outPorts) {
      val node = new NodeMRRG(outPort)
      mrrg.addNode(node)
    }

    mrrg
  }

  /** Set the module identification number of the module represented by this abstract model.
   *
   * @param arg the new module identification number
   */
  def setModuleID(arg: Int): Unit = {
    moduleID = arg
  }

  /** Set the type identification number of the module represented by this abstract model.
   *
   * @param arg the new type identification number
   */
  def setTypeID(arg: Int): Unit = {
    typeID = arg
  }

  /** Set the width of the module represented by this abstract model.
   *
   * @param arg the new width
   */
  def setWidth(arg: Int): Unit = {
    width = arg
  }

  /** Set the name of this abstract model.
   *
   * @param arg the new width
   */
  def setName(arg: String): Unit = {
    name = arg
  }

  /** Set the opcodes supported by the module represented by this abstract model.
   * When using this function,
   * users should guarantee correct correspondence of the opcodes owned by a
   * functional node and the functional capabilities of a module.
   *
   * @param arg the new opcodes
   */
  def setSupOps(arg: List[OpEnum]): Unit = {
    supOps.clear()
    arg.foreach(t => supOps.append(t))
  }

  /** Set a list of parameters of the module represented by this abstract model.
   * Configuration bits is the end of the list.
   *
   * @param arg a list of the new parameters
   */
  def setParams(arg: List[Int]): Unit = {
    arg.foreach(t => params.append(t))
    setConfigBit(arg(arg.length - 1))
  }

  /** Set configuration bits of the module represented by this abstract model.
   *
   * @param arg the new configuration bits
   */
  def setConfigBit(arg: Int): Unit = {
    configBit = arg
    for (i <- 0 until arg)
      configArray.append(0)
  }

  /** Set fire time of the module represented by this abstract model in a reconfiguration cycle.
   *
   * @param fireTime the fire time
   * @param RC       the reconfiguration cycle
   */
  def setFireTime(fireTime: Int, RC: Int): Unit = {
    fireTimes(RC) = fireTime
  }

  /** Set skew of the module represented by this abstract model in a reconfiguration cycle.
   *
   * @param skew the skew
   * @param RC   the reconfiguration cycle
   */
  def setSkew(skew: Int, RC: Int): Unit = {
    skews(RC) = skew
  }

  /** Reset all fire times.
   *
   * @param t the default fire time
   */
  def resetFireTimes(t: Int = -1): Unit = {
    for (i <- 0 until II_UPPER_BOUND) {
      fireTimes(i) = t
    }
  }

  /** Reset all skews.
   */
  def resetSkew(): Unit = {
    for (i <- 0 until II_UPPER_BOUND) {
      skews(i) = 0
    }
  }

  /** Get the module identification number of the module represented by this abstract model.
   *
   * @return the module identification number of the module represented by this abstract model
   */
  def getModuleID(): Int = {
    moduleID
  }

  /** Get the type identification number of the module represented by this abstract model.
   *
   * @return the type identification number of the module represented by this abstract model
   */
  def getTypeID(): Int = {
    typeID
  }

  /** Get the name of the this abstract model.
   *
   * @return the name of the this abstract model
   */
  def getName(): String = {
    name
  }

  /** Get the width of the module represented by this abstract model.
   *
   * @return the width of the module represented by this abstract model
   */
  def getWidth(): Int = {
    params(params.length - 2)
  }

  /** Get the opcodes supported by the module represented by this abstract model.
   *
   * @return the opcodes supported by the module represented by this abstract model
   */
  def getSupOps(): ArrayBuffer[OpEnum] = {
    supOps
  }

  /** Get the parameters of the module represented by this abstract model.
   *
   * @return the parameters of the module represented by this abstract model.
   */
  def getParams(): List[Int] = {
    params.toList
  }

  /** Get the configuration bits of the module represented by this abstract model.
   *
   * @return the configuration bits of the module represented by this abstract model.
   */
  def getConfigBit(): Int = {
    configBit
  }

  /** Get the fire times of the module represented by this abstract model.
   *
   * @return the fire times of the module represented by this abstract model.
   */
  def getFireTimes(): Array[Int] = {
    fireTimes
  }

  /** Get the schedule of the module represented by this abstract model in Int format.
   *
   * @example If II_UPPER_BOUND = 1, only schedule at reconfiguration cycle 0 will be got.
   *          If skew = 2, fire time = 3, LOG_SKEW_LENGTH = 2 and LOG_SCHEDULE_SIZE = 3, the result is Array(19).
   *          If skew = -2, fire time = 3, LOG_SKEW_LENGTH = 2 and LOG_SCHEDULE_SIZE = 3, the result is Array(51).
   * @return the schedule of the module represented by this abstract model
   */
  def getSchedule(): Array[Int] = {
    val ret = new Array[Int](II_UPPER_BOUND)
    for (i <- 0 until II_UPPER_BOUND) {

      var fireTime = fireTimes(i)
      var mask = 0
      for (m <- 0 until LOG_SCHEDULE_SIZE) {
        mask += 1 << m
      }
      var sche = fireTime & mask

      if (LOG_SKEW_LENGTH > 0 && (!this.isInstanceOf[ElementCounter])) {
        var skew = skews(i)
        if (skew < 0 && USE_RELATIVE_SKEW) {
          skew = Math.pow(2, LOG_SKEW_LENGTH).toInt - skew
        }
        sche += (skew << LOG_SCHEDULE_SIZE)
      }
      ret(i) = sche
    }
    ret
  }

}
