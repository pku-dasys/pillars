package tetriski.pillars.core

import scala.collection.mutable.ArrayBuffer

/** Since almost all elements and blocks has the common 'port' array,
 * we extract the 'port' array and wrap it in the Trait to be extended.
 */
trait Ports {
  /** A map between name and identification number of a input port.
   */
  var inPortMap = Map[String, Int]()

  /** A map between name and identification number of a output port.
   */
  var outPortMap = Map[String, Int]()

  /** The array of names of input ports.
   */
  var inPorts = ArrayBuffer[String]()

  /** The array of names of output ports.
   */
  var outPorts = ArrayBuffer[String]()

  /** Add some input ports.
   *
   * @param args a array of names of input ports.
   * @return inPortMap after add this ports
   */
  def addInPorts(args: Array[String]): Map[String, Int] = {
    val currentSize = inPortMap.size
    for (i <- 0 until args.length) {
      inPortMap += (args(i) -> (i + currentSize))
      inPorts.append(args(i))
    }
    inPortMap
  }

  /** Add some output ports.
   *
   * @param args a array of names of output ports.
   * @return outPortMap after add this ports
   */
  def addOutPorts(args: Array[String]): Map[String, Int] = {
    val currentSize = outPortMap.size
    for (i <- 0 until args.length) {
      outPortMap += (args(i) -> (i + currentSize))
      outPorts.append(args(i))
    }
    outPortMap
  }

  /** Get names the input ports.
   *
   * @return names the input ports
   */
  def getInPorts(): Iterable[String] = {
    inPorts
  }

  /** Get names the output ports.
   *
   * @return names the output ports
   */
  def getOutPorts(): Iterable[String] = {
    outPorts
  }

  /** Get the identification number of a port with its name.
   *
   * @param name the name of this port
   * @return the identification number of this port
   */
  def getPortID(name: String): Int = {
    if (inPortMap.contains(name)) {
      inPortMap(name)
    } else if (outPortMap.contains(name)) {
      outPortMap(name)
    } else {
      println("No port exception " + name)
      -1
    }
  }
}
