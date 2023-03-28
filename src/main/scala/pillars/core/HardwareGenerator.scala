package pillars.core

import scala.collection.mutable.ArrayBuffer

/** A class combining the ArchitctureHierarchy class and Connect class
 * to help TopModule generate the final top design.
 *
 * @param arch    the abstract top design model
 * @param connect a class containing the connections between ports
 */
class HardwareGenerator(arch: BlockTrait, connect: Connect) {
  /** A map showing the connections between ports in Int format.
   */
  var connectMap = Map[List[Int], List[List[Int]]]()

  /** A map showing the connections between ports in String format.
   */
  var mapRelation = connect.mapRelation

  /** Get the map showing the connections between a port and its fan-outs in Int format.
   *
   * @example List(1, 2, 2) represents the port "2" of the module "2" in the type "1".
   * @param src  a list representing the source port
   * @param dsts a array of lists representing the sink ports
   * @return the map showing the connections between a port and its fan-outs in Int format
   */
  def getConnectList(src: List[String], dsts: ArrayBuffer[List[String]]): Map[List[Int], List[List[Int]]] = {
    /** Encode a string representation of a port into a list of integer as mentioned before.
     *
     * @param strs a list of String representing this port
     * @return a list of Int representing this port
     */
    def encode(strs: List[String]): List[Int] = {
      val ret = List[Int]()
      if (strs.size == 2) {
        return List(arch.typeNum, 0, arch.getPortID(strs(1)))
      }

      var temp = arch
      for (i <- 1 until strs.size) {
        if (strs(i)(strs(i).size - 1) == '/') {
          temp = temp(strs(i).substring(0, strs(i).size - 1))
        } else if (i == strs.size - 2) {
          val target = temp.getElement(strs(i))
          return List(target.getTypeID(), target.getModuleID(), target.getPortID(strs(strs.size - 1)))
        }
      }
      ret
    }

    val encodeSrc = encode(src)
    val encodeDsts = dsts.map(encode).toList
    Map(encodeSrc -> encodeDsts)
  }

  /** Integer representation of connection, which is needed in the top design.
   */
  var connectList = mapRelation.map((x) => getConnectList(x._1, x._2))
    .foreach((x) => connectMap = connectMap.++(x))

  /** A class containing the parameters of modules and the port number of top design.
   */
  val pillarsModuleInfo = arch.asInstanceOf[ArchitectureHierarchy].getPillarsModuleInfo()

  /** A list of regions where modules share a configuration controller.
   */
  val regionList = arch.asInstanceOf[ArchitectureHierarchy].getRegionList()

}
