package tetriski.pillars.core

import scala.collection.mutable.ArrayBuffer

////This class combines the ArchitctureHierarchy class and Connect class to help TopModule generate final
////hardware modules with linking information.
class HardwareGeneration(arch: BlockTrait, connect: Connect) {
  var connectMap = Map[List[Int], List[List[Int]]]()
  var mapRelation = connect.mapRelation

  def getConnectList(src: List[String], dsts: ArrayBuffer[List[String]]): Map[List[Int], List[List[Int]]] = {
    //Encode a string representation of a port into a list of integer as mentioned before
    def encode(strs: List[String]): List[Int] = {
      var ret = List[Int]()
      if (strs.size == 2) {
        return List(arch.typeNum, 0, arch.getPortID(strs(1)))
      }

      var temp = arch
//      println(strs)
      for (i <- 1 until strs.size) {
        if (strs(i)(strs(i).size - 1) == '/' ) {
          temp = temp(strs(i).substring(0, strs(i).size - 1))
        } else if (i == strs.size - 2) {
          var target = temp.getModule(strs(i))
          return List(target.getTypeID(), target.getModuleID(), target.getPortID(strs(strs.size - 1)))
        }

      }
      ret
    }

    val encodeSrc = encode(src)
    val encodeDsts = dsts.map(encode).toList

    //    println("src: ", encodeSrc, src)
    //    println("dst: ", encodeDsts, dsts)

    Map(encodeSrc -> encodeDsts)
  }

  //Return integer representation of connection, which is needed in TopModule
  var connectList = mapRelation.map((x) => getConnectList(x._1, x._2)).foreach((x) => connectMap = connectMap.++(x))

  val pillarsModuleInfo = arch.asInstanceOf[ArchitctureHierarchy].getPillarsModuleInfo()

  val configList = arch.asInstanceOf[ArchitctureHierarchy].getConfigList()

}
