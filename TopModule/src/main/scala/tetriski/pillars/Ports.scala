package tetriski.pillars

import scala.collection.mutable.ArrayBuffer

//Since almost every modules has the common 'port' array, we extract
//the 'port' array and wrap it in the Trait to be extended.
trait Ports {
  var inPortMap = Map[String, Int]()
  var outPortMap = Map[String, Int]()

  var inPorts = ArrayBuffer[String]()
  var outPorts = ArrayBuffer[String]()

  //Initial ports with portMap
  def addInPorts(args: Array[String]): Map[String, Int] = {
    val currentSize = inPortMap.size
    for (i <- 0 until args.length) {
      inPortMap += (args(i) -> (i + currentSize))
      inPorts.append(args(i))
    }
    inPortMap
  }

  def addOutPorts(args: Array[String]): Map[String, Int] = {
    val currentSize = outPortMap.size
    for (i <- 0 until args.length) {
      outPortMap += (args(i) -> (i + currentSize))
      outPorts.append(args(i))
    }
    outPortMap
  }

  def getInPorts(): Iterable[String] = {
    inPorts
  }

  def getOutPorts(): Iterable[String] = {
    outPorts
  }

  //We can use ** operator to get a port's ID with its name
  def **(name: String) : Int = {
    if (inPortMap.contains(name)){
      inPortMap(name)
    }else if (outPortMap.contains(name)){
      outPortMap(name)
    }else{
      println("No port exception " + name)
      -1
    }
  }
}