package tetriski.Purlin.utils

import scala.collection.mutable.ArrayBuffer

class MeshNoCModel extends MeshModel {

  var maxAdjacency = 0
  var congestionLimit = 0.0

  var topologyMap = Map[(NoCRouterModel, Int), (NoCRouterModel, Int)]()
  var topologyDirectionMap = Map[(NoCRouterModel, Int), (NoCRouterModel, Int)]()
  var routerModelMap = Map[(Int, Int), NoCRouterModel]()

  var allocatedMessages = Map[Int, Message]()

  def estimateAll(): Double = {
    var latency = 0.0
    for (item <- allocatedMessages) {
      val messageIndex = item._1
      val message = item._2
      latency += estimate(message, messageIndex, true)
    }
    latency / allocatedMessages.size.toDouble
  }

  def estimate(message: Message, messageIndex: Int = -1, allocated: Boolean = false,
               inputStrategies: List[RoutingStrategy] = List()): Double = {
    val begin = message.injectionCycle.getOrElse(0)
    val packetLength = message.packetLength.getOrElse(0)
    val end = packetLength + begin
    val strategies = if (inputStrategies.size > 0) {
      inputStrategies
    } else {
      message.routingStrategy.getOrElse(List())
    }
    val routingHops = strategies.length

    var congestionOverhead = 0.0

    for (i <- 0 until routingHops) {
      val strategy = strategies(i)
      val x = strategy.routerX.getOrElse(-1)
      val y = strategy.routerY.getOrElse(-1)
      val direction = strategy.dstDirection

      var congestionLevel = getCongestionLevel(x, y, direction, message)
      if (allocated) {
        congestionLevel -= 1
      }

      if (congestionLevel > 0) {
        val lBegin = begin + i
        val lEnd = end + i
        val router = routerModelMap((x, y))
        val congestionIndexes = router.routingInfo.filter(item => item._2 != messageIndex).map(item => item._3)
        var overlapNum = new Array[Int](packetLength)
        for (index <- congestionIndexes) {
          val congestionMessage = allocatedMessages(index)
          val ss = congestionMessage.routingStrategy.getOrElse(List())
          for (j <- 0 until ss.size) {
            val s = ss(j)
            if (s.routerX.getOrElse(-1) == x && s.routerY.getOrElse(-1) == y && s.dstDirection == direction) {
              val cBegin = congestionMessage.injectionCycle.getOrElse(0) + j
              val cEnd = cBegin + congestionMessage.packetLength.getOrElse(0)
              val overlap = ((lBegin until lEnd).toSet & (cBegin until cEnd).toSet)
              for(index <- overlap){
                overlapNum(index - lBegin) += 1
              }
            }
          }
        }
        val overhead = overlapNum.map(i =>
          if(i >= channelSize){
          i - channelSize + 1
        }else{
          0
        }).sum

//        val overhead = overlapNum.sum
        congestionOverhead += overhead.toDouble *  Parameters.overlapPunishFactor
      }

    }

    congestionOverhead + routingHops.toDouble
  }

  def allocateMessage(message: Message, messageIndex: Int): Unit = {
    allocatedMessages += messageIndex -> message
    val injectionCycle = message.injectionCycle.getOrElse(0)
    val packetLength = message.packetLength.getOrElse(0)
    val strategies = message.routingStrategy.getOrElse(List())
    for (s <- strategies) {
      setPath(s.routerX.getOrElse(-1), s.routerY.getOrElse(-1),
        s.srcDirection.getOrElse(-1), s.dstDirection, messageIndex, injectionCycle, packetLength)
    }
  }

  def ripUP(message: Message, messageIndex: Int): Unit = {
    allocatedMessages -= messageIndex
    val injectionCycle = message.injectionCycle.getOrElse(0)
    val packetLength = message.packetLength.getOrElse(0)
    val strategies = message.routingStrategy.getOrElse(List())
    for (s <- strategies) {
      ripUp(s.routerX.getOrElse(-1), s.routerY.getOrElse(-1),
        s.srcDirection.getOrElse(-1), s.dstDirection, messageIndex, injectionCycle, packetLength)
    }
  }

  def getCongestionLevel(x: Int, y: Int, direction: Int, message: Message): Int = {
    val begin = message.injectionCycle.getOrElse(0)
    val packetLength = message.packetLength.getOrElse(0)
    val end = packetLength + begin
    val congestionLevel = (begin until end).map(c => getPresentlyUsed(x, y, direction, c)).reduce(_ + _) / packetLength
    if (congestionLevel >= channelSize) {
      congestionLevel - channelSize + 1
    } else {
      0
    }
  }

  def getPresentlyUsed(x: Int, y: Int, direction: Int, injectionCycle: Int): Int = {
    val router = routerModelMap((x, y))
    val congestionMap = router.directionCongestionMap(direction)
    val congestion = if(congestionMap.contains(injectionCycle)) {
      congestionMap(injectionCycle)
    }else {
      0.0
    }
    Math.round(congestion * channelSize).toInt
  }

  def gotoNextRouter(router: NoCRouterModel, dst: Int): (NoCRouterModel, Int) = {
    val sink = topologyDirectionMap((router, dst))
    val sinkRouter = sink._1
    val sinkPort = sink._2
    (sinkRouter, sinkPort)
  }

  def clear: Unit = {
    routerModelMap.foreach(p => p._2.clear)
  }


  def setPath(x: Int, y: Int, src: Int, dst: Int, msgIndex: Int, injectionCycle: Int, packetLength: Int): Unit = {
    val NoCRouterModel = routerModelMap((x, y))
    NoCRouterModel.setPath(src, dst, msgIndex, injectionCycle, packetLength)
  }

  def ripUp(x: Int, y: Int, src: Int, dst: Int, msgIndex: Int, injectionCycle: Int, packetLength: Int): Unit = {
    val NoCRouterModel = routerModelMap((x, y))
    NoCRouterModel.ripUp(src, dst, msgIndex, injectionCycle, packetLength)
  }

  def this(channelSize: Int, xSize: Int, ySize: Int, congestionLimit: Double) {
    this()
    this.channelSize = channelSize
    this.xSize = xSize
    this.ySize = ySize
    this.congestionLimit = congestionLimit
    init()
  }

  def init(): Unit = {
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        val routerModel = new NoCRouterModel(x, y, channelSize, xSize, ySize, congestionLimit)
        routerModelMap += (x, y) -> routerModel
      }
    }
    //mesh
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        val routerModel = routerModelMap(x, y)
        val adjacency = routerModel.adjacency
        for (i <- 0 until adjacency.size - 1) {
          val direction = adjacency(i)
          val reDirection = Parameters.reverse(direction)

          val dstRouter = direction match {
            case Parameters.E => routerModelMap(x + 1, y)
            case Parameters.W => routerModelMap(x - 1, y)
            case Parameters.S => routerModelMap(x, y + 1)
            case Parameters.N => routerModelMap(x, y - 1)
          }

          topologyDirectionMap += (routerModel, direction) -> (dstRouter, reDirection)
          val dstIndex = dstRouter.adjacency.indexOf(reDirection)
          topologyMap += (routerModel, i) -> (dstRouter, dstIndex)
        }
      }
    }
    maxAdjacency = 5

  }
}

class NoCRouterModel {
  var x = 0
  var y = 0
  var adjacency = Array[Int]()
  var channelSize = 0
  var congestionLimit = 0.0

  var deadlockPrevented = false

  def tilePort = -1

  var directionCongestionMap = scala.collection.mutable.Map[Int, scala.collection.mutable.Map[Int, Double]]()
  val routingInfo = new ArrayBuffer[(Int, Int, Int)]()


  def clear: Unit = {
    directionCongestionMap = scala.collection.mutable.Map[Int, scala.collection.mutable.Map[Int, Double]]()
    for (direction <- adjacency) {
      directionCongestionMap.put(direction, scala.collection.mutable.Map[Int, Double]())
    }
    routingInfo.clear()
  }

  def findUnimpededDirection(srcDirection: Int, injectionCycle: Int, congestionRate: Double = congestionLimit):
  scala.collection.mutable.Map[Int, scala.collection.mutable.Map[Int, Double]] = {
    var ret = directionCongestionMap.filter(i => i._1 != srcDirection)
      .filter(i => {if (i._2.contains(injectionCycle)) {
        i._2(injectionCycle)
      } else {
        0.0
      }} < congestionRate)
    if(deadlockPrevented){
      //odd-even model
      if(y % 2 == 1){
        if(srcDirection == Parameters.S || srcDirection == Parameters.N){
          ret = ret.filter(item => item._1 != Parameters.W)
        }
      }else{
        if(srcDirection == Parameters.E){
          ret = ret.filter(item => item._1 == Parameters.E)
        }
      }
    }
    ret
  }


  def findPortIndex(direction: Int) = adjacency.indexOf(direction)

  def this(x: Int, y: Int, channelSize: Int, xSize: Int = Parameters.xSize,
           ySize: Int = Parameters.ySize, congestionLimit: Double, deadlockPrevented: Boolean = false) = {
    this()
    this.x = x
    this.y = y
    this.congestionLimit = congestionLimit
    this.deadlockPrevented = deadlockPrevented

    val connectArray = new ArrayBuffer[Int]()
    if (x < xSize - 1) {
      connectArray.append(Parameters.E)
    }
    if (x > 0) {
      connectArray.append(Parameters.W)
    }
    if (y < ySize - 1) {
      connectArray.append(Parameters.S)
    }
    if (y > 0) {
      connectArray.append(Parameters.N)
    }

    this.adjacency = connectArray.toArray ++ Array(tilePort)

    for (direction <- adjacency) {
      directionCongestionMap.put(direction, scala.collection.mutable.Map[Int, Double]())
    }

    this.channelSize = channelSize
  }

  def setPath(srcDirection: Int, dstDirection: Int, msgIndex: Int, injectionCycle: Int, packetLength: Int) = {
    for (c <- injectionCycle until injectionCycle + packetLength) {
      if (directionCongestionMap(dstDirection).contains(c)) {
        directionCongestionMap(dstDirection)(c) = 1.0 / channelSize + directionCongestionMap(dstDirection)(c)
        if (directionCongestionMap(dstDirection)(c) > congestionLimit) {
          throw new Exception("Exceed congestion limit!" + srcDirection + " " + dstDirection + " " + msgIndex)
        }
      } else {
        directionCongestionMap(dstDirection).put(c, 1.0 / channelSize)
      }
    }
    routingInfo.append((srcDirection, dstDirection, msgIndex))

  }

  def ripUp(srcDirection: Int, dstDirection: Int, msgIndex: Int, injectionCycle: Int, packetLength: Int): Unit = {
    if (routingInfo.contains((srcDirection, dstDirection, msgIndex))) {
      for (c <- injectionCycle until injectionCycle + packetLength) {
        directionCongestionMap(dstDirection)(c) = -1.0 / channelSize + directionCongestionMap(dstDirection)(c)
      }
      routingInfo.remove(routingInfo.indexOf((srcDirection, dstDirection, msgIndex)))
    } else {
      throw new Exception("Invalid path!" + srcDirection + " " + dstDirection + " " + msgIndex)
    }
  }

}
