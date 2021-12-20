package tetriski.Purlin.utils

import chisel3.util.log2Ceil

import scala.collection.mutable.ArrayBuffer

class ConfigBundle {
  var y = 0
  var x = 0
  var dIndex = 0
  var channel = 0
  var config = 0

  def this(x: Int, y: Int, dIndex: Int, channel: Int, config: Int) = {
    this()
    this.y = y
    this.x = x
    this.dIndex = dIndex
    this.channel = channel
    this.config = config
  }
}

class MeshSBModel extends MeshModel {

  var Fs = 0
  var configSize = 0
  var maxAdjacency = 0

  var topologyMap = Map[(SBModel, Int), (SBModel, Int)]()
  var topologyDirectionMap = Map[(SBModel, Int), (SBModel, Int)]()
  var routerModelMap = Map[(Int, Int), SBModel]()

  def gotoNextRouter(router: SBModel, dst: (Int, Int)): (SBModel, (Int, Int)) = {
    //    println("!!!", router.x, router.y, dst._1)
    val sink = topologyDirectionMap((router, dst._1))
    val sinkRouter = sink._1
    val sinkPort = sink._2
    val sinkChannel = dst._2
    (sinkRouter, (sinkPort, sinkChannel))
  }

  def clearPath: Unit = {
    routerModelMap.foreach(p => p._2.clearPath)
  }

  def getConfigArray: Array[ConfigBundle] = {
    var ret = new Array[ConfigBundle](0)
    for (y <- 0 until ySize) {
      for (x <- 0 until xSize) {
        val singleConfig = routerModelMap(x, y).getConfigArray
        ret ++= singleConfig
      }
    }
    ret
  }

  def setPath(x: Int, y: Int, src: (Int, Int), dst: (Int, Int)): Unit = {
    val SBModel = routerModelMap((x, y))
    SBModel.setPath(src, dst)
  }

  def this(channelSize: Int, xSize: Int, ySize: Int, Fs: Int) {
    this()
    this.channelSize = channelSize
    this.xSize = xSize
    this.ySize = ySize
    this.Fs = Fs
    init()
  }

  def init(): Unit = {
    for (x <- 0 until xSize) {
      for (y <- 0 until ySize) {
        val routerModel = new SBModel(Fs, x, y, channelSize, xSize, ySize)
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
          //          println(routerModel.x, routerModel.y, direction)
        }
      }
    }
    maxAdjacency = 5

    configSize = routerModelMap.values.toList.map(model => model.configSize).max
  }
}

class SBModel {
  var x = 0
  var y = 0
  var Fs = 0
  //  var Fc = 0
  var adjacency = Array[Int]()
  var channelSize = 0
  var portConnectMap = Map[(Int, Int), Array[(Int, Int)]]()

  def tilePort = -1

  var configSize = 0
  var pathSelectMap = Map[(Int, Int), (Int, Int)]()

  var sinkFromSrc = Map[(Int, Int), Array[(Int, Int)]]()

  def clearPath: Unit = {
    pathSelectMap = Map[(Int, Int), (Int, Int)]()
  }

  def findPath(src: (Int, Int)): Set[(Int, Int)] = {
    val dsts = sinkFromSrc(src)
    dsts.toSet.&~(pathSelectMap.keys.toSet)
  }

  def findPortIndex(direction: Int) = adjacency.indexOf(direction)

  def this(Fs: Int, x: Int, y: Int, channelSize: Int, xSize: Int = Parameters.xSize, ySize: Int = Parameters.ySize) = {
    this()
    this.Fs = Fs
    this.x = x
    this.y = y
    //    this.Fc = Fc

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
    this.channelSize = channelSize

    this.configSize = log2Ceil(Fs)

    for (dst <- adjacency) {
      for (dstC <- 0 until channelSize) {
        var count = 0
        val srcBuffer = new ArrayBuffer[(Int, Int)]()
        for (srcC <- dstC until (channelSize + dstC)) {
          for (src <- adjacency.sortBy(i => Math.abs(3 - i - dst))) {
            if (src != dst) {
              if (count < Fs) {
                srcBuffer.append((src, srcC % channelSize))
                count += 1
              }
            }
          }
        }
        this.portConnectMap += (dst, dstC) -> srcBuffer.toArray
      }
    }

    for (src <- adjacency) {
      for (srcC <- 0 until channelSize) {
        sinkFromSrc += (src, srcC) -> this.portConnectMap.filter(p => p._2.contains((src, srcC))).keys.toArray
      }
    }
  }

  def setPath(src: (Int, Int), dst: (Int, Int)) = {
    var flag = false
    if (portConnectMap.contains(dst)) {
      if (portConnectMap(dst).contains(src)) {
        flag = true
        if (pathSelectMap.contains(dst)) {
          pathSelectMap -= dst
          pathSelectMap += dst -> src
        } else {
          pathSelectMap += dst -> src
        }
      }
    }
    if (!flag) {
      throw new Exception("Invalid path!" + src + " " + dst)
    }

  }

  def getConfigArray: Array[ConfigBundle] = {
    val ret = new ArrayBuffer[ConfigBundle]()

    for (d <- adjacency) {
      for (c <- 0 until channelSize) {
        val dIndex = adjacency.indexOf(d)
        if (pathSelectMap.contains((d, c))) {
          val configBundle = new ConfigBundle(x, y, dIndex, c, portConnectMap((d, c)).indexOf(pathSelectMap((d, c))))
          ret.append(configBundle)
        }
      }
    }
    ret.toArray
  }
}
