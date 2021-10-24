package tetriski.pillars.NoC

import java.io.{File, PrintWriter}

import chisel3.iotesters
import chisel3.util.log2Ceil
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object RouterCompiler extends App {
  val model = new MeshSBModel(2, 4, 4, 8)
  genRandomTask(0.4, model, 10)
  val result = greedyRouting(readJson(), model)
  writeJson(result, "globalRoutingResult.json")

  val network = () => new MeshSwitchBox(model, 16)
  iotesters.Driver.execute(Array("-tgvo", "on", "-tbn", "verilator"), network) {
    c => new RoutingResultTester(c, model, result)
  }

  def greedyRouting(globalRouting: GlobalRouting, model: MeshSBModel): GlobalRouting = {
    model.clearPath
    var sourcePortUsedMap = Map[(Int, Int), Int]()
    var sinkPortUsedMap = Map[(Int, Int), Int]()

    val messageBuffer = new ArrayBuffer[Message]()
    for (message <- globalRouting.messages) {
      val source = (message.srcX, message.srcY)
      val sourceChannel = sourcePortUsedMap.getOrElse(source, 0)
      sourcePortUsedMap += source -> (sourceChannel + 1)
      val start = model.routerModelMap(source)

      val sink = (message.dstX, message.dstY)
      val sinkChannel = sinkPortUsedMap.getOrElse(sink, 0)
      sinkPortUsedMap += sink -> (sinkChannel + 1)
      val end = model.routerModelMap(sink)

      //BFS
      val queue = scala.collection.mutable.Queue[List[RoutingStrategy]]()
      val visited = scala.collection.mutable.Set[(SBModel, (Int, Int))]()
      visited.add(start, (NoCParam.TILE, sourceChannel))

      val initDst = start.findPath((NoCParam.TILE, sourceChannel))
      for (dst <- initDst) {
        val next = model.gotoNextRouter(start, dst)
        if (!visited.contains(next)) {
          val strategy = RoutingStrategy(Option(source._1), Option(source._2), Option(NoCParam.TILE),
            Option(sourceChannel), dst._1, Option(dst._2))
          queue.enqueue(List(strategy))
          visited.add(next)
        }
      }


      var flag = true
      while (!queue.isEmpty && flag) {
        val strategies = queue.dequeue()
        val lastStrategy = strategies.last
        val routerX = lastStrategy.routerX.getOrElse(-1)
        val routerY = lastStrategy.routerY.getOrElse(-1)
        val dstChannel = lastStrategy.dstChannel.getOrElse(-1)
        val dstDirection = lastStrategy.dstDirection
        val sourceRouter = model.routerModelMap(routerX, routerY)

        val current = model.gotoNextRouter(sourceRouter, (dstDirection, dstChannel))
        val currentRouter = current._1
        val currentPort = current._2

        val initDst = currentRouter.findPath(currentPort)
        for (dst <- initDst) {
          if (dst._1 == NoCParam.TILE) {
            if (currentRouter == end && dst._2 == sinkChannel) {
              flag = false
              val strategy = RoutingStrategy(Option(currentRouter.x), Option(currentRouter.y),
                Option(currentPort._1), Option(currentPort._2), dst._1, Option(dst._2))
              val newMessage = Message(message.srcX, message.srcY,
                message.dstX, message.dstY, None, Option(strategies ::: List(strategy)))
              messageBuffer.append(newMessage)
              println("Find path from: (" + source._1
                + ", " + source._2 + ") to (" + sink._1 + ", " + sink._2 + ").")
              for (s <- (strategies ::: List(strategy))) {
                model.setPath(s.routerX.getOrElse(-1), s.routerY.getOrElse(-1),
                  (s.srcDirection.getOrElse(-1), s.srcChannel.getOrElse(-1)),
                  (s.dstDirection, s.dstChannel.getOrElse(-1)))
              }
            }
          } else {
            val next = model.gotoNextRouter(currentRouter, dst)
            if (!visited.contains(next)) {
              val strategy = RoutingStrategy(Option(currentRouter.x), Option(currentRouter.y), Option(currentPort._1),
                Option(currentPort._2), dst._1, Option(dst._2))
              queue.enqueue(strategies ::: List(strategy))
              visited.add(next)
            }
          }
        }
      }

      if (queue.isEmpty) {
        throw new RuntimeException("Cannot find path from: (" + source._1
          + ", " + source._2 + ") to (" + sink._1 + ", " + sink._2 + ").")
      }
    }
    GlobalRouting(messageBuffer.toList)
  }

  def readJson(filename: String = "globalRouting.json") = {
    val in = Source.fromFile(filename).getLines().reduce(_ + _)
    val json = Json.parse(in)
    GlobalRouting.read(json)
  }

  def writeJson(globalRouting: GlobalRouting, filename: String = "globalRouting.json"): Unit = {
    val json = GlobalRouting.write(globalRouting)
    val out = Json.prettyPrint(json)
    val writer = new PrintWriter(new File(filename))
    writer.write(out)
    writer.close()
  }

  def genRandomTask(avgRatio: Double, model: MeshSBModel, valueNum: Int = -1) = {
    val availablePortNum: Int = (model.channelSize * model.xSize * model.ySize * avgRatio).toInt
    val channelSize = model.channelSize
    var srcPortUsedMap = Map[(Int, Int), Int]()
    var dstPortUsedMap = Map[(Int, Int), Int]()

    var messages = List[Message]()

    for (i <- 0 until availablePortNum) {
      var srcX = 0
      var srcY = 0
      var srcPortUsed = channelSize
      while (srcPortUsed >= channelSize) {
        srcX = scala.util.Random.nextInt(model.xSize)
        srcY = scala.util.Random.nextInt(model.ySize)
        srcPortUsed = srcPortUsedMap.getOrElse((srcX, srcY), 0)
      }
      srcPortUsedMap += (srcX, srcY) -> (srcPortUsedMap.getOrElse((srcX, srcY), 0) + 1)

      var dstX = 0
      var dstY = 0
      var dstPortUsed = channelSize
      while (dstPortUsed >= channelSize) {
        dstX = scala.util.Random.nextInt(model.xSize)
        dstY = scala.util.Random.nextInt(model.ySize)
        dstPortUsed = dstPortUsedMap.getOrElse((dstX, dstY), 0)
      }
      dstPortUsedMap += (dstX, dstY) -> (srcPortUsedMap.getOrElse((dstX, dstY), 0) + 1)

      val message = Message(srcX, srcY, dstX, dstY, if (valueNum == -1) {
        None
      } else {
        Option(valueNum)
      }, None)
      messages ::= message
    }

    val globalRouting = GlobalRouting(messages)
    writeJson(globalRouting)
  }


}

case class RoutingStrategy(routerX: Option[Int],
                           routerY: Option[Int],
                           srcDirection: Option[Int],
                           srcChannel: Option[Int],
                           dstDirection: Int,
                           dstChannel: Option[Int])

object RoutingStrategy {
  implicit val routingStrategyFormats = Json.format[RoutingStrategy]

  def write(routingStrategy: RoutingStrategy) = {
    Json.toJson(routingStrategy)
  }

  def read(js: JsValue) = {
    val routerX = (js \ "routerX").asOpt[Int]
    val routerY = (js \ "routerY").asOpt[Int]
    val srcDirection = (js \ "srcDirection").asOpt[Int]
    val srcChannel = (js \ "srcChannel").asOpt[Int]
    val dstDirection = (js \ "dstDirection").as[Int]
    val dstChannel = (js \ "routerY").asOpt[Int]
    RoutingStrategy(routerX, routerY, srcDirection, srcChannel, dstDirection, dstChannel)
  }
}

case class Message(srcX: Int,
                   srcY: Int,
                   dstX: Int,
                   dstY: Int,
                   valueNum: Option[Int],
                   routingStrategy: Option[List[RoutingStrategy]])

object Message {
  implicit val messageFormats = Json.format[Message]

  def write(message: Message) = {
    JsObject(Seq(
      "srcX" -> Json.toJson(message.srcX),
      "srcY" -> Json.toJson(message.srcY),
      "dstX" -> Json.toJson(message.dstX),
      "dstY" -> Json.toJson(message.dstY),
      "valueNum" -> Json.toJson(message.valueNum),
      "routingStrategy" -> Json.toJson(message.routingStrategy)
    ))
  }

  def read(js: JsValue) = {
    val srcX = (js \ "srcX").as[Int]
    val srcY = (js \ "srcY").as[Int]
    val dstX = (js \ "dstX").as[Int]
    val dstY = (js \ "dstY").as[Int]
    val valueNum = (js \ "valueNum").asOpt[Int]
    val routingStrategy = (js \ "routingStrategy").asOpt[List[RoutingStrategy]]
    Message(srcX, srcY, dstX, dstY, valueNum, routingStrategy)
  }
}

case class GlobalRouting(messages: List[Message])

object GlobalRouting {
  def write(globalRouting: GlobalRouting) = {
    JsObject(Seq(
      "messages" -> Json.toJson(globalRouting.messages)
    ))
  }

  def read(js: JsValue) = {
    val messages = (js \ "messages").as[List[Message]]
    GlobalRouting(messages)
  }
}


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

class MeshSBModel {

  var channelSize = 0
  var xSize = 0
  var ySize = 0
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
          val reDirection = NoCParam.reverse(direction)

          val dstRouter = direction match {
            case NoCParam.E => routerModelMap(x + 1, y)
            case NoCParam.W => routerModelMap(x - 1, y)
            case NoCParam.S => routerModelMap(x, y + 1)
            case NoCParam.N => routerModelMap(x, y - 1)
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

  def this(Fs: Int, x: Int, y: Int, channelSize: Int, xSize: Int = NoCParam.xSize, ySize: Int = NoCParam.ySize) = {
    this()
    this.Fs = Fs
    this.x = x
    this.y = y
    //    this.Fc = Fc

    val connectArray = new ArrayBuffer[Int]()
    if (x < xSize - 1) {
      connectArray.append(NoCParam.E)
    }
    if (x > 0) {
      connectArray.append(NoCParam.W)
    }
    if (y < ySize - 1) {
      connectArray.append(NoCParam.S)
    }
    if (y > 0) {
      connectArray.append(NoCParam.N)
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
