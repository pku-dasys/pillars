package tetriski.pillars.Mapping

import java.io.FileWriter

import tetriski.pillars.core._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.max

object Scheduler {
  def checkConnect(sink: NodeMRRG, from: NodeMRRG): Int = {
    if (sink.mapnode.isInstanceOf[OpNode]) return 1
    if (from.mapnode.isInstanceOf[OpNode]) {
      val sinkMap = sink.mapnode.asInstanceOf[ValNode]
      val sourceMap = from.mapnode.asInstanceOf[OpNode]
      if (sinkMap.name.indexOf(sourceMap.name) != -1) return 1
      return 0
    }
    else {
      val sinkMap = sink.mapnode.asInstanceOf[ValNode]
      val sourceMap = from.mapnode.asInstanceOf[ValNode]
      if (sinkMap.name == sourceMap.name) return 1
      return 0
    }
    return 0
  }

  def bfs(node: NodeMRRG, mrrg: MRRG): Unit = {
    val cycle = new Array[Int](mrrg.getSize())
    for (i <- 0 until cycle.length)
      cycle(i) = -1
    val queue = new ArrayBuffer[NodeMRRG]()
    val mapNode = node.mapnode.asInstanceOf[OpNode]
    queue.append(node)

    var l, r = 0
    while (l <= r) {
      val node = queue(l)
      for (in <- node.fanIn) {
        if (in.mapnode != null && cycle(mrrg.nodeMap(in.name)) == -1) {
          if (checkConnect(node, in) == 1) {
            if ((node.name.indexOf("rf0.internalNode") != -1) ||
              node.name.indexOf("global_rf.internalNode") != -1 ||
              (in.name.indexOf("loadStoreUnit.internalNode") != -1)) {
              cycle(mrrg.nodeMap(in.name)) = cycle(mrrg.nodeMap(node.name)) + 1
            } else {
              cycle(mrrg.nodeMap(in.name)) = cycle(mrrg.nodeMap(node.name))
            }


            if (in.ops.size != 0) {
              val inputnode = in.mapnode.asInstanceOf[OpNode]
              println(mapNode.name + " " + inputnode.name + " " + mapNode.input(0).name + " " +
                mapNode.input.size + " " + cycle(mrrg.nodeMap(in.name)))
              if (inputnode.name == mapNode.input(0).name) {
                mapNode.inputLatency(0) = cycle(mrrg.nodeMap(in.name)) + 1
                if(in.name.contains("load")){
                  mapNode.inputLatency(0) += 1
                }
              } else {
                mapNode.inputLatency(1) = cycle(mrrg.nodeMap(in.name)) + 1
                if(in.name.contains("load")){
                  mapNode.inputLatency(1) += 1
                }
              }
            }
            else {
              queue.append(in)
              r += 1
            }
          }
        }
      }
      l += 1
    }
  }

  def schedule(dfg: DFG, mrrg: MRRG, filename: String = null, II: Int = 0): Unit = {
    val vis = new Array[Set[String]](dfg.getOpSize())
    //var stack = new ArrayBuffer[OpNode]()
    var stack = scala.collection.mutable.Stack[OpNode]()
    for (node <- mrrg.nodes) {
      if (node.mapnode != null && node.ops.size != 0) {
        if (node.mapnode.isInstanceOf[OpNode]) {
          bfs(node, mrrg)
          val mapnode = node.mapnode.asInstanceOf[OpNode]
          if (mapnode.input.size == 0) {
            //            stack.enqueue(mapnode)
            stack.push(mapnode)
          }

          vis(dfg.op_nodes_map(mapnode.name)) = (0 until mapnode.input.size)
            .map(i => (mapnode.input(i).name)).toSet

          for (i <- 0 until mapnode.input.size)
            if (mapnode.input(i).name == mapnode.name) {
              vis(dfg.op_nodes_map(mapnode.name)) -= mapnode.name
            }
        }
      }
    }

    while (!stack.isEmpty) {
      val node = stack.pop
      node.visited = true
      //var i, laten = 0
      var laten, preLaten, latenWithoutConst = node.latency
      node.constInput = false

      for (i <- 0 until node.input.size) {
        println(node.name + " " + node.input(i).name + " " + node.input(i).latency + " " + node.inputLatency(i))
        if (node.input(i).name != node.name) {
          laten = max(laten, node.input(i).latency + node.inputLatency(i))
          if (node.input(i).input.size != 0) {
            latenWithoutConst = max(latenWithoutConst, node.input(i).latency + node.inputLatency(i))
          }
        } else {
          if (i == 0) {
            node.annulateLatency = node.inputLatency(i)
          } else {
            node.annulateLatency = -node.inputLatency(i)
          }
        }
        if (node.input(i).name.indexOf("const") != -1) {
          //flag = 1
          node.constInput = true
        }
      }

      if (node.output != null && node.input.size == 0) {
        var temp, tempMin = -100
        for (outNode <- node.output.output) {
          for (i <- 0 until outNode.input.size) {
            val tempNode = outNode.input(i)
            if (tempNode.name == node.name && outNode.visited == true && outNode.name != node.name) {
              temp = max(temp, outNode.latency - outNode.inputLatency(i))
            }
          }
        }
        if (temp > tempMin) {
          laten = temp
        }
      }

      //        node.latency = laten
      if (latenWithoutConst != preLaten) {
        node.setLatency(latenWithoutConst)
      } else {
        node.setLatency(laten)
      }

      //update const
      if (preLaten != node.latency && node.latency != 0) {
        for (i <- 0 until node.input.size) {
          val inNode = node.input(i)
          if (inNode.latency + node.inputLatency(i) != node.latency
            && inNode.name != node.name
            && (inNode.input.size == 0)) {
            stack.push(inNode)
            for (inNodeOut <- inNode.output.output) {
              if (inNodeOut.name != node.name) {
                vis(dfg.op_nodes_map(inNodeOut.name)) += inNode.name
              }
            }
          }
        }
      }

      if (node.input.size == 2) {
        if (node.annulateLatency != 0) {
          if (node.annulateLatency > 0) {
            node.skew = node.annulateLatency - II
          } else {
            node.skew = node.annulateLatency + II
          }
        } else if (!node.constInput) {
          node.skew = node.input(0).latency + node.inputLatency(0) - node.input(1).latency - node.inputLatency(1)
        }
      }
      if (node.output != null) {
        for (out <- node.output.output) {
          if (vis(dfg.op_nodes_map(out.name)).contains(node.name)) {
            vis(dfg.op_nodes_map(out.name)) -= node.name
            if (vis(dfg.op_nodes_map(out.name)).size == 0) {
              stack.push(out)
              //            r += 1
            }
          }
        }
      }
      //      l += 1
    }


    val unscheduledArray = Source.fromFile(filename + "_r.txt").getLines().toArray
    val scheduleFile = new FileWriter(filename + "_r.txt")
    var i = 0
    var beginCycle = 0
    val pattern = "[0-9]+:".r

    val minLatency = dfg.op_nodes.map(op => op.latency).min
    dfg.op_nodes.foreach(op => op.setLatency(op.latency - minLatency))

    for (j <- 0 until dfg.op_nodes.size) {
      val op = dfg.op_nodes(j)
      val tempResult = unscheduledArray(j).split(" ").toList
      val mrrgName = tempResult(1)
      val tempStr = (pattern findFirstIn mrrgName).toArray
      val ii = tempStr(0).replace(":", "").toInt
      if (op.latency == 0 && op.annulateLatency == 0) {
        beginCycle = ii
      }
    }

    for (op <- dfg.op_nodes) {
      var outLatency = op.latency + beginCycle
      if (op.annulateLatency != 0 && op.constInput) {
        outLatency = outLatency + II
      }
      println(op.name + " " + outLatency + " " + op.skew)
      scheduleFile.write(unscheduledArray(i) + " " + outLatency + " " + op.skew + "\n")
      i = i + 1
    }
    scheduleFile.flush()
    scheduleFile.close()
  }
}
