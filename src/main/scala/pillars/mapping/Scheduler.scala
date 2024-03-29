package pillars.mapping

import pillars.core._

import scala.collection.mutable.ArrayBuffer
import scala.math.max
import MRRGMode._

/** This object is used to determine when each opNode will fire.
 *
 */
object Scheduler {
  /** Check whether two node in MRRG is connected in mapping result.
   *
   * @param sink the sink node
   * @param from the node we will check
   */
  def checkConnect(sink: NodeMRRG, from: NodeMRRG): Int = {
    if (sink.mapNode.isInstanceOf[OpNode]) return 1
    if (from.mapNode.isInstanceOf[OpNode]) {
      val sinkMap = sink.mapNode.asInstanceOf[ValNode]
      val sourceMap = from.mapNode.asInstanceOf[OpNode]
      if (sinkMap.name.indexOf(sourceMap.name) != -1) return 1
      0
    }
    else {
      val sinkMap = sink.mapNode.asInstanceOf[ValNode]
      val sourceMap = from.mapNode.asInstanceOf[ValNode]
      if (sinkMap.name == sourceMap.name) return 1
      0
    }
  }

  /** Calculate delay of each arcs in DFG.
   *
   * @param node the functionalNode current opNode mapped to
   * @param mrrg the mrrg we used
   */
  def bfs(node: NodeMRRG, mrrg: MRRG): Unit = {
    val cycle = new Array[Int](mrrg.getSize())
    for (i <- 0 until cycle.length)
      cycle(i) = -1
    val queue = new ArrayBuffer[NodeMRRG]()
    val mapNode = node.mapNode.asInstanceOf[OpNode]
    queue.append(node)

    var l, r = 0
    while (l <= r) {
      val node = queue(l)
      for (in <- node.fanIn) {
        if (in.mapNode != null && cycle(mrrg.nodeMap(in.name)) == -1) {
          if (checkConnect(node, in) == 1) {
            if (node.mode ==REG_MODE || in.mode == MEM_MODE) {
              cycle(mrrg.nodeMap(in.name)) = cycle(mrrg.nodeMap(node.name)) + 1
            } else {
              cycle(mrrg.nodeMap(in.name)) = cycle(mrrg.nodeMap(node.name))
            }


            if (in.ops.size != 0) {
              val inputNode = in.mapNode.asInstanceOf[OpNode]
              //              println(mapNode.name + " " + inputNode.name + " " + mapNode.input(0).name + " " +
              //                mapNode.input.size + " " + cycle(mrrg.nodeMap(in.name)))
              val inputLatency = cycle(mrrg.nodeMap(in.name)) + 1
              if (inputNode.name == mapNode.input(0).name) {
                mapNode.inputLatency(0) = inputLatency
              } else {
                mapNode.inputLatency(1) = inputLatency
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

  /** This function is used to do schedule reconstruction,
   * we will calculate skew and fire time(beginCycle) here.
   *
   * @param dfg      the DFG we used
   * @param mrrg     the MRRG we used
   * @param filename the file name for writing the result
   * @param II       the II of mrrg
   */
  def schedule(dfg: DFG, mrrg: MRRG, filename: String = null, II: Int = 0): Unit = {
    val vis = new Array[Set[String]](dfg.getOpSize())
    val queue = scala.collection.mutable.Queue[OpNode]()

    /** Determine delay on each arcs.
     */
    for (node <- mrrg.nodes) {
      if (node.mapNode != null && node.ops.size != 0) {
        if (node.mapNode.isInstanceOf[OpNode]) {
          bfs(node, mrrg)
          val mapnode = node.mapNode.asInstanceOf[OpNode]
          if (mapnode.input.size == 0) {
            queue.enqueue(mapnode)
          }

          vis(dfg.opNodesMap(mapnode.name)) = (0 until mapnode.input.size)
            .map(i => (mapnode.input(i).name)).toSet

          for (i <- 0 until mapnode.input.size)
            if (mapnode.input(i).name == mapnode.name) {
              vis(dfg.opNodesMap(mapnode.name)) -= mapnode.name
            }
        }
      }
    }

    while (!queue.isEmpty) {
      val node = queue.dequeue()
      node.visited = true
      var latency, preLatency, latencyWithoutConst = node.latency
      //      node.primaryInput = false

      /** Update the latency of node based on its input.
       */
      for (i <- 0 until node.input.size) {
        println(node.input(i).name  + " " + node.name + " " + node.inputLatency(i))
        if (node.input(i).name != node.name) {
          latency = max(latency, node.input(i).latency + node.inputLatency(i))
          if (node.input(i).input.size != 0) {
            latencyWithoutConst = max(latencyWithoutConst, node.input(i).latency + node.inputLatency(i))
          }
        } else {
          if (i == 0) {
            node.annulateLatency = node.inputLatency(i)
          } else {
            node.annulateLatency = -node.inputLatency(i)
          }
        }
        //        if (node.input(i).name.indexOf("const") != -1 ||
        //          node.input(i).name.indexOf("input") != -1) {
        //          node.primaryInput = true
        //        }
      }

      if (node.output != null) {
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
          latency = temp
        }
      }

      if (latencyWithoutConst != preLatency) {
        node.setLatency(latencyWithoutConst)
      } else {
        node.setLatency(latency)
      }

      /** Reschedule an input chain with incorrect latency.
       */
      if (preLatency != node.latency && node.latency != 0) {
        for (i <- 0 until node.input.size) {
          val inNode = node.input(i)
          var pushBack = false
          var tempNode = inNode
          if (tempNode.input.size == 1) {
            if (tempNode.name != tempNode.input(0).name) {
              while (tempNode.input.size == 1) {
                tempNode = tempNode.input(0)
              }
            }
          }
          if (tempNode.input.size == 0) {
            pushBack = true
          }
          if (inNode.latency + node.inputLatency(i) != node.latency
            && pushBack) {
            queue.enqueue(inNode)
            val tempQueue = scala.collection.mutable.Queue[OpNode]()
            tempQueue.enqueue(inNode)
            while (!tempQueue.isEmpty) {
              val tempNodeIn = tempQueue.dequeue()
              if (tempNodeIn.output != null) {
                for (tempNodeOut <- tempNodeIn.output.output) {
                  if (tempNodeOut.name != node.name && tempNodeOut.name != tempNodeIn.name) {
                    vis(dfg.opNodesMap(tempNodeOut.name)) += tempNodeIn.name
                    tempQueue.enqueue(tempNodeOut)
                    if (queue.contains(tempNodeOut)) {
                      queue.dequeueAll(n => n.name == tempNodeOut.name)
                    }
                  }
                }
              }
            }
          }
        }
      }

      if (node.output != null) {
        for (out <- node.output.output) {
          if (vis(dfg.opNodesMap(out.name)).contains(node.name)) {
            vis(dfg.opNodesMap(out.name)) -= node.name
            if (vis(dfg.opNodesMap(out.name)).size == 0) {
              queue.enqueue(out)
            }
          }
        }
      }
    }

    /** Calculate skew of each opNodes in DFG.
     */
    for (node <- dfg.opNodes) {
      if (node.input.size == 2) {
        if (node.annulateLatency != 0) {
          if (node.annulateLatency > 0) {
            node.skew = node.annulateLatency - II
          } else {
            node.skew = node.annulateLatency + II
          }
        } else if (!node.primaryInput) {
          val inLatency0 = node.input(0).latency + node.inputLatency(0)
          val inLatency1 = node.input(1).latency + node.inputLatency(1)
          node.skew = inLatency0 - inLatency1
        }
      }
    }

    dfg.updateSchedule(filename + "_r.txt")

  }
}
