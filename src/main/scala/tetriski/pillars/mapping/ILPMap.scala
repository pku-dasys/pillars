package tetriski.pillars.mapping

import java.io.FileWriter
import java.util.ArrayList

import tetriski.pillars.core.{MRRG, MRRGMode, NodeMRRG, OpcodeTranslator}
import tetriski.pillars.hardware.PillarsConfig._

import scala.collection.JavaConverters
import scala.collection.mutable.ArrayBuffer

/** This object is used to do mapping calling gurobimap_java.
 */
object ILPMap {
  /** Map given DFG(IR) to given MRRG,
   * and write result to file which name is related to filename using FileWriter fw.
   *
   * @example If the file name is "dir/test", this mapper will produce "dir/test_i.txt" (Information TXT)
   *          and "dir/test_r.txt" (Result TXT) when mapping is successful.
   * @param dfg             the given DFG
   * @param mrrg            the given MRRG
   * @param filename        the name we will used to write result
   * @param fw              the FileWriter we used
   * @param separatedPR     a parameter indicating whether ILP placement and routing should be separated
   * @param scheduleControl a parameter indicating whether the latency and skew should be controlled and obtained in ILP
   * @param skewLimit       the limit of skew which only is used when latencyControl is ture
   * @param latencyLimit    the limit of latency which only is used when latencyControl is ture
   * @return the run time of mapper
   */
  def mapping(dfg: DFG, mrrg: MRRG, filename: String = null, fw: FileWriter = null,
              separatedPR: Boolean = false, scheduleControl: Boolean = false,
              skewLimit: Int = 2, latencyLimit: Int = 32): Double = {
    val mapper = new gurobiMapJava(filename)

    mapper.II = dfg.II
    mapper.useRelativeSkew = USE_RELATIVE_SKEW

//        val seed = System.currentTimeMillis()
//    println(seed)
//        mapper.RNG.setSeed(seed)
    //    mapper.RNG.setSeed(GlobalMappingResult.usedFuncALUs.size)

    val neighboringDistance = 20
    mapper.neighboringDistance = neighboringDistance
    mrrg.shortestPath(neighboringDistance).foreach(t =>
      mapper.MRRGDistance.put(JavaConverters.seqAsJavaList(List(t._1._1, t._1._2)), t._2))
    mrrg.neighboringNodeMap.map(pair => mapper.MRRGNeighboringNode
      .put(pair._1, JavaConverters.setAsJavaSet(pair._2)))

    val num_dfg_op = dfg.getOpSize()
    val num_dfg_val = dfg.getValSize()

    for (i <- 0 until num_dfg_op) {
      val dfgNode = dfg.opNodes(i)
      val inputSize = dfgNode.input.size
      if (inputSize > 1) {
        val inputs = new ArrayList[String]()
        for (inputOperand <- 0 until inputSize) {
          inputs.add(dfgNode.input(inputOperand).name)
          if (dfgNode.input(inputOperand).name == dfgNode.name) {
            mapper.DFGSelfJoinMap.put(dfgNode.name, inputOperand)
          }
        }
        mapper.DFGMultipleInputMap.put(dfgNode.name, inputs)
      }

      mapper.DFGOpNodeName.add(dfgNode.name)
      mapper.DFGOpNodeOpcode.add(Integer.valueOf(dfgNode.opcode.id))
      if (OpcodeTranslator.commutativeSet.contains(dfgNode.opcode)) {
        mapper.DFGCommutativeSet.add(i)
      }

      if (dfgNode.output != null) {
        mapper.DFGOpNodeOut.add(Integer.valueOf(dfg.valNodesMap(dfgNode.output.name)))
        mapper.DFGValB2opMap.put(dfgNode.name + "OUT", i)
      } else {
        mapper.DFGOpNodeOut.add(Integer.valueOf(-1))
      }
    }
    for (i <- 0 until num_dfg_val) {
      mapper.DFGValNodeName.add(dfg.valNodes(i).name)

      for (outNode <- dfg.valNodes(i).output) {
        val connect = new ArrayList[String]()
        connect.add(dfg.opNodes(mapper.DFGValB2opMap.get(dfg.valNodes(i).name)).name)
        connect.add(outNode.name)
        mapper.connectList.add(connect)
      }

      val outputSize = dfg.valNodes(i).output.size
      val out = new ArrayList[Integer]()
      val operand = new ArrayList[Integer]()
      for (j <- 0 until outputSize) {
        out.add(Integer.valueOf(dfg.opNodesMap(dfg.valNodes(i).output(j).name)))
        operand.add(Integer.valueOf(dfg.valNodes(i).outputOperand(j)))
      }
      mapper.DFGValNodeOut.add(out)
      mapper.DFGValNodeOutputOperand.add(operand)
    }

    var num_mrrg_r = 0
    var num_mrrg_f = 0
    val num_mrrg = mrrg.getSize()
    val routingNodes = ArrayBuffer[NodeMRRG]()
    val functionNodes = ArrayBuffer[NodeMRRG]()
    var nodeIDMap = Map[NodeMRRG, Int]()

    for (i <- 0 until num_mrrg) {

      val node = mrrg.nodes(i)
      val mode = node.mode
      if (mode == MRRGMode.MEM_MODE) {
        mapper.MRRGLatency.put(node.name, 1)
      } else if (mode == MRRGMode.REG_MODE) {
        mapper.MRRGLatency.put(node.name, 1)
        //        for (fanIn <- node.fanIn) {
        //          mapper.MRRGLatency.put(fanIn.name, 1)
        //        }
      }

      if (mrrg.nodes(i).ops.size != 0) {
        functionNodes.append(mrrg.nodes(i))
        nodeIDMap = nodeIDMap + (mrrg.nodes(i) -> num_mrrg_f)
        var j = 0
        val opSize = mrrg.nodes(i).ops.size
        val opcodes = new ArrayList[Integer]()
        for (j <- 0 until opSize) {
          opcodes.add(Integer.valueOf(mrrg.nodes(i).ops(j).id))
        }
        mapper.MRRGFunctionSupportOpcode.add(opcodes)
        num_mrrg_f += 1
      }
      else {
        routingNodes.append(mrrg.nodes(i))
        nodeIDMap = nodeIDMap + (mrrg.nodes(i) -> num_mrrg_r)
        num_mrrg_r += 1
      }
    }

    for (i <- 0 until num_mrrg_f) {
      mapper.MRRGFunctionName.add(functionNodes(i).name)
      val faninsize = functionNodes(i).fanIn.size
      val fanin = new ArrayList[Integer]()
      val fanintype = new ArrayList[Integer]()
      for (j <- 0 until faninsize) {
        fanin.add(Integer.valueOf(nodeIDMap(functionNodes(i).fanIn(j))))
        if (functionNodes(i).fanIn(j).ops.size == 0) {
          fanintype.add(Integer.valueOf(0))
        } else {
          fanintype.add(Integer.valueOf(1))
        }
      }
      mapper.MRRGFunctionFanin.add(fanin)
      mapper.MRRGFunctionFaninType.add(fanintype)

      val fanoutsize = functionNodes(i).fanOut.size
      val fanout = new ArrayList[Integer]()
      val fanouttype = new ArrayList[Integer]()
      for (j <- 0 until fanoutsize) {
        fanout.add(Integer.valueOf(nodeIDMap(functionNodes(i).fanOut(j))))
        if (functionNodes(i).fanOut(j).ops.size == 0) {
          fanouttype.add(Integer.valueOf(0))
        } else {
          fanouttype.add(Integer.valueOf(1))
        }
      }
      mapper.MRRGFunctionFanout.add(fanout)
      mapper.MRRGFunctionFanoutType.add(fanouttype)
    }

    for (i <- 0 until num_mrrg_r) {
      mapper.MRRGRoutingName.add(routingNodes(i).name)
      val fanInSize = routingNodes(i).fanIn.size
      val fanIn = new ArrayList[Integer]()
      val fanInType = new ArrayList[Integer]()
      for (j <- 0 until fanInSize) {
        fanIn.add(Integer.valueOf(nodeIDMap(routingNodes(i).fanIn(j))))
        if (routingNodes(i).fanIn(j).ops.size == 0) {
          fanInType.add(Integer.valueOf(0))
        } else {
          fanInType.add(Integer.valueOf(1))
        }
      }
      mapper.MRRGRoutingFanin.add(fanIn)
      mapper.MRRGRoutingFaninType.add(fanInType)

      val node = routingNodes(i)
      val fanOutSize = node.fanOut.size
      val fanOut = new ArrayList[Integer]()
      val fanOutType = new ArrayList[Integer]()
      for (j <- 0 until fanOutSize) {
        fanOut.add(Integer.valueOf(nodeIDMap(node.fanOut(j))))
        if (node.fanOut(j).ops.size == 0) {
          fanOutType.add(Integer.valueOf(0))
        } else {
          fanOutType.add(Integer.valueOf(1))
        }
      }
      mapper.MRRGRoutingFanout.add(fanOut)
      mapper.MRRGRoutingFanoutType.add(fanOutType)
    }

    if (fw != null) {
      val result = mapper.ILPMap(fw)
      println("Elapsed time:" + mapper.elapsedTime + "ms")
      GlobalMappingResult.addResult(mapper.result, mapper.elapsedTime, mapper.usedBypassALU, mapper.usedFuncALU)
      return result
    } else {
      if (scheduleControl) {
        mapper.skewLimit = skewLimit
        mapper.maxLatency = latencyLimit
      }
      val result = mapper.ILPMap(separatedPR, scheduleControl)
      println("Elapsed time:" + mapper.elapsedTime + "ms")
      GlobalMappingResult.addResult(mapper.result, mapper.elapsedTime, mapper.usedBypassALU, mapper.usedFuncALU)

      if (mapper.result.contains("success")) {
        val routingResult = result(0)
        for (i <- 0 until num_mrrg_r) {
          if (routingResult.get(i).intValue != -1) {
            routingNodes(i).mapNode = dfg.valNodes(routingResult.get(i).intValue())
            //println(routingNodes(i).name)
          }
        }
        val functionResult = result(1)
        for (i <- 0 until num_mrrg_f) {
          if (functionResult.get(i).intValue != -1) {
            functionNodes(i).mapNode = dfg.opNodes(functionResult.get(i).intValue())
            //println(functionNodes(i).name)
          }
        }

        if (scheduleControl && mapper.ringCheckPass) {
          var skewMap = mapper.DFGRelativeSkewMap
          for (node <- dfg.opNodes) {
            if (mapper.DFGMultipleInputMap.containsKey(node.name)) {
              if (mapper.DFGCommutatedSet.contains(node.name)) {
                node.commutated = true
                skewMap.replace(node.name, -skewMap.get(node.name))
              }
            }
          }
          if (!USE_RELATIVE_SKEW) {
            skewMap.clear()
            for (node <- dfg.opNodes) {
              if (mapper.DFGMultipleInputMap.containsKey(node.name)) {
                val sinkName = node.name
                val inputs = mapper.DFGMultipleInputMap.get(node.name)
                val sourceName0 = inputs.get(0)
                val sourceName1 = inputs.get(1)
                val name0 = "WaitSkew_" + sourceName0 + "_" + sinkName
                val name1 = "WaitSkew_" + sourceName1 + "_" + sinkName
                val waitSkew0 = mapper.waitSkewMap.get(name0)
                val waitSkew1 = mapper.waitSkewMap.get(name1)
                var skew = (waitSkew1 << LOG_SKEW_LENGTH) + waitSkew0
                if (mapper.DFGCommutatedSet.contains(node.name)) {
                  skew = (waitSkew0 << LOG_SKEW_LENGTH) + waitSkew1
                }
                skewMap.put(node.name, skew)
              }
            }
          }
          dfg.updateSchedule(filename + "_r.txt", mapper.DFGLatencyMap, skewMap, filename + "_r.txt")
        }
        else {
          Scheduler.schedule(dfg, mrrg, filename = filename, II = dfg.II)
        }
        dfg.func2regMap = JavaConverters.mapAsScalaMap(mapper.func2regMap)
        dfg.funcDirect2funcMap = JavaConverters.mapAsScalaMap(mapper.funcDirect2funcMap)
        dfg.reg2funcMap = JavaConverters.mapAsScalaMap(mapper.reg2funcMap)
        dfg.regConnect = JavaConverters.mapAsScalaMap(mapper.regConnect)
        dfg.synthesizable = true
        dfg.regNum = mapper.regMap.size()
      }
      return mapper.elapsedTime
    }
    -1
  }

}
