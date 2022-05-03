package tetriski.pillars.mapping

import java.util.List
import java.io.FileWriter
import scala.collection.JavaConverters.{bufferAsJavaList, mapAsJavaMap}
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import org.apache.logging.log4j.LogManager

import tetriski.pillars.core.{MRRG, MRRGMode, NodeMRRG, OpcodeTranslator}
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.hardware.PillarsConfig

/** This object solves CGRA mapping using an optimization-modulo-theories (OMT) engine.
 */
object OmtMap {
  val logger = LogManager.getLogger(OmtMap.getClass().getName())

  /** Map the given DFG (IR) to the given MRRG,
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
  def mapping(
    dfg: DFG,
    mrrg: MRRG,
    filename: String = null,
    fw: FileWriter = null,
    separatedPR: Boolean = false,
    scheduleControl: Boolean = false,
    skewLimit: Int = 2,
    latencyLimit: Int = 32
  ): Double = {
    val mapper = new Z3ProverMap()
    mapper.filename = filename
    mapper.ii = dfg.II
    mapper.useRelativeSkew = PillarsConfig.USE_RELATIVE_SKEW

    for (i <- 0 until dfg.getOpSize()) { // collect DFG op nodes
      val dfgNode = dfg.opNodes(i)
      val inputSize = dfgNode.input.size
      if (inputSize > 1) {
        val inputS = new ArrayBuffer[String]()
        val inputI = new ArrayBuffer[Int]()
        for (inputOperand <- 0 until inputSize) {
          val inputName = dfgNode.input(inputOperand).name
          inputS.append(inputName)
          inputI.append(dfg.valNodesMap(dfgNode.input(inputOperand).output.name))
          if (dfgNode.input(inputOperand).name == dfgNode.name) {
            mapper.dfgSelfJoinMap.put(dfgNode.name, inputOperand)
          }
        }
        mapper.dfgOpOperand.put(i, inputI)
        mapper.dfgMultiInputMap.put(dfgNode.name, inputS)
      }

      mapper.dfgOpNodeName.append(dfgNode.name)
      mapper.dfgOpNodeOpcode.append(dfgNode.opcode.id)
      if (OpcodeTranslator.commutativeSet.contains(dfgNode.opcode)) {
        mapper.dfgCommutativeSet.add(i)
      }

      if (dfgNode.output != null) {
        mapper.dfgOpNodeOut.append(dfg.valNodesMap(dfgNode.output.name))
        mapper.dfgValB2opMap.put(dfgNode.name + "OUT", i)
      } else {
        mapper.dfgOpNodeOut.append(-1)
      }
    }

    for (i <- 0 until dfg.getValSize()) { // collect DFG value nodes
      mapper.dfgValNodeName.append(dfg.valNodes(i).name)

      for (outNode <- dfg.valNodes(i).output) {
        val connect = ArrayBuffer[String]()
        connect.append(dfg.opNodes(mapper.dfgValB2opMap(dfg.valNodes(i).name)).name)
        connect.append(outNode.name)
        mapper.connectList.append(connect)
      }

      val outputSize = dfg.valNodes(i).output.size
      val out = ArrayBuffer[Int]()
      val operand = ArrayBuffer[Int]()
      for (j <- 0 until outputSize) {
        out.append(dfg.opNodesMap(dfg.valNodes(i).output(j).name))
        operand.append(dfg.valNodes(i).outputOperand(j))
      }
      mapper.dfgValNodeOut.append(out)
      mapper.dfgValNodeOutputOperand.append(operand)
    }

    val (funcNodes, routNodes) = { // collect MRRG nodes
      val funcNodes = ArrayBuffer[NodeMRRG]()
      val routNodes = ArrayBuffer[NodeMRRG]()

      for (i <- 0 until mrrg.nodes.size) {
        val node = mrrg.nodes(i)
        if (node.ops.size != 0) {
          funcNodes.append(node)
        } else {
          routNodes.append(node)
        }
      }
      (funcNodes, routNodes)
    }

    val nodeIdMap = { // assign ids to MRRG funcNodes and routNodes
      val nodeIdMap = Map[NodeMRRG, Int]()
      for ((funcNode, id) <- funcNodes.zipWithIndex) {
        nodeIdMap.put(funcNode, id)
      }
      for ((routNode, id) <- routNodes.zipWithIndex) {
        nodeIdMap.put(routNode, id)
      }
      nodeIdMap
    }

    val sramMap = {
      val sramMap = Map[Int, ArrayBuffer[Int]]()
      for ((funcNode, funcNodeId) <- funcNodes.zipWithIndex) {
        val sramId = funcNode.sramID
        if (sramId != -1) {
          if (!sramMap.contains(sramId)){
            sramMap.put(sramId, ArrayBuffer[Int]())
          }
          sramMap(sramId).append(funcNodeId)
        }
      }
      sramMap
    }

    // collect fixed map relation
    for ((opNode, sramId) <- dfg.fixedMapSRAM) {
      val opNodeIndex = dfg.opNodes.indexOf(opNode)
      mapper.fixedMapRelation.put(opNodeIndex, Set(sramMap(sramId): _*))
    }

    def nodeType(node: NodeMRRG): Int = {
      if (node.ops.size == 0) mapper.MRRG_ROUT_NODE_TYPE
      else mapper.MRRG_FUNC_NODE_TYPE
    }

    // collect attribues and edges of funcNodes
    for (funcNode <- funcNodes) {
      // collect attributes
      mapper.mrrgFuncName.append(funcNode.name)
      funcNode.mode match {
        case MRRGMode.MEM_MODE | MRRGMode.REG_MODE => mapper.mrrgLatency.put(funcNode.name, 1)
        case _ => {}
      }
      mapper.mrrgFuncOpcodes.append(funcNode.ops.map(_.id))

      // collect in-edges
      val faninId = new ArrayBuffer[Int]()
      val faninType = new ArrayBuffer[Int]()
      for (faninNode <- funcNode.fanIn) {
        faninId.append(nodeIdMap(faninNode))
        faninType.append(nodeType(faninNode))
      }
      mapper.mrrgFuncFaninId.append(faninId)
      mapper.mrrgFuncFaninType.append(faninType)

      // collect out-edges
      val fanoutId = new ArrayBuffer[Int]()
      val fanoutType = new ArrayBuffer[Int]()
      for (fanoutNode <- funcNode.fanOut) {
        fanoutId.append(nodeIdMap(fanoutNode))
        fanoutType.append(nodeType(fanoutNode))
      }
      mapper.mrrgFuncFanoutId.append(fanoutId)
      mapper.mrrgFuncFanoutType.append(fanoutType)
    }

    // collect attribues and edges of routNodes
    for (routNode <- routNodes) {
      // collect attributes
      mapper.mrrgRoutName.append(routNode.name)

      // collect in-edges
      val faninId = ArrayBuffer[Int]()
      val faninType = ArrayBuffer[Int]()
      for (faninNode <- routNode.fanIn) {
        faninId.append(nodeIdMap(faninNode))
        faninType.append(nodeType(faninNode))
      }
      mapper.mrrgRoutFaninId.append(faninId)
      mapper.mrrgRoutFaninType.append(faninType)

      // collect out-edges
      val fanoutId = ArrayBuffer[Int]()
      val fanoutType = ArrayBuffer[Int]()
      for (fanoutNode <- routNode.fanOut) {
        fanoutId.append(nodeIdMap(fanoutNode))
        fanoutType.append(nodeType(fanoutNode))
      }
      mapper.mrrgRoutFanoutId.append(fanoutId)
      mapper.mrrgRoutFanoutType.append(fanoutType)
    }

    val elapsedTime = if (fw != null) {
      val result = mapper.omtMap(fw)
      logger.info(s"Elapsed time: ${mapper.elapsedTime}ms")
      GlobalMappingResult.addResult(mapper.result, mapper.elapsedTime, mapper.usedBypassAlu, mapper.usedFuncAlu)
      result

    } else {
      if (scheduleControl) {
        mapper.skewLimit = skewLimit
        mapper.maxLatency = latencyLimit
      }
      val result = mapper.omtMap(separatedPR, scheduleControl)
      logger.info(s"Elapsed time: ${mapper.elapsedTime}ms")
      GlobalMappingResult.addResult(mapper.result, mapper.elapsedTime, mapper.usedBypassAlu, mapper.usedFuncAlu)

      if (mapper.result.contains("success")) {
        val routResult = result(0)
        for ((routNode, routNodeId) <- routNodes.zipWithIndex) {
          val valNodeId = routResult(routNodeId)
          if (valNodeId != -1) {
            routNode.mapNode = dfg.valNodes(valNodeId)
            logger.trace(s"MRRG routNode ${routNode.name} mapped")
          }
        }
        val funcResult = result(1)
        for ((funcNode, funcNodeId) <- funcNodes.zipWithIndex) {
          val opNodeId = funcResult(funcNodeId)
          if (opNodeId != -1) {
            funcNode.mapNode = dfg.opNodes(opNodeId)
            logger.trace(s"MRRG funcNode ${funcNode.name} mapped")
          }
        }

        if (scheduleControl && mapper.ringCheckPass) {
          val skewMap = mapper.dfgRelativeSkewMap
          for (node <- dfg.opNodes) {
            if (mapper.dfgMultiInputMap.contains(node.name)) {
              if (mapper.dfgCommutatedSet.contains(node.name)) {
                node.commutated = true
                skewMap.update(node.name, -skewMap(node.name))
              }
            }
          }
          if (!PillarsConfig.USE_RELATIVE_SKEW) {
            skewMap.clear()
            for (node <- dfg.opNodes) {
              if (mapper.dfgMultiInputMap.contains(node.name)) {
                val sinkName = node.name
                val inputs = mapper.dfgMultiInputMap(node.name)
                val sourceName0 = inputs(0)
                val sourceName1 = inputs(1)
                val name0 = "WaitSkew_" + sourceName0 + "_" + sinkName
                val name1 = "WaitSkew_" + sourceName1 + "_" + sinkName
                val waitSkew0 = mapper.waitSkewMap(name0)
                val waitSkew1 = mapper.waitSkewMap(name1)
                var skew = (waitSkew1 << PillarsConfig.LOG_SKEW_LENGTH) + waitSkew0
                if (mapper.dfgCommutatedSet.contains(node.name)) {
                  skew = (waitSkew0 << PillarsConfig.LOG_SKEW_LENGTH) + waitSkew1
                }
                skewMap.put(node.name, skew)
              }
            }
          }
          def convert(in: Map[String,Int]): java.util.Map[String,Integer] = {
            mapAsJavaMap(in.map(kv => (kv._1, Integer.valueOf(kv._2))))
          }
          dfg.updateSchedule(filename + "_r.txt", convert(mapper.dfgLatencyMap), convert(skewMap), filename + "_r.txt")
        }
        else {
          Scheduler.schedule(dfg, mrrg, filename = filename, II = dfg.II)
        }

        def convertLists(in: Map[Int,ArrayBuffer[Int]]): Map[Integer, List[Integer]] = {
          in.map{case (k,v) => (Integer.valueOf(k), bufferAsJavaList(v.map(Integer.valueOf(_))))}
        }
        def convertNestedLists(in: Map[Int,ArrayBuffer[ArrayBuffer[Int]]]): Map[Integer, List[List[Integer]]] = {
          in.map{case (k,v) => (Integer.valueOf(k), bufferAsJavaList(
          v.map(buf => bufferAsJavaList(buf.map(Integer.valueOf(_))))))}
        }
        dfg.func2regMap = convertLists(mapper.func2regMap)
        dfg.funcDirect2funcMap = convertNestedLists(mapper.func2funcMap)
        dfg.reg2funcMap = convertNestedLists(mapper.reg2funcMap)
        dfg.regConnect = convertLists(mapper.regConnect)
        dfg.synthesizable = true
        dfg.regNum = mapper.regMap.size
      }
      mapper.elapsedTime
    }

    System.exit(-1) // for debug purpose

    elapsedTime
  }
}
