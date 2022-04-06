package tetriski.pillars.mapping

import java.io.FileWriter
import java.util.ArrayList

import tetriski.pillars.core.{MRRG, MRRGMode, OpcodeTranslator}
import tetriski.pillars.hardware.PillarsConfig.USE_RELATIVE_SKEW

import scala.collection.JavaConverters
import scala.collection.mutable.ArrayBuffer

object SearchMap {
  def mapping(dfg: DFG, mrrg: MRRG, filename: String = null, fw: FileWriter = null,
              separatedPR: Boolean = false, scheduleControl: Boolean = false,
              skewLimit: Int = 2, latencyLimit: Int = 32
             ): Double = {
    assert(USE_RELATIVE_SKEW, "This mapper works only when USE_RELATIVE_SKEW is true!")

    val mapper = new searchMap(dfg.getOpSize(), mrrg.getSize())
    mapper.filename = filename


    dfg.opNodes.indices.foreach(x => mapper.dfgOPNodes.add(x))
    //dfg.valNodes.indices.foreach(x=> mapper.dfgValNodes.add(x + dfg.opNodes.length))
    // DFG node and edge
    for (x <- dfg.opNodes.indices) {
      val me = dfg.opNodes(x)
      mapper.DFGOpNodeName.add(me.name)

      for (y <- me.input.keys) {
        val b = dfg.opNodesMap(me.input(y).name)
        mapper.DFGin(x).add(b)
        mapper.DFGout(b).add(x)
      }

      if (me.input.size > 1) {
        val inputI = new ArrayList[Integer]()
        for (inputOperand <- 0 until me.input.size) {
          inputI.add(dfg.opNodesMap(me.input(inputOperand).output.name.replace("OUT","")))
        }
        mapper.DFGOpOperand.put(x, inputI)
        if (OpcodeTranslator.commutativeSet.contains(me.opcode)) {
          mapper.DFGCommutativeSet.add(x)
        }
//        mapper.DFGin(x).clear()
//        mapper.DFGin(x).addAll(inputI)
      }
    }

    var sramMap = Map[Int, ArrayBuffer[Int]]()
    //MRRG node and edge
    for (x <- mrrg.getOpNodeSet()) {
      mapper.mrrgFuncNodes.add(
        mrrg.nodeMap(x.name)
      )
      val sramID = mrrg.nodes(mrrg.nodeMap(x.name)).sramID
      if (sramID != -1) {
        if (sramMap.keySet.contains(sramID)) {
          sramMap(sramID).append(mrrg.nodeMap(x.name))
        } else {
          sramMap += sramID -> new ArrayBuffer[Int]
          sramMap(sramID).append(mrrg.nodeMap(x.name))
        }
      }
      mapper.MRRGFunctionName(mrrg.nodeMap(x.name)) = x.name
    }

    for (x <- mrrg.getNoOpSet()) {
      mapper.mrrgRoutingNodes.add(
        mrrg.nodeMap(x.name)
      )

      mapper.MRRGRoutingName(mrrg.nodeMap(x.name)) = x.name
    }
    for (x <- mrrg.nodes.indices) {
      val me = mrrg.nodes(x)
      for (y <- me.fanIn) {
        mapper.MRRGin(x).add(mrrg.nodeMap(y.name))
      }
      for (y <- me.fanOut) {
        mapper.MRRGout(x).add(mrrg.nodeMap(y.name))
      }
    }

    //MRRG op
    for (x <- mrrg.nodes.indices) {
      val me = mrrg.nodes(x)
      for (y <- me.ops) {
        mapper.MRRGop(x).add(Integer.valueOf(y.id))
      }
    }
    //DFG op
    for (x <- dfg.opNodes.indices) {
      val me = dfg.opNodes(x)
      mapper.DFGop(x).add(Integer.valueOf(me.opcode.id))
    }
    var sum = 0
    for (x <- mrrg.nodes.indices) {
      val me = mrrg.nodes(x)
      val mode = me.mode
      var latency = 0
      //printf("%d mode is %d\n",x,mode);
      if ((mode == MRRGMode.MEM_MODE) || (mode == MRRGMode.REG_MODE)) {
        latency = 1
      }
      mapper.MRRGlatency.add(latency)
      sum += latency
    }
    printf("total latency is %d\n", sum)
    mapper.II = dfg.II
    if(scheduleControl){
      mapper.MaxDelay = skewLimit
    }

    val fixedMapSRAM = dfg.fixedMapSRAM
    var fixedMapRelation = Map[Int, Set[Integer]]()
    for (opNode <- fixedMapSRAM.keys) {
      val opNodeIndex = dfg.opNodes.indexOf(opNode)
      val sramID = fixedMapSRAM(opNode)
//      if (opNodeIndex == 17 || opNodeIndex == 16) {
        fixedMapRelation += opNodeIndex -> sramMap(sramID).map(i => new Integer(i)).toSet
//      }
    }
    fixedMapRelation.map(pair => mapper.fixedMapRelation
      .put(pair._1, JavaConverters.setAsJavaSet(pair._2)))

    mapper.get()
    var skewMap = mapper.DFGRelativeSkewMap
    dfg.updateSchedule(filename + "_r.txt", mapper.DFGLatencyMap, skewMap, filename + "_r.txt")
    return 0.0
  }
}


