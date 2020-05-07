package tetriski.pillars.mapping

import java.io.FileWriter
import java.util.ArrayList

import tetriski.pillars.core.{MRRG, NodeMRRG}

import scala.collection.mutable.ArrayBuffer

/** This object is used to do mapping calling gurobimap_java.
 */
object ILPMap {
  /** Map given DFG(IR) to given MRRG,
   * and write result to file which name is related to filename using FileWriter fw.
   *
   * @param dfg      the given DFG
   * @param mrrg     the given MRRG
   * @param filename the name we will used to write result
   * @param fw       the FileWriter we used
   */
  def mapping(dfg: DFG, mrrg: MRRG, filename: String = null, fw: FileWriter = null): Double = {
    val mapper = new gurobimap_java(filename)

    val num_dfg_op = dfg.getOpSize()
    val num_dfg_val = dfg.getValSize()

    for (i <- 0 until num_dfg_op) {
      mapper.DFGopnodename.add(dfg.op_nodes(i).name)
      mapper.DFGopnodeopcode.add(Integer.valueOf(dfg.op_nodes(i).opcode.id))
      if (dfg.op_nodes(i).output != null) {
        mapper.DFGopnodeout.add(Integer.valueOf(dfg.val_nodes_map(dfg.op_nodes(i).output.name)))
      } else {
        mapper.DFGopnodeout.add(Integer.valueOf(-1))
      }
    }
    for (i <- 0 until num_dfg_val) {
      mapper.DFGvalnodename.add(dfg.val_nodes(i).name)
      var j = 0
      val outputSize = dfg.val_nodes(i).output.size
      val out = new ArrayList[Integer]()
      val operand = new ArrayList[Integer]()
      for (j <- 0 until outputSize) {
        out.add(Integer.valueOf(dfg.op_nodes_map(dfg.val_nodes(i).output(j).name)))
        operand.add(Integer.valueOf(dfg.val_nodes(i).output_operand(j)))
      }
      mapper.DFGvalnodeout.add(out)
      mapper.DFGvalnodeoutputoperand.add(operand)
    }

    var num_mrrg_r = 0
    var num_mrrg_f = 0
    val num_mrrg = mrrg.getSize()
    val routingNodes = ArrayBuffer[NodeMRRG]()
    val functionNodes = ArrayBuffer[NodeMRRG]()
    var nodeIDMap = Map[NodeMRRG, Int]()

    for (i <- 0 until num_mrrg) {
      if (mrrg.nodes(i).ops.size != 0) {
        functionNodes.append(mrrg.nodes(i))
        nodeIDMap = nodeIDMap + (mrrg.nodes(i) -> num_mrrg_f)
        var j = 0
        val opSize = mrrg.nodes(i).ops.size
        val opcodes = new ArrayList[Integer]()
        for (j <- 0 until opSize) {
          opcodes.add(Integer.valueOf(mrrg.nodes(i).ops(j).id))
        }
        mapper.MRRGfunctionSupportop.add(opcodes)
        num_mrrg_f += 1
      }
      else {
        routingNodes.append(mrrg.nodes(i))
        nodeIDMap = nodeIDMap + (mrrg.nodes(i) -> num_mrrg_r)
        num_mrrg_r += 1
      }
    }

    for (i <- 0 until num_mrrg_f) {
      mapper.MRRGfunctionname.add(functionNodes(i).name)
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
      mapper.MRRGfunctionfanin.add(fanin)
      mapper.MRRGfunctionfanintype.add(fanintype)

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
      mapper.MRRGfunctionfanout.add(fanout)
      mapper.MRRGfunctionfanouttype.add(fanouttype)
    }

    for (i <- 0 until num_mrrg_r) {
      mapper.MRRGroutingname.add(routingNodes(i).name)
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
      mapper.MRRGroutingfanin.add(fanIn)
      mapper.MRRGroutingfanintype.add(fanInType)

      val fanOutSize = routingNodes(i).fanOut.size
      val fanOut = new ArrayList[Integer]()
      val fanOutType = new ArrayList[Integer]()
      for (j <- 0 until fanOutSize) {
        fanOut.add(Integer.valueOf(nodeIDMap(routingNodes(i).fanOut(j))))
        if (routingNodes(i).fanOut(j).ops.size == 0) {
          fanOutType.add(Integer.valueOf(0))
        } else {
          fanOutType.add(Integer.valueOf(1))
        }
      }
      mapper.MRRGroutingfanout.add(fanOut)
      mapper.MRRGroutingfanouttype.add(fanOutType)
    }

    if (fw != null) {
      val result = mapper.ILPMap(fw)
      return result
    } else {
      val result = mapper.ILPMap()
      val routingResult = result(0)
      for (i <- 0 until num_mrrg_r) {
        if (routingResult.get(i).intValue != -1) {
          routingNodes(i).mapnode = dfg.val_nodes(routingResult.get(i).intValue())
          //println(routingNodes(i).name)
        }
      }
      val functionResult = result(1)
      for (i <- 0 until num_mrrg_f) {
        if (functionResult.get(i).intValue != -1) {
          functionNodes(i).mapnode = dfg.op_nodes(functionResult.get(i).intValue())
          //println(functionNodes(i).name)
        }
      }
    }
    -1
  }
}
