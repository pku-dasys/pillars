package tetriski.pillars.archlib

import chisel3.util.log2Up
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{BlockTrait, OpEnum}
import tetriski.pillars.hardware.PillarsConfig

import scala.collection.mutable.ArrayBuffer

//TODO: update all connections with new format.


/** A tile with an IOBlock and a PE array which has torus connectivity.
 *
 * @constructor create an abstract tile model with an IOBlock and a PE array which has torus connectivity
 * @param name         the name of the model
 * @param x            the columns of PE array in this tile
 * @param y            the rows of PE array in this tile
 * @param numIn        the number of input ports of this tile
 * @param numOut       the number of output ports of this tile
 * @param useMuxBypass a parameter indicating whether PEs in this tile use two additional
 *                     bypass multiplexers
 * @param dataWidth    the data width
 */
class TileBlock(name: String, x: Int, y: Int, numIn: Int, numOut: Int,
                useMuxBypass: Boolean = true, dataWidth: Int = 32)
  extends BlockTrait {
  initName(name)

  addOutPorts((0 to numOut - 1).map(i => "out_" + i.toString).toArray)
  addInPorts((0 to numIn - 1).map(i => "input_" + i.toString).toArray)
  val ioBlock = new AdresIOBlock("ioBlock", numIn, numOut, x, dataWidth = dataWidth)
  addBlock(ioBlock)

  for (i <- 0 until numOut) {
    addConnect(List(List(ioBlock.getName() + "/", "out_" + i.toString), List("out_" + i.toString)))
  }
  for (i <- 0 until numIn) {
    addConnect(List(List("input_" + i.toString), List(ioBlock.getName() + "/", "input_" + i.toString)))
  }


  /** A PE array which has torus connectivity.
   */
  var peMap = Map[Int, AdresPEBlock]()
  for (j <- 0 until y) {
    for (i <- 0 until x) {
      val inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s")
      val opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR, OpEnum.MUL)
      val pe = new AdresPEBlock("pe_" + j.toString + "_" + i.toString, opList = opList,
        useMuxBypass = useMuxBypass, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
      peMap = peMap + ((i + j * x) -> pe)
      addBlock(pe)
    }
  }
  for (j <- 0 until y) {
    for (i <- 0 until x) {
      val peCurrent = peMap(i + j * x)
      val peN = peMap(i + ((j - 1 + y) % y) * x)
      val peS = peMap(i + ((j + 1) % y) * x)
      val peE = peMap((i + 1) % x + j * x)
      val peW = peMap(((i - 1) + x) % x + j * x)
      if (j != y - 1) {
        connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peS.getName() + "/", "input_n")))
      } else {
        connectArray.append(List(List(ioBlock.getName() + "/", "neighbour_out_" + i.toString),
          List(peS.getName() + "/", "input_n")))
        connectArray.append(List(List(peS.getName() + "/", "out_0"),
          List(ioBlock.getName() + "/", "neighbour_input_" + i.toString)))
      }
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peN.getName() + "/", "input_s")))
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peE.getName() + "/", "input_w")))
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peW.getName() + "/", "input_e")))
    }
  }

}

/** A tile with an IOBlock, some LSUBLocks and a PE array which has torus connectivity.
 * PEs in the same row share a block with a load/store unit.
 *
 * @constructor create an abstract tile model
 *              with an IOBlock, some LSUBLocks and a PE array which has torus connectivity
 * @param name         the name of the model
 * @param x            the columns of PE array in this tile
 * @param y            the rows of PE array in this tile
 * @param numIn        the number of input ports of this tile
 * @param numOut       the number of output ports of this tile
 * @param useMuxBypass a parameter indicating whether PEs in this tile use two additional
 *                     bypass multiplexers
 * @param complex      a parameter indicating whether using more routable connections
 * @param isToroid     a parameter indicating whether using toroid architecture
 * @param alternation  a parameter indicating whether using alternate PEs
 * @param basicOpList  basic operator list of ALUs
 * @param dataWidth    the data width
 */
class TileLSUBlock(name: String, x: Int, y: Int, numIn: Int, numOut: Int,
                   useMuxBypass: Boolean = true, complex: Boolean = false, isToroid: Boolean = true,
                   alternation: Boolean = false, basicOpList: List[OpEnum] = null, dataWidth: Int = 32)
  extends BlockTrait {
  initName(name)

  addOutPorts((0 to numOut - 1).map(i => "out_" + i.toString).toArray)
  addInPorts((0 to numIn - 1).map(i => "input_" + i.toString).toArray)
  val ioBlock = new AdresIOBlock("ioBlock", numIn, numOut, x, dataWidth = dataWidth)
  addBlock(ioBlock)

  for (i <- 0 until numOut) {
    //    addConnect(List(List(ioBlock.getName() + "/", "out_" + i.toString), List("out_" + i.toString)))
    addConnect(ioBlock / s"out_$i" -> term(s"out_$i"))
  }
  for (i <- 0 until numIn) {
    //    addConnect(List(List("input_" + i.toString), List(ioBlock.getName() + "/", "input_" + i.toString)))
    addConnect(term(s"input_$i") -> ioBlock / s"input_$i")
  }

  /** A PE array which has torus connectivity.
   */
  var peMap = Map[Int, AdresPEBlock]()
  for (j <- 0 until y) {
    for (i <- 0 until x) {
      var inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu", "input_counter")
      var neighborPortSet = inPortsNeighbor.toSet
      if (complex) {
        neighborPortSet ++= Set("input_wn", "input_ws", "input_en", "input_es")
      }
      if (j == 0) {
        neighborPortSet += "input_IOBlock"
      }
      if (!isToroid) {
        if (j == 0) {
          neighborPortSet --= Set("input_n", "input_wn", "input_en")
        }
        if (j == y - 1) {
          neighborPortSet --= Set("input_s", "input_ws", "input_es")
        }
        if (i == 0) {
          neighborPortSet --= Set("input_w", "input_ws", "input_wn")
        }
        if (i == x - 1) {
          neighborPortSet --= Set("input_e", "input_es", "input_en")
        }
      }


      inPortsNeighbor = neighborPortSet.toArray

      var defaultOpList = List(OpEnum.ADD, OpEnum.MUL, OpEnum.SUB, OpEnum.SHLA, OpEnum.SHRA)
      if (basicOpList != null) {
        defaultOpList = basicOpList
      }
      var opList = defaultOpList
      var dualALU = false
      if (alternation) {
        //        if (i < x / 2 && j < y / 2) {
        //          opList = List(OpEnum.ADD, OpEnum.SUB)
        //        }else if(i >= x / 2 && j >= y / 2){
        //          opList = List(OpEnum.ADD, OpEnum.MUL, OpEnum.SUB, OpEnum.SHLA,
        //          OpEnum.SHRA, OpEnum.AND, OpEnum.OR, OpEnum.XOR)
        //        }else {
        //          opList = List(OpEnum.AND, OpEnum.OR, OpEnum.XOR, OpEnum.SHRA, OpEnum.SHLA)
        //        }
        opList = List(OpEnum.ADD, OpEnum.MUL, OpEnum.SUB, OpEnum.SHLA, OpEnum.SHRA, OpEnum.AND, OpEnum.OR, OpEnum.XOR)
      }
      if (alternation && (i + j) % 2 == 1) {
        if (complex) {
          dualALU = true
        } else {
          opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR)
        }
      }
      val pe = new AdresPEBlock("pe_" + j.toString + "_" + i.toString, opList = opList,
        useMuxBypass = useMuxBypass, inPortsNeighbor = inPortsNeighbor, dualALU = dualALU, dataWidth = dataWidth)
      peMap = peMap + ((i + j * x) -> pe)
      addBlock(pe)
    }
  }
  for (j <- 0 until y) {
    for (i <- 0 until x) {
      val peCurrent = peMap(i + j * x)
      val peN = peMap(i + ((j - 1 + y) % y) * x)
      val peS = peMap(i + ((j + 1) % y) * x)
      val peE = peMap((i + 1) % x + j * x)
      val peW = peMap(((i - 1) + x) % x + j * x)

      val peWN = peMap(((i - 1) + x) % x + ((j - 1 + y) % y) * x)
      val peWS = peMap(((i - 1) + x) % x + ((j + 1) % y) * x)
      val peEN = peMap((i + 1) % x + ((j - 1 + y) % y) * x)
      val peES = peMap((i + 1) % x + ((j + 1) % y) * x)

      if (j == 0) {
        addConnect(ioBlock / s"neighbour_out_$i" -> peCurrent / "input_IOBlock")
        addConnect(peCurrent / "out_0" -> ioBlock / s"neighbour_input_$i")
      }

      var connectSet = Set[List[List[String]]]()
      if (isToroid) {
        connectSet ++= Set(peCurrent / "out_0" -> peN / "input_s", peCurrent / "out_0" -> peE / "input_w",
          peCurrent / "out_0" -> peW / "input_e", peCurrent / "out_0" -> peS / "input_n")
        if (complex) {
          connectSet ++= Set(peCurrent / "out_0" -> peWN / "input_es", peCurrent / "out_0" -> peEN / "input_ws",
            peCurrent / "out_0" -> peWS / "input_en", peCurrent / "out_0" -> peES / "input_wn")
        }
      }
      if (!isToroid) {
        if (j != 0) {
          connectSet += peCurrent / "out_0" -> peN / "input_s"
        }
        if (j != y - 1) {
          connectSet += peCurrent / "out_0" -> peS / "input_n"
        }
        if (i != 0) {
          connectSet += peCurrent / "out_0" -> peW / "input_e"
        }
        if (i != x - 1) {
          connectSet += peCurrent / "out_0" -> peE / "input_w"
        }

        if (complex) {
          if (j != 0 && i != 0) {
            connectSet += peCurrent / "out_0" -> peWN / "input_es"
          }
          if (j != 0 && i != x - 1) {
            connectSet += peCurrent / "out_0" -> peEN / "input_ws"
          }
          if (j != y - 1 && i != 0) {
            connectSet += peCurrent / "out_0" -> peWS / "input_en"
          }
          if (j != y - 1 && i != x - 1) {
            connectSet += peCurrent / "out_0" -> peES / "input_wn"
          }
        }
      }
      for (connect <- connectSet) {
        addConnect(connect)
      }
    }
  }

  /** PEs in the same row share a LoadStoreUnit.
   */
  for (j <- 0 until y) {
    val lsuBlock = new AdresLSUBlock("lsu_" + j.toString, x, dataWidth = dataWidth)
    addBlock(lsuBlock)
    for (i <- 0 until x) {
      val pe = peMap(i + j * x)
      addConnect(pe / "out_0" -> lsuBlock / s"neighbour_input_$i")
      addConnect(lsuBlock / "out" -> pe / "input_lsu")
    }
  }

  /** PEs in the same column share a Counter (independent width according to global config).
   */
  for (i <- 0 until x) {
    val counter = new ElementCounter("counter_" + i.toString, List(PillarsConfig.COUNTER_WIDTH))
    counter.addOutPorts(Array("out"))
    addElement(counter)
    for (j <- 0 until y) {
      val pe = peMap(i + j * x)
      addConnect(counter / "out" -> pe / "input_counter")
    }
  }

}

/** A complete tile with an IOBlock, an GlobalRFBlock, some LSUBLocks and a PE array.
 * PEs in the same row share a block with a load/store unit.
 * Similar to the original ADRES architecture,
 * the PEs in the top row share a global RF instead of a local RF.
 * It can generate different tiles according to the parameter.
 *
 * @example If isReduceArch and isFullArch are both false,
 *          a default architecture which has a PE array with torus connectivity will be generated.
 *          All PEs in the PE array have default set of operations.
 *          And some multiplexers is used to connect PEs and ports of the top design.
 * @example If isFullArch is true,
 *          a full architecture which has a PE array with torus connectivity will be generated.
 *          All PEs in the PE array have full set of operations.
 *          ALUs in full PEs can perform a full set of operations:
 *          add, subtract, multiply, shifts, and, or, and xor.
 *          And PEs and ports of the top design are connected directly.
 * @example If isReduceArch is true,
 *          a reduce architecture which has a PE array will be generated.
 *          Less PEs in the PE array have full set of operations.
 *          And PEs and ports of the top design are connected directly.
 * @constructor create an abstract tile model with an IOBlock, an GlobalRFBlock, some LSUBLocks and a PE array
 *              with an IOBlock, an GlobalRFBlock, some LSUBLocks and a PE array
 * @param name         the name of the model
 * @param x            the columns of PE array in this tile
 * @param y            the rows of PE array in this tile
 * @param numIn        the number of input ports of this tile
 * @param numOut       the number of output ports of this tile
 * @param useMuxBypass a parameter indicating whether PEs in this tile use two additional
 *                     bypass multiplexers
 * @param isReduceArch a parameter indicating whether the architecture should be reduced
 * @param isFullArch   a parameter indicating whether the architecture should be full
 * @param dataWidth    the data width
 */
class TileCompleteBlock(name: String, x: Int, y: Int, numIn: Int, numOut: Int, useMuxBypass: Boolean = true,
                        isReduceArch: Boolean = false, isFullArch: Boolean = false, dataWidth: Int = 32)
  extends BlockTrait {
  initName(name)
  addOutPorts((0 to numOut - 1).map(i => "out_" + i.toString).toArray)
  addInPorts((0 to numIn - 1).map(i => "input_" + i.toString).toArray)

  var isToroid = true
  if (isReduceArch) {
    isToroid = false
  }
  var regDirectConnectionIO = false
  if (isReduceArch || isFullArch) {
    regDirectConnectionIO = true
  }


  val ioBlock = new AdresIOBlock("ioBlock", numIn, numOut, x,
    dataWidth = dataWidth, regDirectConnectionIO = regDirectConnectionIO)
  addBlock(ioBlock)

  val globalRFBlock = new AdresGlobalRFBlock("globalRFBlock", x, dataWidth = dataWidth)
  addBlock(globalRFBlock)

  for (i <- 0 until numOut) {
    addConnect(ioBlock / s"out_$i" -> term(s"out_$i"))
  }
  for (i <- 0 until numIn) {
    addConnect(term(s"input_$i") -> ioBlock / s"input_$i")
  }

  /** A PE array which is generated according to parameters.
   */
  var peMap = Map[Int, BlockTrait]()
  for (j <- 0 until y) {
    for (i <- 0 until x) {
      if (j == 0) {
        var inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")
        if (!isToroid) {
          if (i == 0) {
            inPortsNeighbor = Array("input_e", "input_s", "input_lsu")
          } else if (i == x - 1) {
            inPortsNeighbor = Array("input_w", "input_s", "input_lsu")
          } else {
            inPortsNeighbor = Array("input_w", "input_e", "input_s", "input_lsu")
          }
        }
        var opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
          OpEnum.MUL, OpEnum.DIV, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
        if (isFullArch || isReduceArch) {
          opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
            OpEnum.MUL, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
        }
        val pe = new AdresVLIWPEBlock("pe_" + j.toString + "_" + i.toString, opList = opList,
          useMuxBypass = useMuxBypass, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
        peMap = peMap + ((i + j * x) -> pe)
        addBlock(pe)
      } else {
        var inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")
        if (!isToroid) {
          if (j != y - 1) {
            if (i == 0) {
              inPortsNeighbor = Array("input_e", "input_n", "input_s", "input_lsu")
            } else if (i == x - 1) {
              inPortsNeighbor = Array("input_w", "input_n", "input_s", "input_lsu")
            } else {
              inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")
            }
          } else {
            if (i == 0) {
              inPortsNeighbor = Array("input_e", "input_n", "input_lsu")
            } else if (i == x - 1) {
              inPortsNeighbor = Array("input_w", "input_n", "input_lsu")
            } else {
              inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_lsu")
            }
          }
        }
        var opList = List(OpEnum.ADD, OpEnum.MUL)

        if (isFullArch) {
          opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
            OpEnum.MUL, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
        } else if (isReduceArch) {
          if ((i % 2) == 0) {
            opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
              OpEnum.MUL, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
          } else {
            opList = List(OpEnum.ADD, OpEnum.SUB)
          }
        }

        val pe = new AdresPEBlock("pe_" + j.toString + "_" + i.toString, opList = opList,
          useMuxBypass = useMuxBypass, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
        peMap = peMap + ((i + j * x) -> pe)
        addBlock(pe)
      }
    }
  }
  for (j <- 0 until y) {
    for (i <- 0 until x) {
      val peCurrent = peMap(i + j * x)
      val peN = peMap(i + ((j - 1 + y) % y) * x)
      val peS = peMap(i + ((j + 1) % y) * x)
      val peE = peMap((i + 1) % x + j * x)
      val peW = peMap(((i - 1) + x) % x + j * x)
      if (j == 0) {
        val mux0Index = i * 2
        val muxOutIndex = i * 2 + 1
        addConnect(ioBlock / s"neighbour_out_$i" -> peCurrent / "input_IO")
        addConnect(peCurrent / "out_0" -> ioBlock / s"neighbour_input_$i")
        addConnect(peCurrent / "rf_out" -> globalRFBlock / s"input_$i")
        addConnect(globalRFBlock / s"out_$mux0Index" -> peCurrent / "input_rf_mux0")
        addConnect(globalRFBlock / s"out_$muxOutIndex" -> peCurrent / "input_rf_muxOut")
      }
      if (isToroid) {
        addConnect(peCurrent / "out_0" -> peS / "input_n")
        addConnect(peCurrent / "out_0" -> peN / "input_s")
        addConnect(peCurrent / "out_0" -> peE / "input_w")
        addConnect(peCurrent / "out_0" -> peW / "input_e")
      } else {
        if (j != y - 1) {
          addConnect(peCurrent / "out_0" -> peS / "input_n")
        }
        if (j != 0) {
          addConnect(peCurrent / "out_0" -> peN / "input_s")
        }
        if (i != x - 1) {
          addConnect(peCurrent / "out_0" -> peE / "input_w")
        }
        if (i != 0) {
          addConnect(peCurrent / "out_0" -> peW / "input_e")
        }
      }
    }
  }

  /** PEs in the same row share a LoadStoreUnit.
   */
  for (j <- 0 until y) {
    val lsuBlock = new AdresLSUBlock("lsu_" + j.toString, x, dataWidth = dataWidth)
    addBlock(lsuBlock)
    for (i <- 0 until x) {
      val pe = peMap(i + j * x)
      addConnect(pe / "out_0" -> lsuBlock / s"neighbour_input_$i")
      addConnect(lsuBlock / "out" -> pe / "input_lsu")
    }
  }

}

/** A subblock that performs computation between the selected input and a immediate operand.
 *
 * @constructor create an abstract block model that performs computation between the input and a immediate operand
 * @param name the name of the model
 */
class BlockImmediate(name: String) extends BlockTrait {
  val aluParams = List(32)
  val aluOpList = List(OpEnum.ADD)
  val muxParams = List(2, 32)
  val constParams = List(PillarsConfig.CONST_WIDTH)
  initName(name)

  addInPorts(Array("in0", "in1"))
  addOutPorts(Array("out0"))
  setConfigRegion()

  /** A multiplexer that can choose a data source for the port "inputA" of the ALU.
   */
  val mux0 = new ElementMux("mux0", muxParams)
  mux0.addInPorts(Array("input0", "input1"))
  mux0.addOutPorts(Array("out0"))
  addElement(mux0)

  /** An ALU that can perform some operations.
   */
  val alu0 = new ElementAlu("alu0", aluOpList, supBypass = true, aluParams)
  alu0.addInPorts(Array("inputA", "inputB"))
  alu0.addOutPorts(Array("out0"))
  addElement(alu0)

  /** A const unit connected to the port "inputB" of ALU.
   */
  val const0 = new ElementConst("const0", constParams)
  const0.addOutPorts(Array("out0"))
  addElement(const0)

  /** Interconnection inside this block.
   */
  addConnect(term("in0") -> mux0 / "input0")
  addConnect(term("in1") -> mux0 / "input1")
  addConnect(mux0 / "out0" -> alu0 / "inputA")
  addConnect(const0 / "out0" -> alu0 / "inputB")
  addConnect(alu0 / "out0" -> term("out0"))
}

/** A parent block that consists of a simple mesh of four sub-blocks.
 *
 * @constructor create an abstract block model that consists of a simple mesh of four sub-blocks
 * @param name the name of the model
 */
class BlockMesh(name: String) extends BlockTrait {
  initName(name)

  addInPorts(Array("in0", "in1"))
  addOutPorts(Array("out0"))

  /** The four sub-blocks.
   */
  val subBLocks = (0 until 4).map(i => new BlockImmediate(s"b$i"))
  subBLocks.foreach(x => addBlock(x))

  /** Interconnection inside this block.
   */
  Array(0, 1).foreach(i => addConnect(term(s"in$i") -> subBLocks(i) / "in0"))
  Array(1, 2).foreach(i => addConnect(subBLocks(0) / "out0" -> subBLocks(i) / "in1"))
  Array(0, 3).foreach(i => addConnect(subBLocks(1) / "out0" -> subBLocks(i) / "in1"))
  addConnect(subBLocks(2) / "out0" -> subBLocks(3) / "in0")
  addConnect(subBLocks(3) / "out0" -> subBLocks(2) / "in0")
  addConnect(subBLocks(3) / "out0" -> term("out0"))
}