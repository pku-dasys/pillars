package tetriski.pillars.archlib

import chisel3.util.log2Up
import tetriski.pillars.core.{BlockTrait, OpEnum}
import tetriski.pillars.core.OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer

/** A simple PE block, only used in a deprecated example.
 *
 * @deprecated
 * @param name the name of the model
 */
class PEBlock(name: String) extends BlockTrait {

  initName(name)
  setConfigRegion()

  addOutPorts(Array("out_0"))
  addInPorts(Array("input_0", "input_1", "input_2", "input_3"))

  val aluOpList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR, OpEnum.MUL)
  val supBypass = false
  val alu0 = new ElementAlu("alu0", aluOpList, supBypass, List(32))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  alu0.addOutPorts(Array("out_0"))
  alu0.addInPorts(Array("input_a", "input_b"))
  addElement(alu0)

  val mux0 = new ElementMux("mux0", List(5, 32))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_0, 1: input_1, 2: input_2, 3: input_3, 4: input_4
  mux0.addOutPorts(Array("out_0"))
  mux0.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4"))
  addElement(mux0)

  val mux1 = new ElementMux("mux1", List(5, 32))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_0, 1: input_1, 2: input_2, 3: input_3, 4: input_4
  mux1.addOutPorts(Array("out_0"))
  mux1.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4"))
  addElement(mux1)

  val rf0 = new ElementRF("rf0", List(1, 1, 2, 32))
  //port sequnces outs: 0: out_0, 1: out_1
  //port sequnces inputs: 0: input_0
  rf0.addOutPorts(Array("out_0", "out_1"))
  rf0.addInPorts(Array("input_0"))
  addElement(rf0)

  connectArray =
    ArrayBuffer(List(List("input_0"), List("mux0", "input_0")),
      List(List("input_0"), List("mux1", "input_0")),
      List(List("input_1"), List("mux0", "input_1")),
      List(List("input_1"), List("mux1", "input_1")),
      List(List("input_2"), List("mux0", "input_2")),
      List(List("input_2"), List("mux1", "input_2")),
      List(List("input_3"), List("mux0", "input_3")),
      List(List("input_3"), List("mux1", "input_3")),
      List(List("rf0", "out_0"), List("mux0", "input_4")),
      List(List("rf0", "out_0"), List("mux1", "input_4")),
      List(List("mux0", "out_0"), List("alu0", "input_a")),
      List(List("mux1", "out_0"), List("alu0", "input_b")),
      List(List("alu0", "out_0"), List("rf0", "input_0")),
      List(List("rf0", "out_1"), List("out_0")))
}

/** An ADRES PE block with a local RF, an ALU and some multiplexers.
 * Parameter 'dualALU' and parameter 'useMuxBypass' should not be true at the same time.
 *
 * @constructor create an abstract ADRES PE model
 * @param name            the name of the model
 * @param useMuxBypass    a parameter indicating whether this PE uses two additional
 *                        bypass multiplexers
 * @param opList          the subset of optional operations for the ALU in this PE
 * @param aluSupBypass    a parameter indicating whether the ALU should support bypass
 * @param inPortsNeighbor the names of input ports of this block
 * @param dualALU         a parameter indicating whether this PE should contain 2 ALU
 * @param dataWidth       the data width
 */
class AdresPEBlock(name: String, useMuxBypass: Boolean, opList: List[OpEnum] = null,
                   aluSupBypass: Boolean = true, inPortsNeighbor: Array[String] = null,
                   dualALU: Boolean = false, dataWidth: Int = 32) extends BlockTrait {
  initName(name)
  //Set configuration region.
  setConfigRegion()

  addOutPorts(Array("out_0"))
  addInPorts(inPortsNeighbor)

  val neighborSize = inPortsNeighbor.size

  var aluOpList = opList
  if (aluOpList == null) {
    //default
    aluOpList = List(OpEnum.ADD, OpEnum.MUL)
  } else if (dualALU){
    aluOpList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR)
  }

  /** An ALU that can perform some operations.
   */
  val alu0 = new ElementAlu("alu0", aluOpList, aluSupBypass, List(dataWidth))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  alu0.addOutPorts(Array("out_0"))
  alu0.addInPorts(Array("input_a", "input_b"))
  addElement(alu0)

  /** A multiplexer that can choose a data source for input_a of the ALU.
   */
  val mux0 = new ElementMux("mux0", List(neighborSize + 2, dataWidth))
  mux0.addOutPorts(Array("out_0"))
  mux0.addInPorts((0 until neighborSize + 2).map(i => s"input_$i").toArray)
  addElement(mux0)

  /** A multiplexer that can choose a data source for input_b of the ALU.
   */
  val mux1 = new ElementMux("mux1", List(neighborSize + 2, dataWidth))
  mux1.addOutPorts(Array("out_0"))
  mux1.addInPorts((0 until neighborSize + 2).map(i => s"input_$i").toArray)
  addElement(mux1)

  /** A 2-RF with one input port and two output ports.
   * Its output port 0 is connected to mux0.
   * Its output port 1 is connected to the output port of this PE or muxOut.
   */
  var rfInputNum = 1
  if (dualALU) {
    rfInputNum = 2
  }
  val rf0 = new ElementRF("rf0", List(1, rfInputNum, 2, dataWidth))
  rf0.addOutPorts(Array("out_0", "out_1"))
  rf0.addInPorts((0 until rfInputNum).map(i => s"input_$i").toArray)
  addElement(rf0)

  /** A const unit connected to the multiplexers.
   */
  val const0 = new ElementConst("const0", List(dataWidth))
  const0.addOutPorts(Array("out_0"))
  addElement(const0)


  for (i <- 0 until neighborSize) {
    addConnect(term(inPortsNeighbor(i)) -> mux0 / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> mux1 / s"input_$i")
  }

  addConnect(const0 / "out_0" -> mux0 / s"input_$neighborSize")
  addConnect(const0 / "out_0" -> mux1 / s"input_$neighborSize")
  addConnect(rf0 / "out_0" -> mux0 / ("input_" + (neighborSize + 1).toString))
  addConnect(rf0 / "out_0" -> mux1 / ("input_" + (neighborSize + 1).toString))
  addConnect(mux0 / "out_0" -> alu0 / "input_a")
  addConnect(mux1 / "out_0" -> alu0 / "input_b")
  addConnect(alu0 / "out_0" -> rf0 / "input_0")

  //This PE uses two additional bypass multiplexers.
  if (useMuxBypass) {
    /** A multiplexer that can choose a data source for bypass.
     */
    val muxBp = new ElementMux("muxBp", List(neighborSize, dataWidth))
    muxBp.addOutPorts(Array("out_0"))
    muxBp.addInPorts((0 until neighborSize).map(i => "input_" + i.toString).toArray)
    addElement(muxBp)

    /** A multiplexer that can choose a data source for the output port of this PE.
     */
    val muxOut = new ElementMux("muxOut", List(2, dataWidth))
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_0, 1: input_1
    muxOut.addOutPorts(Array("out_0"))
    muxOut.addInPorts(Array("input_0", "input_1"))
    addElement(muxOut)

    for (i <- 0 until neighborSize) {
      addConnect(term(inPortsNeighbor(i)) -> muxBp / s"input_$i")
    }

    addConnect(muxBp / "out_0" -> muxOut / "input_1")
    addConnect(rf0 / "out_1" -> muxOut / "input_0")
    addConnect(muxOut / "out_0" -> term("out_0"))

  } else {
    if (dualALU) {
      /** An ALU that can perform some operations.
       */
      val alu1 = new ElementAlu("alu1", opList, aluSupBypass, List(dataWidth))
      //port sequnces outs: 0: out
      //port sequnces inputs: 0: input_a, 1: input_b
      alu1.addOutPorts(Array("out_0"))
      alu1.addInPorts(Array("input_a", "input_b"))
      addElement(alu1)

      /** A multiplexer that can choose a data source for input_b of the ALU_1.
       */
      val mux2 = new ElementMux("mux2", List(neighborSize + 2, dataWidth))
      mux2.addOutPorts(Array("out_0"))
      mux2.addInPorts((0 until neighborSize + 2).map(i => s"input_$i").toArray)
      addElement(mux2)

      val register = new ElementRF("register0", List(0, 1, 1, dataWidth))
      register.addOutPorts(Array("out_0"))
      register.addInPorts(Array("input_0"))
      addElement(register)

      for (i <- 0 until neighborSize) {
        addConnect(term(inPortsNeighbor(i)) -> mux2 / s"input_$i")
      }
      addConnect(const0 / "out_0" -> mux2 / s"input_$neighborSize")
      addConnect(rf0 / "out_0" -> mux2 / ("input_" + (neighborSize + 1).toString))
      addConnect(alu0 / "out_0" -> register / "input_0")
      addConnect(register / "out_0" -> alu1 / "input_a")
      addConnect(mux2 / "out_0" -> alu1 / "input_b")
      addConnect(alu1 / "out_0" -> rf0 / "input_1")
    }
    addConnect(rf0 / "out_1" -> term("out_0"))
  }
}

/** An ADRES VLIW PE block with an ALU and some multiplexers.
 * This PE is connected to a global RF and the IO block, so it have some additional ports.
 *
 * @constructor create an abstract ADRES VLIW PE model
 * @param name            the name of the model
 * @param useMuxBypass    a parameter indicating whether this PE uses two additional
 *                        bypass multiplexers
 * @param opList          the subset of optional operations for the ALU in this PE
 * @param aluSupBypass    a parameter indicating whether the ALU should support bypass
 * @param inPortsNeighbor the names of input ports of this block
 * @param dataWidth       the data width
 */
class AdresVLIWPEBlock(name: String, useMuxBypass: Boolean, opList: List[OpEnum] = null,
                       aluSupBypass: Boolean = true, inPortsNeighbor: Array[String] = null,
                       dataWidth: Int = 32) extends BlockTrait {
  initName(name)
  //Set configuration region.
  setConfigRegion()

  addOutPorts(Array("out_0", "rf_out"))
  addInPorts(inPortsNeighbor ++ Array("input_rf_mux0", "input_rf_muxOut", "input_IO"))


  var aluOpList = opList
  if (aluOpList == null) {
    //default
    aluOpList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
      OpEnum.MUL, OpEnum.DIV, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
  }

  val neighborSize = inPortsNeighbor.size

  /** An ALU that can perform some operations.
   */
  val alu0 = new ElementAlu("alu0", aluOpList, aluSupBypass, List(dataWidth))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  alu0.addOutPorts(Array("out_0"))
  alu0.addInPorts(Array("input_a", "input_b"))
  addElement(alu0)

  /** A multiplexer that can choose a data source for input_a of the ALU.
   */
  val mux0 = new ElementMux("mux0", List(neighborSize + 3, dataWidth))
  mux0.addOutPorts(Array("out_0"))
  mux0.addInPorts((0 until neighborSize + 3).map(i => "input_" + i.toString).toArray)
  addElement(mux0)

  /** A multiplexer that can choose a data source for input_b of the ALU.
   */
  val mux1 = new ElementMux("mux1", List(neighborSize + 3, dataWidth))
  mux1.addOutPorts(Array("out_0"))
  mux1.addInPorts((0 until neighborSize + 3).map(i => "input_" + i.toString).toArray)
  addElement(mux1)

  /** A const unit connected to the multiplexers.
   */
  val const0 = new ElementConst("const0", List(dataWidth))
  const0.addOutPorts(Array("out_0"))
  addElement(const0)

  for (i <- 0 until neighborSize) {
    addConnect(List(inPortsNeighbor(i)), List("mux0", "input_" + i.toString))
    addConnect(List(inPortsNeighbor(i)), List("mux1", "input_" + i.toString))
  }
  addConnect(List("input_IO"), List("mux0", "input_" + (neighborSize).toString))
  addConnect(List("input_IO"), List("mux1", "input_" + (neighborSize).toString))
  addConnect(List("const0", "out_0"), List("mux0", "input_" + (neighborSize + 1).toString))
  addConnect(List("const0", "out_0"), List("mux1", "input_" + (neighborSize + 1).toString))
  addConnect(List("input_rf_mux0"), List("mux0", "input_" + (neighborSize + 2).toString))
  addConnect(List("input_rf_mux0"), List("mux1", "input_" + (neighborSize + 2).toString))
  addConnect(List("mux0", "out_0"), List("alu0", "input_a"))
  addConnect(List("mux1", "out_0"), List("alu0", "input_b"))
  addConnect(List("alu0", "out_0"), List("rf_out"))

  //This PE uses two additional bypass multiplexers.
  if (useMuxBypass) {
    /** A multiplexer that can choose a data source for bypass.
     */
    val muxBp = new ElementMux("muxBp", List(neighborSize + 1, dataWidth))
    muxBp.addOutPorts(Array("out_0"))
    muxBp.addInPorts((0 until neighborSize + 1).map(i => "input_" + i.toString).toArray)
    addElement(muxBp)

    /** A multiplexer that can choose a data source for the output port of this PE.
     */
    val muxOut = new ElementMux("muxOut", List(2, dataWidth, 1))
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_0, 1: input_1
    muxOut.addOutPorts(Array("out_0"))
    muxOut.addInPorts(Array("input_0", "input_1"))
    addElement(muxOut)

    for (i <- 0 until neighborSize) {
      addConnect(List(inPortsNeighbor(i)), List("muxBp", "input_" + i.toString))
    }
    addConnect(List("input_IO"), List("muxBp", "input_" + (neighborSize).toString))

    addConnect(List("muxBp", "out_0"), List("muxOut", "input_1"))
    addConnect(List("input_rf_muxOut"), List("muxOut", "input_0"))
    addConnect(List("muxOut", "out_0"), List("out_0"))
  } else {
    addConnect(List("input_rf_muxOut"), List("out_0"))
  }

}

/** A PE block with a single register.
 *
 * @constructor create an abstract block model with a single register
 * @param name      the name of the model
 * @param dataWidth the data width
 */
class RegBlock(name: String, dataWidth: Int = 32) extends BlockTrait {
  initName(name)

  addOutPorts(Array("out_0"))
  addInPorts(Array("input_0"))

  val rf0 = new ElementRF("rf0", List(0, 1, 1, dataWidth))
  rf0.addOutPorts(Array("out_0"))
  rf0.addInPorts(Array("input_0"))
  addElement(rf0)

  addConnect(List("input_0"), List("rf0", "input_0"))
  addConnect(List("rf0", "out_0"), List("out_0"))

}

/** A block connecting ports of the top design and PEs.
 *
 * @constructor create an abstract block model connecting ports of the top design and PEs
 * @param name                  the name of the model
 * @param numIn                 the number of input ports of the top design
 * @param numOut                the number of output ports of the top design
 * @param numNeighbour          the number of neighbour PEs
 * @param dataWidth             the data width
 * @param regDirectConnectionIO a parameter indicating which type of IOBlock should be generated
 */
class AdresIOBlock(name: String, numIn: Int, numOut: Int, numNeighbour: Int,
                   dataWidth: Int = 32, regDirectConnectionIO: Boolean = false) extends BlockTrait {

  initName(name)
  setConfigRegion()

  addOutPorts((0 to numOut).map(i => "out_" + i.toString).toArray)
  addOutPorts((0 to numNeighbour).map(i => "neighbour_out_" + i.toString).toArray)
  addInPorts((0 to numIn).map(i => "input_" + i.toString).toArray)
  addInPorts((0 to numNeighbour).map(i => "neighbour_input_" + i.toString).toArray)

  if (regDirectConnectionIO) {
    /** Directly connect PEs and ports of the top design.
     * Blocks with a single register is used in this type of IOBlock.
     */
    if (numIn == numNeighbour && numOut == numNeighbour) {
      val inRfBlocks = (0 until numNeighbour).map(i => new RegBlock("inRegBlock_" + i.toString, dataWidth))
      val outRfBlocks = (0 until numNeighbour).map(i => new RegBlock("outRegBlock_" + i.toString, dataWidth))
      for (i <- 0 until numNeighbour) {
        addBlock(inRfBlocks(i))
        addConnect(List("input_" + i.toString), List("inRegBlock_" + i.toString + "/", "input_0"))
        addConnect(List("inRegBlock_" + i.toString + "/", "out_0"), List("neighbour_out_" + i.toString))

        addBlock(outRfBlocks(i))
        addConnect(List("neighbour_input_" + i.toString), List("outRegBlock_" + i.toString + "/", "input_0"))
        addConnect(List("outRegBlock_" + i.toString + "/", "out_0"), List("out_" + i.toString))
      }
    } else {
      /** It should be ensured that the numbers of input ports, output ports and neighbour PEs are equal.
       */
      System.err.println("Disaccord between number of ports!")
    }
  } else {
    /** Use some multiplexers to connect PEs and ports of the top design.
     */
    for (i <- 0 until numOut) {
      val mux = new ElementMux("muxN2O_" + i.toString, List(numNeighbour, dataWidth))
      mux.addOutPorts(Array("out_0"))
      for (j <- 0 until numNeighbour) {
        mux.addInPorts(Array("input_" + j.toString))
        addConnect(List(List("neighbour_input_" + j.toString), List(mux.getName(), "input_" + j.toString)))
      }
      addElement(mux)
      addConnect(List(List(mux.getName(), "out_0"), List("out_" + i.toString)))
    }

    for (i <- 0 until numNeighbour) {
      val mux = new ElementMux("muxI2N_" + i.toString, List(numIn, dataWidth))
      mux.addOutPorts(Array("out_0"))
      for (j <- 0 until numIn) {
        mux.addInPorts(Array("input_" + j.toString))
        addConnect(List(List("input_" + j.toString), List(mux.getName(), "input_" + j.toString)))
      }
      addElement(mux)
      addConnect(List(List(mux.getName(), "out_0"), List("neighbour_out_" + i.toString)))
    }
  }

}

/** A block with an LSU and some multiplexers.
 *
 * @constructor create an abstract block model with an LSU and some multiplexers
 * @param name         the name of the model
 * @param numNeighbour the number of neighbour PEs
 * @param dataWidth    the data width
 */
class AdresLSUBlock(name: String, numNeighbour: Int, dataWidth: Int = 32) extends BlockTrait {

  initName(name)
  setConfigRegion()

  addOutPorts(Array("out"))
  addInPorts((0 to numNeighbour).map(i => "neighbour_input_" + i.toString).toArray)

  /** A multiplexer that can choose a data source for data address.
   */
  val muxAddr = new ElementMux("muxAddr", List(numNeighbour, dataWidth))
  muxAddr.addOutPorts(Array("out"))
  for (j <- 0 until numNeighbour) {
    muxAddr.addInPorts(Array("input_" + j.toString))
    addConnect(List(List("neighbour_input_" + j.toString), List(muxAddr.getName(), "input_" + j.toString)))
  }
  addElement(muxAddr)

  /** A multiplexer that can choose a data source for input data.
   */
  val muxDataIn = new ElementMux("muxDataIn", List(numNeighbour, dataWidth))
  muxDataIn.addOutPorts(Array("out"))
  for (j <- 0 until numNeighbour) {
    muxDataIn.addInPorts(Array("input_" + j.toString))
    addConnect(List(List("neighbour_input_" + j.toString), List(muxDataIn.getName(), "input_" + j.toString)))
  }
  addElement(muxDataIn)

  /** An LSU can perform load or store operation.
   */
  val LSU = new ElementLSU("loadStoreUnit", List(dataWidth))
  LSU.addInPorts(Array("addr", "dataIn"))
  LSU.addOutPorts(Array("out"))
  addConnect(List(List(muxAddr.getName(), "out"), List(LSU.getName(), "addr")))
  addConnect(List(List(muxDataIn.getName(), "out"), List(LSU.getName(), "dataIn")))
  addConnect(List(List(LSU.getName(), "out"), List("out")))

  addElement(LSU)

}

/** A block with a global RF.
 *
 * @constructor create an abstract block model with a global RF
 * @param name         the name of the model
 * @param numNeighbour the number of neighbour PEs
 * @param dataWidth    the data width
 */
class AdresGlobalRFBlock(name: String, numNeighbour: Int, dataWidth: Int = 32) extends BlockTrait {

  initName(name)
  setConfigRegion()

  val inNum = numNeighbour
  val outNum = inNum * 2
  val log2Reg = log2Up(outNum)

  addOutPorts((0 to (outNum - 1) * 2).map(i => "out_" + i.toString).toArray)
  addInPorts((0 to (inNum - 1)).map(i => "input_" + i.toString).toArray)

  /** A global RF that can transfer data between PEs.
   *
   * @example If the numNeighbour = 4,
   *          it is a 8-RF with 4 input ports and 8 output ports.
   */
  val global_rf = new ElementRF("global_rf", List(log2Reg, inNum, outNum, dataWidth))
  global_rf.addOutPorts((0 to (outNum - 1)).map(i => "out_" + i.toString).toArray)
  global_rf.addInPorts((0 to (inNum - 1)).map(i => "input_" + i.toString).toArray)
  addElement(global_rf)


  for (i <- 0 until inNum) {
    addConnect(List(global_rf.getName(), "out_" + (i * 2).toString), List("out_" + (i * 2).toString))
    addConnect(List(global_rf.getName(), "out_" + (i * 2 + 1).toString), List("out_" + (i * 2 + 1).toString))
    addConnect(List("input_" + i.toString), List(global_rf.getName(), "input_" + i.toString))
  }


}
