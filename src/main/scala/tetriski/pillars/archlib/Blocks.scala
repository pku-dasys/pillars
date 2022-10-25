package tetriski.pillars.archlib

import chisel3.util.log2Up
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{BlockTrait, OpEnum}

import scala.collection.mutable.ArrayBuffer

//TODO: update all connections with new format.

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
 *
 * @constructor create an abstract ADRES PE model
 * @param name            the name of the model
 * @param useMuxBypass    a parameter indicating whether this PE uses two additional
 *                        bypass multiplexers
 * @param opList          the subset of optional operations for the ALU in this PE
 * @param aluSupBypass    a parameter indicating whether the ALU should support bypass
 * @param inPortsNeighbor the names of input ports of this block
 * @param dataWidth       the data width
 */
class AdresPEBlock(name: String, useMuxBypass: Boolean, opList: List[OpEnum] = null,
                   aluSupBypass: Boolean = true, inPortsNeighbor: Array[String] = null,
                   dataWidth: Int = 32) extends BlockTrait {
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
  mux0.addInPorts((0 until neighborSize + 2).map(i => "input_" + i.toString).toArray)
  addElement(mux0)

  /** A multiplexer that can choose a data source for input_b of the ALU.
   */
  val mux1 = new ElementMux("mux1", List(neighborSize + 2, dataWidth))
  mux1.addOutPorts(Array("out_0"))
  mux1.addInPorts((0 until neighborSize + 2).map(i => "input_" + i.toString).toArray)
  addElement(mux1)

  /** A 2-RF with one input port and two output ports.
   * Its output port 0 is connected to mux0.
   * Its output port 1 is connected to the output port of this PE or muxOut.
   */
  val rf0 = new ElementRF("rf0", List(1, 1, 2, dataWidth))
  rf0.addOutPorts(Array("out_0", "out_1"))
  rf0.addInPorts(Array("input_0"))
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
    addConnect(rf0 / "out_1" -> term("out_0"))
  }
}

/** An STDNOC PE block with a local RF, an ALU and a multiplexer.
 * Author: DMD
 * @constructor create an abstract PE model
 * @param name            the name of the model
 * @param useMuxBypass    a parameter indicating whether this PE uses two additional
 *                        bypass multiplexers
 * @param opList          the subset of optional operations for the ALU in this PE
 * @param aluSupBypass    a parameter indicating whether the ALU should support bypass
 * @param inPortsNeighbor the names of input ports of this block
 * @param dataWidth       the data width
 */
class STDNOC_PEBlock(name: String, useMuxBypass: Boolean, opList: List[OpEnum] = null,
                   aluSupBypass: Boolean = true, isMemPE: Boolean =true, inPortsNeighbor: Array[String] = null,
                   dataWidth: Int = 32) extends BlockTrait {
  initName(name)
  //Set configuration region.
  setConfigRegion()

  addOutPorts(Array("out_w", "out_e", "out_n", "out_s"))
//  addOutPorts(Array("out_0", "rf_out"))
//  addOutPorts(Array("out_s"))
  addInPorts(inPortsNeighbor)

  val neighborSize = inPortsNeighbor.size

  var aluOpList = opList
  if (aluOpList == null) {
    //default
    aluOpList = List(OpEnum.ADD, OpEnum.MUL)
  }

  /** An ALU that can perform some operations.
   */
  val alu0 = new ElementAlu("alu0", aluOpList, aluSupBypass, List(dataWidth))//TODO: changes for predicate support??
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  alu0.addOutPorts(Array("out_0"))
  alu0.addInPorts(Array("input_a", "input_b", "input_const", "input_p"))
  addElement(alu0)

//  /** A multiplexer that can choose a data source for input_a of the ALU.
//   */
//  val mux0 = new ElementMux("mux0", List(neighborSize + 2, dataWidth))
//  mux0.addOutPorts(Array("out_0"))
//  mux0.addInPorts((0 until neighborSize + 2).map(i => "input_" + i.toString).toArray)
//  addElement(mux0)
//
//  /** A multiplexer that can choose a data source for input_b of the ALU.
//   */
//  val mux1 = new ElementMux("mux1", List(neighborSize + 2, dataWidth))
//  mux1.addOutPorts(Array("out_0"))
//  mux1.addInPorts((0 until neighborSize + 2).map(i => "input_" + i.toString).toArray)
//  addElement(mux1)
  /** A multiplexer that can choose a data source for I1 of the ALU from 7 sources(N,E,W,S,RP0,RP1,ALU_OUT)
   */
  val muxI1 = new ElementMux("muxI1", List(neighborSize + 3, dataWidth))
  muxI1.addOutPorts(Array("out_0"))
  muxI1.addInPorts((0 until neighborSize + 3).map(i => "input_" + i.toString).toArray)
  addElement(muxI1)
  /** A multiplexer that can choose a data source for I2 of the ALU from 7 sources(N,E,W,S,RP0,RP1)
   */
  val muxI2 = new ElementMux("muxI2", List(neighborSize + 2, dataWidth))
  muxI2.addOutPorts(Array("out_0"))
  muxI2.addInPorts((0 until neighborSize + 2).map(i => "input_" + i.toString).toArray)
  addElement(muxI2)
  /** A multiplexer that can choose a data source for P of the ALU from 7 sources(N,E,W,S,RP0,RP1)
   */
  val muxP = new ElementMux("muxP", List(neighborSize + 2, dataWidth))
  muxP.addOutPorts(Array("out_0"))
  muxP.addInPorts((0 until neighborSize + 2).map(i => "input_" + i.toString).toArray)
  addElement(muxP)

  /** A multiplexer that can choose a data source for North_Out from 3 sources(T,RP0,RP1)
   */
  val muxNO = new ElementMux("muxNO", List(3, dataWidth))
  muxNO.addOutPorts(Array("out_0"))
  muxNO.addInPorts((0 until 3).map(i => "input_" + i.toString).toArray)
  addElement(muxNO)

  /** A multiplexer that can choose a data source for East_Out from 3 sources(T,RP0,RP1)
   */
  val muxEO = new ElementMux("muxEO", List(3, dataWidth))
  muxEO.addOutPorts(Array("out_0"))
  muxEO.addInPorts((0 until 3).map(i => "input_" + i.toString).toArray)
  addElement(muxEO)

  /** A multiplexer that can choose a data source for West_Out from 3 sources(T,RP0,RP1)
   */
  val muxWO = new ElementMux("muxWO", List(3, dataWidth))
  muxWO.addOutPorts(Array("out_0"))
  muxWO.addInPorts((0 until 3).map(i => "input_" + i.toString).toArray)
  addElement(muxWO)

  /** A multiplexer that can choose a data source for South_Out from 3 sources(T,RP0,RP1)
   */
  val muxSO = new ElementMux("muxSO", List(3, dataWidth))
  muxSO.addOutPorts(Array("out_0"))
  muxSO.addInPorts((0 until 3).map(i => "input_" + i.toString).toArray)
  addElement(muxSO)

  /** A multiplexer that can choose a data source for WP0 from 5 sources(T,N,E,W,S)
   */
  val muxWP0 = new ElementMux("muxWP0", List(5, dataWidth))
  muxWP0.addOutPorts(Array("out_0"))
  muxWP0.addInPorts((0 until 5).map(i => "input_" + i.toString).toArray)
  addElement(muxWP0)

  /** A multiplexer that can choose a data source for WP1 from 5 sources(T,N,E,W,S)
   */
  val muxWP1 = new ElementMux("muxWP1", List(5, dataWidth))
  muxWP1.addOutPorts(Array("out_0"))
  muxWP1.addInPorts((0 until 5).map(i => "input_" + i.toString).toArray)
  addElement(muxWP1)


//  /** A 2-RF with one input port and two output ports.
//   * Its output port 0 is connected to mux0.
//   * Its output port 1 is connected to the output port of this PE or muxOut.
//   */
//  val rf0 = new ElementRF("rf0", List(1, 1, 2, dataWidth))
//  rf0.addOutPorts(Array("out_0", "out_1"))
//  rf0.addInPorts(Array("input_0"))
//  addElement(rf0)

  /** A 4-RF with two input ports and two output ports.
   * Its output port 0 is connected to mux0.
   * Its output port 1 is connected to the output port of this PE or muxOut.
   * log2Regs should be 2
   * val rf0 = new ElementRF("rf0", List(2, 2, 2, dataWidth))
   * Changed to 3 to avoid bug: garbage values are written in to register 3 (due to default config 11),
   * otherwise need wr_en signal to avoid this
   */
  val rf0 = new ElementRF("rf0", List(3, 2, 2, dataWidth))
  rf0.addOutPorts(Array("out_0", "out_1"))
  rf0.addInPorts(Array("input_0", "input_1"))
  addElement(rf0)

  /** A register with one input port and one output port.
   */
  val rfI1 = new ElementRF("rfI1", List(0, 1,1, dataWidth))
  rfI1.addOutPorts(Array("out_0"))
  rfI1.addInPorts(Array("input_0"))
  addElement(rfI1)

  /** A register with one input port and one output port.
   */
  val rfI2 = new ElementRF("rfI2", List(0, 1,1, dataWidth))
  rfI2.addOutPorts(Array("out_0"))
  rfI2.addInPorts(Array("input_0"))
  addElement(rfI2)

  /** A register with one input port and one output port.
   */
  val rfP = new ElementRF("rfP", List(0, 1,1, dataWidth))
  rfP.addOutPorts(Array("out_0"))
  rfP.addInPorts(Array("input_0"))
  addElement(rfP)
  /** A const unit connected to the multiplexers.
   */
  val const0 = new ElementConst("const0", List(dataWidth))
  const0.addOutPorts(Array("out_0"))
  addElement(const0)

//  val const1 = new ElementConst("const0", List(dataWidth))
//  const1.addOutPorts(Array("out_0"))
//  addElement(const1)

  for (i <- 0 until neighborSize) {
    addConnect(term(inPortsNeighbor(i)) -> muxI1 / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> muxI2 / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> muxP / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> muxWP0 / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> muxWP1 / s"input_$i")
  }

//  addConnect(const0 / "out_0" -> mux0 / s"input_$neighborSize")
//  addConnect(const0 / "out_0" -> mux1 / s"input_$neighborSize")
//  addConnect(rf0 / "out_0" -> mux0 / ("input_" + (neighborSize + 1).toString))
//  addConnect(rf0 / "out_0" -> mux1 / ("input_" + (neighborSize + 1).toString))
//  addConnect(mux0 / "out_0" -> alu0 / "input_a")
//  addConnect(mux1 / "out_0" -> alu0 / "input_b")
//  addConnect(alu0 / "out_0" -> rf0 / "input_0")

  addConnect(rf0 / "out_0" -> muxI1 / s"input_$neighborSize")
  addConnect(rf0 / "out_1" -> muxI1 / ("input_" + (neighborSize + 1).toString))
//  addConnect(const0 / "out_0" -> muxI1 / ("input_" + (neighborSize + 2).toString))
  addConnect(rf0 / "out_0" -> muxI2 / s"input_$neighborSize")
  addConnect(rf0 / "out_1" -> muxI2 / ("input_" + (neighborSize + 1).toString))
//  addConnect(const0 / "out_0" -> muxI2 / ("input_" + (neighborSize + 2).toString))
  addConnect(rf0 / "out_0" -> muxP / s"input_$neighborSize")
  addConnect(rf0 / "out_1" -> muxP / ("input_" + (neighborSize + 1).toString))
//  addConnect(const0 / "out_0" -> muxP / ("input_" + (neighborSize + 2).toString))

  addConnect(muxI1 / "out_0" -> rfI1 / "input_0")
  addConnect(muxI2 / "out_0" -> rfI2 / "input_0")
  addConnect(muxP / "out_0" -> rfP / "input_0")
  addConnect(muxWP0 / "out_0" -> rf0 / "input_0")
  addConnect(muxWP1 / "out_0" -> rf0 / "input_1")

  //This PE uses two additional bypass multiplexers.
//  if (useMuxBypass) {
//    /** A multiplexer that can choose a data source for bypass.
//     */
//    val muxBp = new ElementMux("muxBp", List(neighborSize, dataWidth))
//    muxBp.addOutPorts(Array("out_0"))
//    muxBp.addInPorts((0 until neighborSize).map(i => "input_" + i.toString).toArray)
//    addElement(muxBp)
//
//    /** A multiplexer that can choose a data source for the output port of this PE.
//     */
//    val muxOut = new ElementMux("muxOut", List(2, dataWidth))
//    //port sequnces outs: 0: out
//    //port sequnces inputs: 0: input_0, 1: input_1
//    muxOut.addOutPorts(Array("out_0"))
//    muxOut.addInPorts(Array("input_0", "input_1"))
//    addElement(muxOut)
//
//    for (i <- 0 until neighborSize) {
//      addConnect(term(inPortsNeighbor(i)) -> muxBp / s"input_$i")
//    }
//
//    addConnect(muxBp / "out_0" -> muxOut / "input_1")
//    addConnect(rf0 / "out_1" -> muxOut / "input_0")
//    addConnect(muxOut / "out_0" -> term("out_0"))
//
//  } else {
//    addConnect(rf0 / "out_1" -> term("out_0"))
//  }
//  addConnect(alu0 / "out_0" -> muxNO / "input_0")
  addConnect(rf0 / "out_0" -> muxNO / "input_1")
  addConnect(rf0 / "out_1" -> muxNO / "input_2")

//  addConnect(alu0 / "out_0" -> muxEO / "input_0")
  addConnect(rf0 / "out_0" -> muxEO / "input_1")
  addConnect(rf0 / "out_1" -> muxEO / "input_2")

//  addConnect(alu0 / "out_0" -> muxWO / "input_0")
  addConnect(rf0 / "out_0" -> muxWO / "input_1")
  addConnect(rf0 / "out_1" -> muxWO / "input_2")

//  addConnect(alu0 / "out_0" -> muxSO / "input_0")
  addConnect(rf0 / "out_0" -> muxSO / "input_1")
  addConnect(rf0 / "out_1" -> muxSO / "input_2")

  addConnect(muxNO / "out_0" ->  term("out_n"))
  addConnect(muxEO / "out_0" ->  term("out_e"))
  addConnect(muxWO / "out_0" ->  term("out_w"))
  addConnect(muxSO / "out_0" ->  term("out_s"))

//  addConnect(List("muxNO", "out_0"), List("out_n"))
//  addConnect(List("muxEO", "out_0"), List("out_e"))
//  addConnect(List("muxWO", "out_0"), List("out_w"))
//  addConnect(List("muxSO", "out_0"), List("out_s"))

  //  addConnect(muxSO / "out_0" ->  term("out_0"))

  addConnect(rfI1 / "out_0" -> alu0 / "input_a")
  addConnect(rfI2 / "out_0" -> alu0 / "input_b")
  addConnect(rfP / "out_0" -> alu0 / "input_p")
  addConnect(const0 / "out_0" -> alu0 / "input_const")

  if(isMemPE){
    /** A multiplexer that can choose a data source for data address.
     */
//    val muxAddr = new ElementMux("muxAddr", List(2, dataWidth))
//    muxAddr.addOutPorts(Array("out"))
//    muxAddr.addInPorts(Array("input_0","input_1"))
//    addElement(muxAddr)
//    for (j <- 0 until 2) {
//      muxAddr.addInPorts(Array("input_" + j.toString))
//      addConnect(List(List("neighbour_input_" + j.toString), List(muxAddr.getName(), "input_" + j.toString)))
//    }
//    addConnect(List(List("neighbour_input_" + j.toString), List(muxAddr.getName(), "input_" + j.toString)))
//    addConnect(rfI2 / "out_0"  -> muxAddr / "input_0")
//    addConnect(const0 / "out_0" -> muxAddr / "input_1")
//    addConnect(muxAddr / "out" -> alu0 / "input_b")

//    addConnect(rfI2 / "out_0" -> alu0 / "input_b")

    /** A multiplexer that can choose a data source for input data.
     */
//    val muxDataIn = new ElementMux("muxDataIn", List(2, dataWidth))
//    muxDataIn.addOutPorts(Array("out"))
//    muxDataIn.addInPorts(Array("input_0","input_1"))
//    addElement(muxDataIn)
//    for (j <- 0 until numNeighbour) {
//      muxDataIn.addInPorts(Array("input_" + j.toString))
//      addConnect(List(List("neighbour_input_" + j.toString), List(muxDataIn.getName(), "input_" + j.toString)))
//    }
//    addConnect(rfI1 / "out_0"  -> muxDataIn / "input_0")
//    addConnect(const0 / "out_0" -> muxDataIn / "input_1")
//    addConnect(muxDataIn / "out" -> alu0 / "input_a")

//    addConnect(rfI1 / "out_0"  -> alu0 / "input_a")

    /** An LSU can perform load or store operation.
     */
    val LSU = new ElementLSU2("loadStoreUnit", List(dataWidth))
    LSU.addInPorts(Array("addr", "dataIn", "constIn"))
    LSU.addOutPorts(Array("out"))
    addElement(LSU)

//    addConnect(List(List(muxAddr.getName(), "out"), List(LSU.getName(), "addr")))
//    addConnect(List(List(muxDataIn.getName(), "out"), List(LSU.getName(), "dataIn")))

    addConnect(rfI2 / "out_0" -> LSU / "addr")
    addConnect(rfI1 / "out_0" -> LSU / "dataIn")
    addConnect(const0 / "out_0" -> LSU / "constIn")

//    addConnect(List(List(LSU.getName(), "out"), List("out")))

    val muxT = new ElementMux("muxT", List(2, dataWidth))
    muxT.addOutPorts(Array("out_0"))
    muxT.addInPorts(Array("input_0", "input_1"))
    addElement(muxT)


   // addConnect(LSU / "out" -> muxT / "input_0")
//    addConnect(List(LSU.getName(), "out"),List(muxT.getName(),"input_0"))

    /** A register with one input port and one output port.
     * This makes latency of memPE is equal to two
     */
    val rfALUO = new ElementRF("rfALUO", List(0, 1,1, dataWidth))
    rfALUO.addOutPorts(Array("out_0"))
    rfALUO.addInPorts(Array("input_0"))
    addElement(rfALUO)

    addConnect(alu0 / "out_0" -> rfALUO / "input_0")
    addConnect(rfALUO / "out_0" -> muxT / "input_1")

    val rfLSUO = new ElementRF("rfLSUO", List(0, 1,1, dataWidth))
    rfLSUO.addOutPorts(Array("out_0"))
    rfLSUO.addInPorts(Array("input_0"))
    addElement(rfLSUO)

    addConnect(LSU / "out" -> rfLSUO / "input_0")
    addConnect(rfLSUO / "out_0" -> muxT / "input_0")

    addConnect(muxT / "out_0" -> muxWP0 / s"input_$neighborSize")
    addConnect(muxT / "out_0" -> muxWP1 / s"input_$neighborSize")
    addConnect(muxT / "out_0" -> muxNO / "input_0")
    addConnect(muxT / "out_0" -> muxEO / "input_0")
    addConnect(muxT / "out_0" -> muxWO / "input_0")
    addConnect(muxT / "out_0" -> muxSO / "input_0")
    addConnect(muxT / "out_0" -> muxI1 / ("input_" + (neighborSize + 2).toString))
  } else{

    //addConnect(mux2 / "out_0" -> alu0 / "input_n")
    addConnect(alu0 / "out_0" -> muxWP0 / s"input_$neighborSize")
    addConnect(alu0 / "out_0" -> muxWP1 / s"input_$neighborSize")

    addConnect(alu0 / "out_0" -> muxNO / "input_0")
    addConnect(alu0 / "out_0" -> muxEO / "input_0")
    addConnect(alu0 / "out_0" -> muxWO / "input_0")
    addConnect(alu0 / "out_0" -> muxSO / "input_0")


    addConnect(alu0 / "out_0" -> muxI1 / ("input_" + (neighborSize + 2).toString))

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
 * @param alternation  a parameter indicating whether using alternate opLists
 * @param dataWidth    the data width
 */
class TileLSUBlock(name: String, x: Int, y: Int, numIn: Int, numOut: Int,
                   useMuxBypass: Boolean = true, complex: Boolean = false,
                   alternation: Boolean = false, dataWidth: Int = 32)
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
      var inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")
      if (complex) {
        inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s",
          "input_wn", "input_ws", "input_en", "input_es", "input_lsu")
      }
      var opList = List(OpEnum.ADD, OpEnum.MUL, OpEnum.SUB, OpEnum.SHLL, OpEnum.SHRA)
      if (alternation && (i + j) % 2 == 1) {
        opList = List(OpEnum.ADD, OpEnum.SUB)
      }
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

      val peWN = peMap(((i - 1) + x) % x + ((j - 1 + y) % y) * x)
      val peWS = peMap(((i - 1) + x) % x + ((j + 1) % y) * x)
      val peEN = peMap((i + 1) % x + ((j - 1 + y) % y) * x)
      val peES = peMap((i + 1) % x + ((j + 1) % y) * x)
      if (j != y - 1) {
         addConnect(peCurrent / "out_0" -> peS / "input_n")

        if (complex) {
          addConnect(peCurrent / "out_0" -> peWS / "input_en")
          addConnect(peCurrent / "out_0" -> peES / "input_wn")
        }
      } else {
        addConnect(ioBlock / s"neighbour_out_$i" -> peS / "input_n")
        addConnect(peS / "out_0" -> ioBlock / s"neighbour_input_$i")

        if (complex) {
          addConnect(ioBlock / s"neighbour_out_$i" -> peWS / "input_en")
          addConnect(ioBlock / s"neighbour_out_$i" -> peES / "input_wn")
        }
      }

      addConnect(peCurrent / "out_0" -> peN / "input_s")
      addConnect(peCurrent / "out_0" -> peE / "input_w")
      addConnect(peCurrent / "out_0" -> peW / "input_e")

      if (complex) {
        addConnect(peCurrent / "out_0" -> peWN / "input_es")
        addConnect(peCurrent / "out_0" -> peEN / "input_ws")
      }
    }
  }

  /** PEs in the same row share a LoadStoreUnit.
   */
  for (j <- 0 until y) {
    val lsuBlock = new AdresLSUBlock("lsu_" + j.toString, x)
    addBlock(lsuBlock)
    for (i <- 0 until x) {
      val pe = peMap(i + j * x)
      addConnect(pe / "out_0" -> lsuBlock / s"neighbour_input_$i")
      addConnect(lsuBlock / "out" -> pe / "input_lsu")
    }
  }

}

/** A tile with an STD NOC architecture. IOBlock, a PE array which has N2N connectivity.
 * PEs in the leftmost row are memory capable PEs. MemPE has LSU.
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
 * @param alternation  a parameter indicating whether using alternate opLists
 * @param dataWidth    the data width
 */
class STDNOC_Block(name: String, x: Int, y: Int, numIn: Int, numOut: Int,
                   useMuxBypass: Boolean = true, complex: Boolean = false,
                   alternation: Boolean = false, dataWidth: Int = 32)
  extends BlockTrait {
  initName(name)

  addOutPorts((0 until numOut).map(i => "out_" + i.toString).toArray)
  addInPorts((0 until numIn).map(i => "input_" + i.toString).toArray)
  val ioBlock = new AdresIOBlock("ioBlock", numIn, numOut, 2 * x, dataWidth = dataWidth)
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
  var peMap = Map[Int, Parsed_PEBlock_Test]()
//  var peMap = Map[Int, AdresPEBlock]()

  for (j <- 0 until y) {
    for (i <- 0 until x) {
      var inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s")
//      if (complex) {
//        inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s",
//          "input_wn", "input_ws", "input_en", "input_es", "input_lsu")
//      }
//      var opList = List(OpEnum.ADD, OpEnum.MUL, OpEnum.SUB, OpEnum.SHLL, OpEnum.SHRA)
      var opList = List(OpEnum.ADD,
        OpEnum.SUB,
        OpEnum.AND,
        OpEnum.OR,
        OpEnum.XOR,
        OpEnum.MUL,
        OpEnum.SLT,
        OpEnum.LS,
        OpEnum.CLT,
        OpEnum.RS,
        OpEnum.MOVC,
        OpEnum.DIV,
        OpEnum.CMP,
        OpEnum.CGT,
        OpEnum.SELECT,
        OpEnum.CMERGE,
        OpEnum.ADD_CONST,
        OpEnum.LS_CONST,
        OpEnum.CMP_CONST,
        OpEnum.CMERGE_CONST,
        OpEnum.CMERGE_NPB)
//      if (alternation && (i + j) % 2 == 1) {
//        opList = List(OpEnum.ADD, OpEnum.SUB)
//      }


      if(i==0) {
        val pe = new Parsed_PEBlock_Test("pe_" + j.toString + "_" + i.toString, opList = opList,
          useMuxBypass = useMuxBypass,isMemPE=true, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
        peMap = peMap + ((i + j * x) -> pe)
        addBlock(pe)
      }else{
        val pe = new Parsed_PEBlock_Test("pe_" + j.toString + "_" + i.toString, opList = opList,
          useMuxBypass = useMuxBypass,isMemPE=false, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
        peMap = peMap + ((i + j * x) -> pe)
        addBlock(pe)
      }

//      val pe = new STDNOC_PEBlock("pe_" + j.toString + "_" + i.toString, opList = opList,
//        useMuxBypass = useMuxBypass, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
//      peMap = peMap + ((i + j * x) -> pe)
//      addBlock(pe)
    }
  }
  for (j <- 0 until y) {
    for (i <- 0 until x) {
      val peCurrent = peMap(i + j * x)
      val peN = peMap(i + ((j - 1 + y) % y) * x)
      val peS = peMap(i + ((j + 1) % y) * x)
      val peE = peMap((i + 1) % x + j * x)
      val peW = peMap(((i - 1) + x) % x + j * x)

//      val peWN = peMap(((i - 1) + x) % x + ((j - 1 + y) % y) * x)
//      val peWS = peMap(((i - 1) + x) % x + ((j + 1) % y) * x)
//      val peEN = peMap((i + 1) % x + ((j - 1 + y) % y) * x)
//      val peES = peMap((i + 1) % x + ((j + 1) % y) * x)
      if (j != y - 1) {
        addConnect(peCurrent / "out_s" -> peS / "input_n")

//        addConnect(peCurrent / "out_0" -> peS / "input_n")
//        if (complex) {
//          addConnect(peCurrent / "out_0" -> peWS / "input_en")
//          addConnect(peCurrent / "out_0" -> peES / "input_wn")
//        }
      } else {
        addConnect(ioBlock / s"neighbour_out_$i" -> peS / "input_n")
        addConnect(peS / "out_n" -> ioBlock / s"neighbour_input_$i")
        val k = i + x
        addConnect(ioBlock / s"neighbour_out_$k" -> peCurrent / "input_s")
        addConnect(peCurrent / "out_s" -> ioBlock / s"neighbour_input_$k")
//        addConnect(peS / "out_s" -> ioBlock / s"neighbour_input_$i")

//        if (complex) {
//          addConnect(ioBlock / s"neighbour_out_$i" -> peWS / "input_en")
//          addConnect(ioBlock / s"neighbour_out_$i" -> peES / "input_wn")
//        }
      }

      addConnect(peCurrent / "out_n" -> peN / "input_s")
      addConnect(peCurrent / "out_e" -> peE / "input_w")
      addConnect(peCurrent / "out_w" -> peW / "input_e")
//      addConnect(peCurrent / "out_s" -> peN / "input_s")
//      addConnect(peCurrent / "out_s" -> peE / "input_w")
//      addConnect(peCurrent / "out_s" -> peW / "input_e")
//      if (complex) {
//        addConnect(peCurrent / "out_0" -> peWN / "input_es")
//        addConnect(peCurrent / "out_0" -> peEN / "input_ws")
//      }
    }
  }

  /** PEs in the same row share a LoadStoreUnit.
   */
//  for (j <- 0 until y) {
//    val lsuBlock = new AdresLSUBlock("lsu_" + j.toString, x)
//    addBlock(lsuBlock)
//    for (i <- 0 until x) {
//      val pe = peMap(i + j * x)
//      addConnect(pe / "out_0" -> lsuBlock / s"neighbour_input_$i")
//      addConnect(lsuBlock / "out" -> pe / "input_lsu")
//    }
//  }

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
  val constParams = List(32)
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