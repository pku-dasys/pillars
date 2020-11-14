package tetriski.pillars.archlib

import Chisel.log2Up
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{BlockTrait, ElementTrait, OpEnum, OpcodeTranslator}
import tetriski.pillars.core.MRRGMode._
import tetriski.pillars.hardware.PillarsConfig.USE_PREDICATE

/** An element corresponding arithmetic logical unit.
 *
 * @constructor create an abstract ALU model
 * @param name      the name of the model
 * @param aluOpList the subset of optional operations
 * @param supBypass a parameter indicating whether the ALU should support bypass
 * @param params    List(width)
 */
class ElementAlu(name: String, aluOpList: List[OpEnum], supBypass: Boolean, params: List[Int]) extends ElementTrait {
  //Module ID 0
  setTypeID(0)

  setSupOps(aluOpList)

  val aluFunSelect = OpcodeTranslator.getAluFunSelect(aluOpList, supBypass)

  //Currently, we have 14 optional operations, so the configBits is 4.
  //TODO: automatically infer configBits to reduce the reconfiguration overhead of ALU.
  var configBits = 4
  if (USE_PREDICATE){
    configBits = 5
  }else {
    configBits = 4
  }
  setParams((aluFunSelect +: params) :+ configBits)
  setName(name)

  // support bypass
  if (supBypass) {
    addInternalNodesNum(2)
  } else {
    addInternalNodesNum(1)
  }

}

/** An element corresponding register file.
 *
 * @constructor create an abstract RF model
 * @param name      the name of the model
 * @param params    List(log2Regs, numIn, numOut, w)
 */
class ElementRF(name: String, params: List[Int]) extends ElementTrait {

  //Module ID 1
  setTypeID(1)

  setSupOps(List())

  /** If all registers in a RF have stored or will stored a data, {@code forbidden} will be true.
   * It is a part of configuration of RF.
   */
  var forbidden = false

  var configBits = params(0) * (params(1) + params(2)) + 1
  setParams(params :+ configBits)
  setName(name)


  addInternalNodesNum(Math.pow(2, params.head).toInt)
  setMRRGMode(REG_MODE)
}

/** An element corresponding multiplexer.
 *
 * @constructor create an abstract mux model
 * @param name      the name of the model
 * @param params    List(numIn, w)
 */
class ElementMux(name: String, params: List[Int]) extends ElementTrait {

  //Module ID 2
  setTypeID(2)

  setSupOps(List())

  val configBits = log2Up(params(0))
  setParams(params :+ configBits)
  setName(name)

  addInternalNodesNum(1)
}

/** An element corresponding const unit.
 *
 * @constructor create an abstract const unit model
 * @param name      the name of the model
 * @param params    List(w)
 */
class ElementConst(name: String, params: List[Int]) extends ElementTrait {
  //Module ID 3
  setTypeID(3)

  setSupOps(List(OpEnum.CONST))

  //The configuration is the output of a const unit.
  val configBits = params(0)
  setParams(params :+ configBits)
  setName(name)

  addInternalNodesNum(1)
}

/** An element corresponding load/store unit.
 *
 * @constructor create an abstract LSU model
 * @param name      the name of the model
 * @param params    List(w)
 */
class ElementLSU(name: String, params: List[Int]) extends ElementTrait {
  //Module ID 4
  setTypeID(4)

  setSupOps(List(OpEnum.LOAD, OpEnum.STORE))

  //0 for load, 1 for store
  val configBits = 1
  setParams(params :+ configBits)
  setName(name)

  addInternalNodesNum(1)
  setMRRGMode(MEM_MODE)
}

/** An element corresponding load/store unit.
 *
 * @constructor create an abstract LSU model
 * @param name      the name of the model
 * @param params    List(w)
 */
class ElementLSU2(name: String, params: List[Int]) extends ElementTrait {
  //Module ID 4
  setTypeID(4)

  setSupOps(List(OpEnum.LOAD, OpEnum.STORE, OpEnum.LOADH, OpEnum.STOREH, OpEnum.LOADB, OpEnum.STOREB))

  //0 for load, 1 for store
  val configBits = 3
  setParams(params :+ configBits)
  setName(name)

  addInternalNodesNum(1)
  setMRRGMode(MEM_MODE)
}

/** A block.
 *
 * @deprecated
 * @param name      the name of the model
 */
class Block(name: String) extends BlockTrait {
  setName(name)
  hierarchyName.append(name)
}