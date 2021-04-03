package tetriski.pillars.archlib

import Chisel.log2Up
import chisel3.Module
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{BlockTrait, ElementTrait, ModuleRegistry, OpEnum, OpcodeTranslator}
import tetriski.pillars.core.MRRGMode._
import tetriski.pillars.hardware.{Alu, ConstUnit, Counter, LoadStoreUnit, Multiplexer, RegisterFile}

/** An element corresponding arithmetic logical unit.
 *
 * @constructor create an abstract ALU model
 * @param name      the name of the model
 * @param aluOpList the subset of optional operations
 * @param supBypass a parameter indicating whether the ALU should support bypass
 * @param moduleParams    List(width)
 */
class ElementAlu(name: String, aluOpList: List[OpEnum],
                 supBypass: Boolean, moduleParams: List[Int]) extends ElementTrait {
  //Module ID 0
//  setTypeID(0)

  setSupOps(aluOpList)

  val aluFunSelect = OpcodeTranslator.getAluFunSelect(aluOpList, supBypass)

  //Currently, we have 14 optional operations, so the configBits is 4.
  //TODO: automatically infer configBits to reduce the reconfiguration overhead of ALU.
  val configBits = 4
  setParams((aluFunSelect +: moduleParams) :+ configBits)
  setName(name)

  // support bypass
  if (supBypass) {
    addInternalNodesNum(2)
  } else {
    addInternalNodesNum(1)
  }

  def genModuleRule()={
    val rule = () => Module(new Alu(aluFunSelect, moduleParams(0)))
    rule
  }

  override val correlation = classOf[Alu]
  setTypeID(ModuleRegistry.getID(this))
}

/** An element corresponding register file.
 *
 * @constructor create an abstract RF model
 * @param name      the name of the model
 * @param moduleParams    List(log2Regs, numIn, numOut, w)
 */
class ElementRF(name: String, moduleParams: List[Int]) extends ElementTrait {

  //Module ID 1
//  setTypeID(1)

  setSupOps(List())

  /** If all registers in a RF have stored or will stored a data, {@code forbidden} will be true.
   * It is a part of configuration of RF.
   */
  var forbidden = false

  var configBits = moduleParams(0) * (moduleParams(1) + moduleParams(2)) + 1
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(Math.pow(2, moduleParams.head).toInt)
  setMRRGMode(REG_MODE)

  def genModuleRule()={
    val rule = () => Module(new RegisterFile(moduleParams(0), moduleParams(1), moduleParams(2), moduleParams(3)))
    rule
  }

  override val correlation = classOf[RegisterFile]
  setTypeID(ModuleRegistry.getID(this))
}

/** An element corresponding multiplexer.
 *
 * @constructor create an abstract mux model
 * @param name      the name of the model
 * @param moduleParams    List(numIn, w)
 */
class ElementMux(name: String, moduleParams: List[Int]) extends ElementTrait {

  //Module ID 2
//  setTypeID(2)

  setSupOps(List())

  val configBits = log2Up(moduleParams(0))
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(1)

  def genModuleRule()={
    val rule = () => Module(new Multiplexer(moduleParams(0), moduleParams(1)))
    rule
  }

  override val correlation = classOf[Multiplexer]
  setTypeID(ModuleRegistry.getID(this))
}

/** An element corresponding const unit.
 *
 * @constructor create an abstract const unit model
 * @param name      the name of the model
 * @param moduleParams    List(w)
 */
class ElementConst(name: String, moduleParams: List[Int]) extends ElementTrait {
  //Module ID 3
//  setTypeID(3)

  setSupOps(List(OpEnum.CONST))

  //The configuration is the output of a const unit.
  val configBits = moduleParams(0)
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(1)

  def genModuleRule()={
    val rule = () => Module(new ConstUnit(moduleParams(0)))
    rule
  }

  override val correlation = classOf[ConstUnit]
  setTypeID(ModuleRegistry.getID(this))
}

/** An element corresponding load/store unit.
 *
 * @constructor create an abstract LSU model
 * @param name      the name of the model
 * @param moduleParams    List(w)
 */
class ElementLSU(name: String, moduleParams: List[Int]) extends ElementTrait {
  //Module ID 4
//  setTypeID(4)

  setSupOps(List(OpEnum.LOAD, OpEnum.STORE))

  //0 for load, 1 for store
  val configBits = 1
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(1)
  setMRRGMode(MEM_MODE)

  def genModuleRule()={
    val rule = () => Module(new LoadStoreUnit(moduleParams(0)))
    rule
  }

  override val correlation = classOf[LoadStoreUnit]
  setTypeID(ModuleRegistry.getID(this))
}

/** An element corresponding counter.
 *
 * @constructor create an abstract counter model
 * @param name      the name of the model
 * @param moduleParams    List(w)
 */
class ElementCounter(name: String, moduleParams: List[Int]) extends ElementTrait {

  setSupOps(List(OpEnum.INCR))

  //The configuration of a counter consists of freq (interval cycles of value change), end (stop value),
  //step (value changes per interval), and init (initial value)..
  val configBits = moduleParams(0) * 4
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(1)

  def genModuleRule()={
    val rule = () => Module(new Counter(moduleParams(0)))
    rule
  }

  override val correlation = classOf[Counter]
  setTypeID(ModuleRegistry.getID(this))
}

///** A block.
// *
// * @deprecated
// * @param name      the name of the model
// */
//class Block(name: String) extends BlockTrait {
//  setName(name)
//  hierarchyName.append(name)
//}