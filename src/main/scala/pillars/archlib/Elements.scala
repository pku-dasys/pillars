package pillars.archlib

import chisel3.util.log2Ceil
import chisel3.Module
import pillars.core.OpEnum.OpEnum
import pillars.core.{BlockTrait, ElementTrait, ModuleRegistry, OpEnum, OpcodeTranslator}
import pillars.core.MRRGMode._
import pillars.hardware.{Alu, ConstUnit, Counter, LoadStoreUnit, Multiplexer, RegisterFile}

/**
 * Represents an Arithmetic Logic Unit (ALU) element in the CGRA architecture.
 *
 * The ALU supports a configurable set of operations and optional bypass functionality.
 * It generates corresponding Chisel hardware module for the specified configuration.
 *
 * @constructor Create a new ALU element model
 * @param name       The name identifier for this ALU instance
 * @param aluOpList  List of supported ALU operations from OpEnum
 * @param supBypass  Boolean flag indicating if bypass functionality is supported
 * @param moduleParams List containing the data width as Int
 */
class ElementAlu(name: String, aluOpList: List[OpEnum],
                 supBypass: Boolean, moduleParams: List[Int]) extends ElementTrait {
  // Initialize supported operations
  setSupOps(aluOpList)

  // Generate function select mapping for ALU operations
  val aluFunSelect = OpcodeTranslator.getAluFunSelect(aluOpList, supBypass)

  // Configuration bits - currently fixed at 4 bits to support up to 16 operations
  // TODO: Make configBits dynamic based on number of operations
  val configBits = 4
  setParams((aluFunSelect +: moduleParams) :+ configBits)
  setName(name)

  // support bypass
  if (supBypass) {
    addInternalNodesNum(2)
  } else {
    addInternalNodesNum(1)
  }

  /**
   * Generates a rule function that creates the Chisel hardware module for this ALU.
   * @return Function that creates an Alu module with configured parameters
   */
  def genModuleRule(): () => Alu = {
    () => Module(new Alu(
      aluFunSelect,
      moduleParams(0),  // Data width
      name = getName()
    ))
  }

  override val correlation = classOf[Alu]
  setTypeID(ModuleRegistry.getID(this))
}

/**
 * Represents a Register File (RF) element in the CGRA architecture.
 *
 * The register file provides configurable storage with multiple read/write ports.
 * It generates corresponding Chisel hardware module for the specified configuration.
 *
 * @constructor Create a new Register File element model
 * @param name       The name identifier for this RF instance
 * @param moduleParams List containing:
 *                    - log2Regs: log2 of number of registers
 *                    - numIn: Number of input ports
 *                    - numOut: Number of output ports
 *                    - w: Data width
 */
class ElementRF(name: String, moduleParams: List[Int]) extends ElementTrait {

  // setTypeID(1) // Module ID 1

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
    val rule = () =>
      Module(new RegisterFile(moduleParams(0), moduleParams(1),
        moduleParams(2), moduleParams(3), name = getName()))
    rule
  }

  override val correlation = classOf[RegisterFile]
  setTypeID(ModuleRegistry.getID(this))
}

/**
 * Represents a Multiplexer (Mux) element in the CGRA architecture.
 *
 * The multiplexer provides configurable input selection with configurable data width.
 * It generates corresponding Chisel hardware module for the specified configuration.
 *
 * @constructor Create a new Multiplexer element model
 * @param name       The name identifier for this Mux instance
 * @param moduleParams List containing:
 *                    - numIn: Number of input ports
 *                    - w: Data width
 */
class ElementMux(name: String, moduleParams: List[Int]) extends ElementTrait {

  // setTypeID(2) // Module ID 2

  setSupOps(List())

  val configBits = log2Ceil(moduleParams(0))
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(1)

  /**
   * Generates a rule function that creates the Chisel hardware module for this Multiplexer.
   * @return Function that creates a Multiplexer module with configured parameters
   */
  def genModuleRule(): () => Multiplexer = {
    () => Module(new Multiplexer(
      moduleParams(0),  // Number of inputs
      moduleParams(1),  // Data width
      name = getName()
    ))
  }

  override val correlation = classOf[Multiplexer]
  setTypeID(ModuleRegistry.getID(this))
}

/**
 * Represents a Constant Unit element in the CGRA architecture.
 *
 * The constant unit generates fixed constant values with configurable data width.
 * It generates corresponding Chisel hardware module for the specified configuration.
 *
 * @constructor Create a new Constant Unit element model
 * @param name       The name identifier for this Const instance
 * @param moduleParams List containing:
 *                    - w: Data width
 */
class ElementConst(name: String, moduleParams: List[Int]) extends ElementTrait {

  // setTypeID(3) // Module ID 3

  setSupOps(List(OpEnum.CONST))

  // The configuration is the output of a const unit.
  val configBits = moduleParams(0)
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(1)

  /**
   * Generates a rule function that creates the Chisel hardware module for this Constant Unit.
   * @return Function that creates a ConstUnit module with configured parameters
   */
  def genModuleRule(): () => ConstUnit = {
    () => Module(new ConstUnit(
      moduleParams(0),  // Data width
      name = getName()
    ))
  }

  override val correlation = classOf[ConstUnit]
  setTypeID(ModuleRegistry.getID(this))
}

/**
 * Represents a Load/Store Unit (LSU) element in the CGRA architecture.
 *
 * The LSU handles memory access operations (load/store) with configurable data width.
 * It generates corresponding Chisel hardware module for the specified configuration.
 *
 * @constructor Create a new Load/Store Unit element model
 * @param name       The name identifier for this LSU instance
 * @param moduleParams List containing:
 *                    - w: Data width
 */
class ElementLSU(name: String, moduleParams: List[Int]) extends ElementTrait {

  // setTypeID(4) // Module ID 4

  setSupOps(List(OpEnum.LOAD, OpEnum.STORE))

  // 0 for load, 1 for store
  val configBits = 1
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(1)
  setMRRGMode(MEM_MODE)

  /**
   * Generates a rule function that creates the Chisel hardware module for this Load/Store Unit.
   * @return Function that creates a LoadStoreUnit module with configured parameters
   */
  def genModuleRule(): () => LoadStoreUnit = {
    () => Module(new LoadStoreUnit(
      moduleParams(0),  // Data width
      name = getName()
    ))
  }

  override val correlation = classOf[LoadStoreUnit]
  setTypeID(ModuleRegistry.getID(this))
}

/**
 * Represents a Counter element in the CGRA architecture.
 *
 * The counter provides configurable counting functionality with support for frequency,
 * end value, step size and initial value configuration. It generates corresponding
 * Chisel hardware module for the specified configuration.
 *
 * @constructor Create a new Counter element model
 * @param name       The name identifier for this Counter instance
 * @param moduleParams List containing:
 *                    - w: Data width
 */
class ElementCounter(name: String, moduleParams: List[Int]) extends ElementTrait {

  setSupOps(List(OpEnum.INCR))

  // The configuration of a counter consists of freq (interval cycles of value change), end (stop value),
  // step (value changes per interval), and init (initial value)..
  val configBits = moduleParams(0) * 4
  setParams(moduleParams :+ configBits)
  setName(name)

  addInternalNodesNum(1)

  /**
   * Generates a rule function that creates the Chisel hardware module for this Counter.
   * @return Function that creates a Counter module with configured parameters
   */
  def genModuleRule(): () => Counter = {
    () => Module(new Counter(
      moduleParams(0),  // Data width
      name = getName()
    ))
  }

  override val correlation = classOf[Counter]
  setTypeID(ModuleRegistry.getID(this))
}

// /** A block.
//  *
//  * @deprecated
//  * @param name      the name of the model
//  */
// class Block(name: String) extends BlockTrait {
//   setName(name)
//   hierarchyName.append(name)
// }