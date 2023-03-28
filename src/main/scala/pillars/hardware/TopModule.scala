package pillars.hardware

import chisel3.util._
import chisel3.{Bundle, Input, Module, Output, UInt, _}
import pillars.core.ModuleRegistry
import pillars.hardware.PillarsConfig._
import pillars.testers.AppTestHelper

import scala.collection.mutable.ArrayBuffer


/** A class containing the parameters of modules and the port number of top design.
 * It should be guaranteed that "configBits" is the last param of the parameters of a module.
 *
 * @param moduleNums     the number of modules for each type
 * @param params         the parameters of each module
 * @param inPortNum      the input port number of top design
 * @param outPortNum     the output port number of top design
 * @param genModuleRules the module generation rules
 */
class PillarsModuleInfo(moduleNums: List[Int], params: List[List[Int]],
                        inPortNum: Int, outPortNum: Int, genModuleRules: List[() => Module]) {
  /** Gets the module generation rules.
   */
  def getGenModuleRules(): List[() => Module] = {
    genModuleRules
  }

  /** Gets the number of modules for each type.
   */
  def getModuleNums(): List[Int] = {
    moduleNums
  }

  /** Gets the parameters of each module.
   */
  def getParams(num: Int): List[Int] = {
    params(num)
  }

  /** Gets the config bits of a module.
   *
   * @param typeID   the type of the module
   * @param moduleID the ID of the module
   */
  def getConfigBits(typeID: Int, moduleID: Int): Int = {
    var currentNum = 0
    for (i <- 0 until typeID) {
      currentNum += moduleNums(i)
    }
    currentNum += moduleID
    params(currentNum)(params(currentNum).length - 1)
  }

  /** Gets the sum of config bits of all modules.
   */
  def getTotalBits(): Int = {
    params.toArray.map(t => t(t.length - 1)).reduce(_ + _)
  }

  /** Gets the input port number of top design.
   */
  def getInPortNum: Int = {
    inPortNum
  }

  /** Gets the output port number of top design.
   */
  def getOutPortNum: Int = {
    outPortNum
  }
}

/** The top design of CGRA.
 * It should be guaranteed that the type ID of each element should be the same with its order in this class.
 *
 * @param moduleInfos a class containing the parameters of modules and the port number of top design.
 * @param connect     a map showing the connections between modules
 * @param regionList  the list of regions where modules share a configuration controller
 * @param w           the data width
 */
class TopModule(val moduleInfos: PillarsModuleInfo, val connect: Map[List[Int], List[List[Int]]],
                val regionList: List[List[List[Int]]], w: Int) extends Module {

  /** Generic IOs of basic module.
   */
  class GenericIO {
    var inputPorts: Array[List[Data]] = null
    var outPorts: Array[List[Data]] = null
    var configurationPorts: Array[Data] = null
  }

  /** Wrapper for ALUs. They can be fired by schedule controllers according to PillarsConfig.
   *
   * @param moduleArray     an array of alus
   * @param scheduleConfigs an array of dispatched schedule configurations
   * @return generic IOs of this type of modules
   */
  def wrapperForAlu(moduleArray: Array[Module], scheduleConfigs: Array[Data]): GenericIO = {
    val alus = moduleArray.map(m => m.asInstanceOf[Alu])
    if (USE_AUXILIARY_SCHEDULER) {
      val aluScheduleControllers = (0 until aluNum).toArray
        .map(_ => Module(new MultiIIScheduleController))
      for (i <- 0 until alus.size) {
        val alu = alus(i)
        val aluScheduleController = aluScheduleControllers(i)
        aluScheduleController.io.en <> io.en
        aluScheduleController.io.enConfig <> io.enConfig
        aluScheduleController.io.II <> io.II

        //        for (j <- 0 until II_UPPER_BOUND) {
        //          aluScheduleController.io.schedules(j) <> scheduleConfigs(i * II_UPPER_BOUND + j)
        //        }
        aluScheduleController.io.schedule <> scheduleConfigs(i)

        alu.io.en <> aluScheduleController.io.valid
        alu.io.skewing <> aluScheduleController.io.skewing
      }
    } else {
      for (alu <- alus) {
        alu.io.en <> io.en
        alu.io.skewing <> DontCare
      }
    }
    val ret = new GenericIO
    ret.inputPorts = alus.map(i => i.io.inputs.toList)
    ret.outPorts = alus.map(i => i.io.outs.toList)
    ret.configurationPorts = alus.map(i => i.io.configuration)
    ret
  }

  /** Wrapper for Counters. They can be fired by schedule controllers according to PillarsConfig.
   *
   * @param moduleArray     an array of counters
   * @param scheduleConfigs an array of dispatched schedule configurations
   * @return generic IOs of this type of modules
   */
  def wrapperForCounter(moduleArray: Array[Module], scheduleConfigs: Array[Data]): GenericIO = {
    val counters = moduleArray.map(m => m.asInstanceOf[Counter])
    if (USE_AUXILIARY_SCHEDULER) {
      val counterScheduleControllers = (0 until counterNum).toArray
        .map(_ => Module(new MultiIIScheduleController(DualInput = false)))
      for (i <- 0 until counters.size) {
        val counter = counters(i)
        val counterScheduleController = counterScheduleControllers(i)
        counterScheduleController.io.en <> io.en
        counterScheduleController.io.enConfig <> io.enConfig
        counterScheduleController.io.II <> io.II
        counterScheduleController.io.schedule <> scheduleConfigs(i)

        counter.io.en <> counterScheduleController.io.valid
        counter.io.II <> io.II
      }
    } else {
      for (counter <- counters) {
        counter.io.en <> io.en
        counter.io.II <> io.II
      }
    }

    val ret = new GenericIO
    ret.inputPorts = counters.map(i => i.io.inputs.toList)
    ret.outPorts = counters.map(i => i.io.outs.toList)
    ret.configurationPorts = counters.map(i => i.io.configuration)
    ret
  }

  /** Wrapper for basic modules extends default IOs.
   *
   * @param moduleArray an array of modules
   * @return generic IOs of this type of modules
   */
  def wrapperForDefaultModule(moduleArray: Array[Module]): GenericIO = {
    val modules = moduleArray.map(m => m.asInstanceOf[DefaultBasicModule])
    for (m <- modules) {
      m.io.en <> io.en
      m.io.II <> DontCare
    }
    val ret = new GenericIO
    ret.inputPorts = modules.map(i => i.io.inputs.toList)
    ret.outPorts = modules.map(i => i.io.outs.toList)
    ret.configurationPorts = modules.map(i => i.io.configuration)
    ret
  }

  /** Wrapper for Load/Store Units. They can be fired by schedule controllers according to PillarsConfig.
   * They are connected with some ports of top design to perform DMA.
   *
   * @param moduleArray     an array of lsus
   * @param scheduleConfigs an array of dispatched schedule configurations
   * @return generic IOs of this type of modules
   */
  def wrapperForLSU(moduleArray: Array[Module], scheduleConfigs: Array[Data]): GenericIO = {
    val lsus = moduleArray.map(m => m.asInstanceOf[LoadStoreUnit])
    for (i <- 0 until lsus.size) {
      val lsu = lsus(i)
      lsu.io.base <> io.baseLSU(i)
      lsu.io.len <> io.lenLSU(i)
      lsu.io.start <> io.startLSU(i)
      lsu.io.idle <> io.idleLSU(i)
      lsu.io.enqEn <> io.enqEnLSU(i)
      lsu.io.deqEn <> io.deqEnLSU(i)
      lsu.io.streamIn <> io.streamInLSU(i)
      lsu.io.streamOut <> io.streamOutLSU(i)
    }
    if (USE_AUXILIARY_SCHEDULER) {
      val LSUnitScheduleControllers = (0 until LSUnitNum).toArray
        .map(_ => Module(new MultiIIScheduleController))
      for (i <- 0 until LSUnitNum) {
        val lsu = lsus(i)

        val LSUnitScheduleController = LSUnitScheduleControllers(i)
        LSUnitScheduleController.io.en <> io.en
        LSUnitScheduleController.io.enConfig <> io.enConfig
        LSUnitScheduleController.io.II <> io.II
        //        for (j <- 0 until II_UPPER_BOUND) {
        //          LSUnitScheduleController.io.schedules(j) <> scheduleConfigs((i + aluNum) * II_UPPER_BOUND + j)
        //        }

        LSUnitScheduleController.io.schedule <> scheduleConfigs(i)

        lsu.io.en <> LSUnitScheduleController.io.valid
        lsu.io.II <> io.II
        lsu.io.skewing <> LSUnitScheduleController.io.skewing
      }
    } else {
      for (i <- 0 until LSUnitNum) {
        val lsu = lsus(i)
        lsu.io.en <> io.en
        lsu.io.II <> io.II
        lsu.io.skewing <> DontCare
      }
    }
    val ret = new GenericIO
    ret.inputPorts = lsus.map(i => i.io.inputs.toList)
    ret.outPorts = lsus.map(i => i.io.outs.toList)
    ret.configurationPorts = lsus.map(i => i.io.configuration)
    ret
  }

  /** Generate basic modules and get generic IOs of different types of modules.
   *
   * @param genModuleRules  a list of rules for generating basic modules
   * @param scheduleConfigs an array of dispatched schedule configurations
   * @return generic IOs of different types of modules
   */
  def getGenericIOs(genModuleRules: List[() => Module], scheduleConfigs: Array[Data]) = {
    val genericIOs = new Array[GenericIO](typeNum)

    var currentNum = 0
    for (i <- 0 until typeNum) {
      val moduleNum = moduleNums(i)
      val moduleClass = classOfModules(i)
      val modules = (0 until moduleNum).toArray.map(t => genModuleRules(t + currentNum).apply())

      val CAlu = classOf[Alu]
      val CLSU = classOf[LoadStoreUnit]
      val CCounter = classOf[Counter]

      genericIOs(i) = moduleClass match {
        case CAlu => wrapperForAlu(modules, scheduleConfigs.slice(0, aluNum))
        case CLSU => wrapperForLSU(modules, scheduleConfigs.slice(aluNum, aluNum + LSUnitNum))
        case CCounter => wrapperForCounter(modules,
          scheduleConfigs.slice(aluNum + LSUnitNum, aluNum + LSUnitNum + counterNum))
        case _ => wrapperForDefaultModule(modules)
      }
      currentNum += moduleNum
    }
    genericIOs
  }

  /** To control the cycle modules should fire, we employ the schedule controllers
   * to fire ALUs and LSUs when functional nodes are mapped onto them.
   *
   * @return an array of dispatched schedule configurations
   */
  def addScheduleControllers(): Array[Data] = {
    val scheduleDispatchPorts = new ArrayBuffer[Data]()
    if (USE_AUXILIARY_SCHEDULER) {
      //      val moduleScheduleBits = (0 until (aluNum + LSUnitNum) * II_UPPER_BOUND)
      //        .map(_ => LOG_SCHEDULE_SIZE + SKEW_WIDTH).toList
      val moduleScheduleBits = (0 until (aluNum + LSUnitNum))
        .map(_ => LOG_SCHEDULE_SIZE + SKEW_WIDTH).toList :::
        (0 until counterNum).map(_ => LOG_SCHEDULE_SIZE).toList
      val totalScheduleBits = moduleScheduleBits.reduce(_ + _)
      val scheduleDispatch = Module(new Dispatch(totalScheduleBits, moduleScheduleBits))
      scheduleDispatch.io.configuration := io.schedules
      scheduleDispatch.io.en := io.en
      for (i <- 0 until aluNum + LSUnitNum + counterNum) {
        //        for (i <- 0 until (aluNum + LSUnitNum) * II_UPPER_BOUND) {
        scheduleDispatchPorts.append(scheduleDispatch.io.outs(i))
      }
    }
    scheduleDispatchPorts.toArray
  }

  /** Dispatch configurations of modules in a configuration region
   * to corresponding configuration controllers, and connects them.
   *
   * @param scheduleConfigs an array of dispatched schedule configurations
   * @param genericIOs      generic IOs of different types of modules
   */
  def dispatchConfigs(scheduleConfigs: Array[Data], genericIOs: Array[GenericIO]): Unit = {
    val configControllers = ArrayBuffer[ConfigController]()
    var regionConfigBits = List[Int]()
    for (region <- regionList) {
      var configBits = List[Int]()
      var configPorts = List[Data]()
      for (moduleList <- region) {
        val typeID = moduleList(0)
        val moduleID = moduleList(1)
        configBits = configBits :+ moduleInfos.getConfigBits(typeID, moduleID)
        val configPort = genericIOs(typeID).configurationPorts(moduleID)
        configPorts = configPorts :+ configPort
      }
      val regionTotalBits = configBits.sum

      val configController = Module(new ConfigController(regionTotalBits))
      configController.io.II <> io.II
      configController.io.en <> io.enConfig

      regionConfigBits = regionConfigBits :+ regionTotalBits
      val dispatch = Module(new Dispatch(regionTotalBits, configBits))
      dispatch.io.en <> io.en
      for (i <- 0 until configBits.size) {
        configPorts(i) := dispatch.io.outs(i)
      }
      configControllers.append(configController)
      dispatch.io.configuration := configController.io.outConfig
    }
    val totalBits = regionConfigBits.sum
    val topDispatch = Module(new Dispatch(totalBits, regionConfigBits))
    topDispatch.io.en <> true.B
    topDispatch.io.configuration := io.configuration
    for (i <- 0 until configControllers.size) {
      configControllers(i).io.inConfig := topDispatch.io.outs(i)
    }
  }

  /** Add wires between modules.
   *
   * @param inPorts  a 3D array of input ports of basic modules
   * @param outPorts a 3D array of output ports of basic modules
   */
  def addConnection(inPorts: Array[Array[List[Data]]], outPorts: Array[Array[List[Data]]]): Unit = {
    for (i <- 0 until connect.keys.size) {
      val src = connect.keys.toList(i)
      val dsts = connect(src)
      for (j <- 0 until dsts.size) {
        val dst = dsts(j)
        if (dst(0) == typeNum) {
          io.outs(dst(2)) := outPorts(src(0))(src(1))(src(2)).asInstanceOf[Data]
        } else if (src(0) == typeNum) {
          inPorts(dst(0))(dst(1))(dst(2)).asInstanceOf[Data] := io.inputs(src(2))
        } else {
          inPorts(dst(0))(dst(1))(dst(2)).asInstanceOf[Data] := outPorts(src(0))(src(1))(src(2)).asInstanceOf[Data]
        }
      }
    }
  }

  /** Get the quantity of a module type
   *
   * @param MClass the class of this module type
   * @return the quantity of a module type
   */
  def getModuleNum(MClass: Class[_ <: Module]) = {
    val index = classOfModules.indexOf(MClass)
    val num = if (index >= 0) {
      moduleNums(index)
    } else {
      0
    }
    num
  }

  val moduleNums = moduleInfos.getModuleNums()
  val typeNum = moduleNums.size
  val classOfModules = ModuleRegistry.classOfModules

  val aluNum = getModuleNum(classOf[Alu])
  val counterNum = getModuleNum(classOf[Counter])
  val LSUnitNum = getModuleNum(classOf[LoadStoreUnit])

  //  val scheduleWidth = ((aluNum + LSUnitNum) * II_UPPER_BOUND) *
  //    (LOG_SCHEDULE_SIZE + SKEW_WIDTH)

  val scheduleWidth = (aluNum + LSUnitNum) *
    (LOG_SCHEDULE_SIZE + SKEW_WIDTH) + counterNum * LOG_SCHEDULE_SIZE

  val io = IO(new Bundle {
    val streamInLSU = MixedVec((0 until LSUnitNum).map(_ => Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))))
    val streamOutLSU = MixedVec((0 until LSUnitNum).map(_ => Flipped(DeqIO(UInt(MEM_OUT_WIDTH.W)))))
    val baseLSU = Input(Vec(LSUnitNum, UInt(log2Ceil(MEM_DEPTH).W)))
    val lenLSU = Input(Vec(LSUnitNum, UInt(log2Ceil(MEM_DEPTH).W)))

    val startLSU = Input(Vec(LSUnitNum, Bool()))
    val enqEnLSU = Input(Vec(LSUnitNum, Bool()))
    val deqEnLSU = Input(Vec(LSUnitNum, Bool()))
    val idleLSU = Output(Vec(LSUnitNum, Bool()))

    val enConfig = Input(Bool())
    val en = Input(Bool())
    val schedules = Input(UInt(scheduleWidth.W))
    val II = Input(UInt(LOG_II_UPPER_BOUND.W))

    val configuration = Input(UInt(moduleInfos.getTotalBits().W))

    val inputs = Input(MixedVec((1 to moduleInfos.getInPortNum) map { _ => getClassIO(w) }))
    val outs = Output(MixedVec((1 to moduleInfos.getOutPortNum) map { _ => getClassIO(w) }))
  })

  val genModuleRules = moduleInfos.getGenModuleRules()
  val scheduleConfigs = addScheduleControllers()
  val genericIOs = getGenericIOs(genModuleRules, scheduleConfigs)
  dispatchConfigs(scheduleConfigs, genericIOs)

  val outPorts = genericIOs.map(x => x.outPorts)
  val inPorts = genericIOs.map(x => x.inputPorts)
  addConnection(inPorts, outPorts)

  //For tests.
  val out = io.outs(0)

}


/** Makeshift facilities for context transmission within the limitation of I/O ports in FPGA.
 *
 * @param moduleInfos a class containing the parameters of modules and the port number of top design.
 * @param connect     a map showing the connections between modules
 * @param regionList  the list of regions where modules share a configuration controller
 * @param w           the data width
 */
class TopModuleWrapper(val moduleInfos: PillarsModuleInfo, val connect: Map[List[Int], List[List[Int]]],
                       val regionList: List[List[List[Int]]], w: Int)
  extends Module {

  val topModule = Module(new TopModule(moduleInfos, connect, regionList, w))
  val LSUnitNum = topModule.LSUnitNum

  val io = IO(new Bundle {
    val streamInLSU = Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))
    val streamOutLSU = Flipped(DeqIO(UInt(MEM_OUT_WIDTH.W)))
    val baseLSU = Input(UInt(log2Ceil(MEM_DEPTH).W))
    val lenLSU = Input(UInt(log2Ceil(MEM_DEPTH).W))

    val startLSU = Input(Bool())
    val enqEnLSU = Input(Bool())
    val deqEnLSU = Input(Bool())
    val idleLSU = Output(Bool())

    val LSUnitID = Input(UInt(log2Up(LSUnitNum).W))

    val en = Input(Bool())
    val enConfig = Input(Bool())
    val II = Input(UInt(LOG_II_UPPER_BOUND.W))

    val singleBitConfig = Input(UInt(1.W))
    val singleBitSchedule = Input(UInt(1.W))

    val inputs = Input(MixedVec((1 to topModule.moduleInfos.getInPortNum) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to topModule.moduleInfos.getOutPortNum) map { i => UInt(w.W) }))
  })

  topModule.io.en <> io.en
  topModule.io.II <> io.II

  /** Connects a LSU with ports of the wrapper.
   *
   * @param i the module ID of LSU
   */
  def connectLSU(i: Int): Unit = {
    topModule.io.baseLSU(i.U) <> io.baseLSU
    topModule.io.lenLSU(i.U) <> io.lenLSU
    topModule.io.startLSU(i.U) <> io.startLSU
    topModule.io.enqEnLSU(i.U) <> io.enqEnLSU
    topModule.io.deqEnLSU(i.U) <> io.deqEnLSU
    topModule.io.streamOutLSU(i) <> io.streamOutLSU
    topModule.io.streamInLSU(i) <> io.streamInLSU
    for (j <- 0 until LSUnitNum) {
      if (j != i) {
        topModule.io.baseLSU(j.U) <> DontCare
        topModule.io.lenLSU(j.U) <> DontCare
        topModule.io.startLSU(j.U) <> DontCare
        topModule.io.enqEnLSU(j.U) <> DontCare
        topModule.io.deqEnLSU(j.U) <> DontCare
        topModule.io.streamOutLSU(j) <> DontCare
        topModule.io.streamInLSU(j).valid := false.B
        topModule.io.streamInLSU(j).bits := 0.U
      }
    }
  }

  //connect LSU with ports according to the input "LSUintID"
  when(io.LSUnitID === 0.U) {
    connectLSU(0)
  }.elsewhen(io.LSUnitID === 1.U) {
    connectLSU(1)
  }.elsewhen(io.LSUnitID === 2.U) {
    connectLSU(2)
  }.otherwise {
    connectLSU(3)
  }

  topModule.io.idleLSU(io.LSUnitID) <> io.idleLSU

  topModule.io.inputs <> io.inputs
  topModule.io.outs <> io.outs

  val cycleReg = RegInit(0.U(LOG_II_UPPER_BOUND.W))

  val maxSize = Math.max(topModule.io.configuration.getWidth, topModule.io.schedules.getWidth)
  val posSize = log2Up(maxSize)
  val posReg = RegInit(0.U(posSize.W))


  val scheduleSize = topModule.io.schedules.getWidth
  val configSize = topModule.io.configuration.getWidth
  /** Register of schedule.
   */
  val scheduleReg = RegInit(0.U(scheduleSize.W))
  /** Register of configuration.
   */
  val bitStreamsReg = RegInit(0.U(configSize.W))

  val s_wait :: s_input_config :: s_work :: Nil = Enum(3)
  val state = RegInit(s_wait)


  topModule.io.schedules := scheduleReg
  topModule.io.configuration := bitStreamsReg
  topModule.io.enConfig := io.enConfig

  //Waits inputs.
  when(state === s_wait) {
    //Stores 1 bit into the registers every cycle.
    when(!(posReg === (maxSize).U)) {
      posReg := posReg + 1.U
    }
    when(posReg < scheduleSize.U) {
      scheduleReg := scheduleReg * 2.U + io.singleBitSchedule
    }
    when(posReg < configSize.U) {
      bitStreamsReg := bitStreamsReg * 2.U + io.singleBitConfig
    }
    when(io.en === true.B) {
      state := s_input_config
      cycleReg := cycleReg + 1.U
    }
  }.elsewhen(state === s_input_config) {
    //Peeks configurations and schedules into the top design.
    when(cycleReg === io.II - 1.U) {
      state := s_work
    }.otherwise {
      cycleReg := cycleReg + 1.U
    }
  }.otherwise {
    when(io.en === false.B) {
      state := s_wait
    }
  }

}

/** A wrapper with solid configurations and schedules.
 *
 * @param moduleInfos a class containing the parameters of modules and the port number of top design.
 * @param connect     a map showing the connections between modules
 * @param regionList  the list of regions where modules share a configuration controller
 * @param w           the data width
 */
class TopModuleWrapperSolid(val moduleInfos: PillarsModuleInfo, val connect: Map[List[Int], List[List[Int]]],
                            val regionList: List[List[List[Int]]], w: Int, appTestHelper: AppTestHelper)
  extends Module {

  val topModule = Module(new TopModule(moduleInfos, connect, regionList, w))
  val LSUnitNum = topModule.LSUnitNum

  val io = IO(new Bundle {
    val streamInLSU = Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))
    val streamOutLSU = Flipped(DeqIO(UInt(MEM_OUT_WIDTH.W)))
    val baseLSU = Input(UInt(log2Ceil(MEM_DEPTH).W))
    val lenLSU = Input(UInt(log2Ceil(MEM_DEPTH).W))

    val startLSU = Input(Bool())
    val enqEnLSU = Input(Bool())
    val deqEnLSU = Input(Bool())
    val idleLSU = Output(Bool())

    val LSUnitID = Input(UInt(log2Up(LSUnitNum).W))

    val en = Input(Bool())
    val II = Input(UInt(LOG_II_UPPER_BOUND.W))

    val inputs = Input(MixedVec((1 to topModule.moduleInfos.getInPortNum) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to topModule.moduleInfos.getOutPortNum) map { i => UInt(w.W) }))
  })

  topModule.io.en <> io.en
  topModule.io.II <> io.II

  /** Connects a LSU with ports of the wrapper.
   *
   * @param i the module ID of LSU
   */
  def connectLSU(i: Int): Unit = {
    topModule.io.baseLSU(i.U) <> io.baseLSU
    topModule.io.lenLSU(i.U) <> io.lenLSU
    topModule.io.startLSU(i.U) <> io.startLSU
    topModule.io.enqEnLSU(i.U) <> io.enqEnLSU
    topModule.io.deqEnLSU(i.U) <> io.deqEnLSU
    topModule.io.streamOutLSU(i) <> io.streamOutLSU
    topModule.io.streamInLSU(i) <> io.streamInLSU
    for (j <- 0 until LSUnitNum) {
      if (j != i) {
        topModule.io.baseLSU(j.U) <> DontCare
        topModule.io.lenLSU(j.U) <> DontCare
        topModule.io.startLSU(j.U) <> DontCare
        topModule.io.enqEnLSU(j.U) <> DontCare
        topModule.io.deqEnLSU(j.U) <> DontCare
        topModule.io.streamOutLSU(j) <> DontCare
        topModule.io.streamInLSU(j).valid := false.B
        topModule.io.streamInLSU(j).bits := 0.U
      }
    }
  }

  when(io.LSUnitID === 0.U) {
    connectLSU(0)
  }.elsewhen(io.LSUnitID === 1.U) {
    connectLSU(1)
  }.elsewhen(io.LSUnitID === 2.U) {
    connectLSU(2)
  }.otherwise {
    connectLSU(3)
  }

  topModule.io.idleLSU(io.LSUnitID) <> io.idleLSU

  topModule.io.inputs <> io.inputs
  topModule.io.outs <> io.outs

  val cycleReg = RegInit(0.U(LOG_II_UPPER_BOUND.W))

  val schedules = appTestHelper.getSchedulesBigInt()
  val bitStreams = appTestHelper.getBitStreams()

  val schedulesInit = schedules.map(t => t.U(topModule.io.schedules.getWidth.W))
  val bitStreamsInit = bitStreams.map(t => t.U(topModule.io.schedules.getWidth.W))

  val scheduleROM = VecInit(schedulesInit)
  val bitStreamsROM = VecInit(bitStreamsInit)

  val s_wait :: s_input_config :: s_work :: Nil = Enum(3)
  val state = RegInit(s_wait)

  topModule.io.schedules := scheduleROM(cycleReg)
  topModule.io.configuration := bitStreamsROM(cycleReg)
  topModule.io.enConfig := true.B

  when(state === s_wait) {
    when(io.en === true.B) {
      state := s_input_config
      cycleReg := cycleReg + 1.U
    }
  }.elsewhen(state === s_input_config) {
    when(cycleReg === io.II - 1.U) {
      state := s_work
    }.otherwise {
      cycleReg := cycleReg + 1.U
    }
  }.otherwise {
    when(io.en === false.B) {
      state := s_wait
    }
  }

}
