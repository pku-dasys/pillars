package tetriski.pillars.hardware

import chisel3.util._
import chisel3.{Bundle, Input, Module, Output, UInt, _}

import scala.collection.mutable.ArrayBuffer
import tetriski.pillars.hardware.PillarsConfig._
import tetriski.pillars.testers.AppTestHelper


/** A class containing the parameters of modules and the port number of top design.
 * It should be guaranteed that "configBits" is the last param of the parameters of a module.
 *
 * @param moduleNums the number of modules for each type
 * @param params     the parameters of each module
 * @param inPortNum  the input port number of top design
 * @param outPortNum the output port number of top design
 */
class PillarsModuleInfo(moduleNums: List[Int], params: List[List[Int]], inPortNum: Int, outPortNum: Int) {
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


  val moduleNums = moduleInfos.getModuleNums()
  val aluNum = moduleNums(0)
  val RFNum = moduleNums(1)
  val MuxNum = moduleNums(2)
  val ConstNum = moduleNums(3)
  val LSUnitNum = moduleNums(4)

  val io = IO(new Bundle {
    val streamInLSU = MixedVec((0 until LSUnitNum).map(p => Flipped(EnqIO(UInt(MEM_IN_WIDTH.W)))))
    val streamOutLSU = MixedVec((0 until LSUnitNum).map(p => Flipped(DeqIO(UInt(MEM_OUT_WIDTH.W)))))
    val baseLSU = Input(Vec(LSUnitNum, UInt(log2Ceil(MEM_DEPTH).W)))
    val lenLSU = Input(Vec(LSUnitNum, UInt(log2Ceil(MEM_DEPTH).W)))

    val startLSU = Input(Vec(LSUnitNum, Bool()))
    val enqEnLSU = Input(Vec(LSUnitNum, Bool()))
    val deqEnLSU = Input(Vec(LSUnitNum, Bool()))
    val idleLSU = Output(Vec(LSUnitNum, Bool()))

    val enConfig = Input(Bool())
    val en = Input(Bool())
    val schedules = Input(UInt((((aluNum + LSUnitNum) * II_UPPER_BOUND) *
      (LOG_SCHEDULE_SIZE + LOG_SCHEDULE_SIZE + 1)).W))
    val II = Input(UInt(LOG_II_UPPER_BOUND.W))

    val configuration = Input(UInt(moduleInfos.getTotalBits().W))

    val inputs = Input(MixedVec((1 to moduleInfos.getInPortNum) map { i => UInt(w.W) }))
    val outs = Output(MixedVec((1 to moduleInfos.getOutPortNum) map { i => UInt(w.W) }))
  })

  val types = moduleNums.size
  var currentNum = 0

  val out = io.outs(0)


  //To control the cycle modules should fire, we employ the schedule controllers
  //to fire ALUs and LSUs when functional nodes are mapped onto them.
  val moduleScheduleBits = (0 until aluNum * II_UPPER_BOUND)
    .map(i => LOG_SCHEDULE_SIZE * 2 + 1).toList ::: (0 until LSUnitNum * II_UPPER_BOUND)
    .map(i => LOG_SCHEDULE_SIZE * 2 + 1).toList
  val totalScheduleBits = moduleScheduleBits.reduce(_ + _)
  val scheduleDispatch = Module(new Dispatch(totalScheduleBits, moduleScheduleBits))
  scheduleDispatch.io.configuration := io.schedules
  scheduleDispatch.io.en := io.en

  //ALUs have type ID = 0, and they will be fired by schedule controllers.
  val alus = (0 until aluNum).toArray.map(t => Module(new Alu(moduleInfos.getParams(t + currentNum)(0),
    moduleInfos.getParams(t + currentNum)(1))))
  val aluScheduleControllers = (0 until aluNum).toArray.map(t => Module(new MultiIIScheduleController))
  for (i <- 0 until aluNum) {
    val alu = alus(i)
    val aluScheduleController = aluScheduleControllers(i)
    aluScheduleController.io.en <> io.en
    aluScheduleController.io.II <> io.II
    for (j <- 0 until II_UPPER_BOUND) {
      aluScheduleController.io.schedules(j) <> scheduleDispatch.io.outs(i * II_UPPER_BOUND + j)
    }
    alu.io.en <> aluScheduleController.io.valid
    alu.io.skewing <> aluScheduleController.io.skewing
  }
  currentNum += aluNum

  //RFs have type ID = 1.
  val RFs = (0 until RFNum).toArray
    .map(t => Module(new RegisterFile(moduleInfos.getParams(t + currentNum)(0),
      moduleInfos.getParams(t + currentNum)(1),
      moduleInfos.getParams(t + currentNum)(2),
      moduleInfos.getParams(t + currentNum)(3))))
  for (rf <- RFs) {
    rf.io.en <> io.en
  }
  currentNum += RFNum

  //Muxs have type ID = 2.
  val Muxs = (0 until MuxNum).toArray.map(t =>
    Module(new Multiplexer(moduleInfos.getParams(t + currentNum)(0), moduleInfos.getParams(t + currentNum)(1))))
  for (mux <- Muxs) {
    mux.io.en <> io.en
  }
  currentNum += MuxNum

  //Const units have type ID = 3.
  val Consts = (0 until ConstNum).toArray
    .map(t => Module(new ConstUnit(moduleInfos.getParams(t + currentNum)(0))))
  for (const <- Consts) {
    const.io.en <> io.en
  }
  currentNum += ConstNum

  //LSUs have type ID = 4, and they will be fired by schedule controllers.
  //They are connected with some ports of top design to perform DMA.
  val LSUs = (0 until LSUnitNum).toArray
    .map(t => Module(new LoadStoreUnit(moduleInfos.getParams(t + currentNum)(0))))
  val LSUnitScheduleControllers = (0 until LSUnitNum).toArray.map(t => Module(new MultiIIScheduleController))
  for (i <- 0 until LSUnitNum) {
    val lsu = LSUs(i)
    lsu.io.base <> io.baseLSU(i)
    lsu.io.len <> io.lenLSU(i)
    lsu.io.start <> io.startLSU(i)
    lsu.io.idle <> io.idleLSU(i)
    lsu.io.enqEn <> io.enqEnLSU(i)
    lsu.io.deqEn <> io.deqEnLSU(i)
    lsu.io.streamIn <> io.streamInLSU(i)
    lsu.io.streamOut <> io.streamOutLSU(i)

    val LSUnitScheduleController = LSUnitScheduleControllers(i)
    LSUnitScheduleController.io.en <> io.en
    LSUnitScheduleController.io.II <> io.II
    for (j <- 0 until II_UPPER_BOUND) {
      LSUnitScheduleController.io.schedules(j) <> scheduleDispatch.io.outs((i + aluNum) * II_UPPER_BOUND + j)
    }
    lsu.io.en <> LSUnitScheduleController.io.valid
    lsu.io.skewing <> LSUnitScheduleController.io.skewing
  }
  currentNum += LSUnitNum

  val modules = List(alus, RFs, Muxs, Consts, LSUs)


  val outPorts = new ArrayBuffer[Array[List[Any]]]
  outPorts.append(alus.map(i => i.io.outs.toList))
  outPorts.append(RFs.map(i => i.io.outs.toList))
  outPorts.append(Muxs.map(i => i.io.outs.toList))
  outPorts.append(Consts.map(i => i.io.outs.toList))
  outPorts.append(LSUs.map(i => i.io.outs.toList))

  val inPorts = new ArrayBuffer[Array[List[Any]]]
  inPorts.append(alus.map(i => i.io.inputs.toList))
  inPorts.append(RFs.map(i => i.io.inputs.toList))
  inPorts.append(Muxs.map(i => i.io.inputs.toList))
  inPorts.append(Consts.map(i => List()))
  inPorts.append(LSUs.map(i => i.io.inputs.toList))


  //Dispatchs configurations of modules in a configuration region
  // to corresponding configuration controllers, and connects them.
  var configControllers = ArrayBuffer[ConfigController]()
  var regionConfigBits = List[Int]()
  for (region <- regionList) {
    var configBits = List[Int]()
    var configPorts = List[Data]()
    for (moduleList <- region) {
      val typeID = moduleList(0)
      val moduleID = moduleList(1)
      configBits = configBits :+ moduleInfos.getConfigBits(typeID, moduleID)
      val configPort = typeID match {
        case 0 => alus(moduleID).io.configuration
        case 1 => RFs(moduleID).io.configuration
        case 2 => Muxs(moduleID).io.configuration
        case 3 => Consts(moduleID).io.configuration
        case 4 => LSUs(moduleID).io.configuration
      }
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


  //AddS wires between modules.
  for (i <- 0 until connect.keys.size) {
    val src = connect.keys.toList(i)
    val dsts = connect(src)
    for (j <- 0 until dsts.size) {
      val dst = dsts(j)
      if (dst(0) == types) {
        io.outs(dst(2)) := outPorts(src(0))(src(1))(src(2)).asInstanceOf[Data]
      } else if (src(0) == types) {
        inPorts(dst(0))(dst(1))(dst(2)).asInstanceOf[Data] := io.inputs(src(2))
      } else {
        inPorts(dst(0))(dst(1))(dst(2)).asInstanceOf[Data] := outPorts(src(0))(src(1))(src(2)).asInstanceOf[Data]
      }
    }
  }

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
 * Some may call it "soft CGRA", which is used for quick compiler.
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

  val schedulesInit = schedules.U(topModule.io.schedules.getWidth.W)
  val bitStreamsInit = bitStreams.map(t => t.U(topModule.io.schedules.getWidth.W))

  val scheduleROM = VecInit(schedulesInit)
  val bitStreamsROM = VecInit(bitStreamsInit)

  val s_wait :: s_input_config :: s_work :: Nil = Enum(3)
  val state = RegInit(s_wait)

  topModule.io.schedules := scheduleROM(0)
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
