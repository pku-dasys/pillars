package tetriski.pillars.archlib

import chisel3.util.log2Up
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{BlockTrait, ElementTrait, OpEnum}

import scala.collection.mutable.LinkedHashMap


/**
 * Parsed PE block with non-hardcoded values
 * @param name
 * @param useMuxBypass
 * @param opList
 * @param aluSupBypass
 * @param isMemPE
 * @param inPortsNeighbor
 * @param dataWidth
 */
class Parsed_PEBlock_Test(name: String, useMuxBypass: Boolean, opList: List[OpEnum] = null,
                     aluSupBypass: Boolean = true, isMemPE: Boolean =true, inPortsNeighbor: Array[String] = null,
                     dataWidth: Int = 32) extends BlockTrait {
  initName(name)
  //Set configuration region.
  setConfigRegion()

  addOutPorts(Array("out_w", "out_e", "out_n", "out_s"))
  addInPorts(inPortsNeighbor)

  val connections : LinkedHashMap[String, Array[String]] = LinkedHashMap(
    "THIS.WEST_I" -> Array("FU0.DP0_I1","FU0.DP0_I2","FU0.DP0_P", "RF0.WP0", "RF0.WP1"),
    "THIS.EAST_I" -> Array("FU0.DP0_I1","FU0.DP0_I2","FU0.DP0_P", "RF0.WP0", "RF0.WP1"),
    "THIS.NORTH_I" -> Array("FU0.DP0_I1","FU0.DP0_I2","FU0.DP0_P", "RF0.WP0", "RF0.WP1"),
    "THIS.SOUTH_I" -> Array("FU0.DP0_I1","FU0.DP0_I2","FU0.DP0_P", "RF0.WP0", "RF0.WP1"),
    "RF0.RP0" -> Array("FU0.DP0_I1","FU0.DP0_I2","FU0.DP0_P","THIS.NORTH_O","THIS.EAST_O","THIS.WEST_O","THIS.SOUTH_O"),
    "RF0.RP1" -> Array("FU0.DP0_I1","FU0.DP0_I2","FU0.DP0_P","THIS.NORTH_O","THIS.EAST_O","THIS.WEST_O","THIS.SOUTH_O"),
    "FU0.DP0_T" -> Array("RF0.WP0","RF0.WP1","THIS.NORTH_O","THIS.EAST_O","THIS.WEST_O","THIS.SOUTH_O")
  )
  val bigCaseIOToSmallCaseMap : LinkedHashMap[String, String] = LinkedHashMap(
    "NORTH_I" -> "input_n",
    "EAST_I" -> "input_e",
    "WEST_I" -> "input_w",
    "SOUTH_I" -> "input_s",
    "NORTH_O" -> "out_n",
    "EAST_O" -> "out_e",
    "WEST_O" -> "out_w",
    "SOUTH_O" -> "out_s",
  )
  val portNameToMuxNameMap : LinkedHashMap[String, String] = LinkedHashMap(
    "DP0_I1" -> "muxI1",
    "DP0_I2" -> "muxI2",
    "DP0_P" -> "muxP",
    "WP0" -> "muxWP0",
    "WP1" -> "muxWP1",
    "NORTH_O" -> "muxNO",
    "EAST_O" -> "muxEO",
    "WEST_O" -> "muxWO",
    "SOUTH_O" -> "muxSO",
  )
  val submods : LinkedHashMap[String, Array[String]] =
    if (isMemPE) {
      LinkedHashMap (
        "FU_MEM" -> Array("FU0"),
        "RF" -> Array("RF0"),
      )
    } else {
      LinkedHashMap (
        "FU" -> Array("FU0"),
        "RF" -> Array("RF0"),
      )
    }

  var aluCount = 0
  val aluInPorts = Array("DP0_I1", "DP0_I2", "DP0_P")
  val aluOutPort = "DP0_T"

  var rfCount = 0
  val rfInPorts = Array("WP0", "WP1")
  val rfOutPorts = Array("RP0", "RP1")
  val rfRegs = Array("R0", "R1", "R2", "R3")
  val rfConnections : LinkedHashMap[String, Array[String]] = LinkedHashMap(
    "THIS.WP0" -> Array("THIS.R0","THIS.R1","THIS.R2","THIS.R3"),
    "THIS.WP1" -> Array("THIS.R0","THIS.R1","THIS.R2","THIS.R3"),
    "THIS.R0" -> Array("THIS.RP0","THIS.RP1"),
    "THIS.R1" -> Array("THIS.RP0","THIS.RP1"),
    "THIS.R2" -> Array("THIS.RP0","THIS.RP1"),
    "THIS.R3" -> Array("THIS.RP0","THIS.RP1")
  )

  var rfs : LinkedHashMap[String, ElementRF] = LinkedHashMap()
  var portToInConnections : LinkedHashMap[String, Array[String]] = LinkedHashMap()
  var portToMux : LinkedHashMap[String, ElementMux] = LinkedHashMap()
  val aluOpList =
    if (opList == null) {
      //default
      List(OpEnum.ADD, OpEnum.MUL)
    }
    else {
      opList
    }

  var fusInPortsToRFMap : LinkedHashMap[String, (LinkedHashMap[String, ElementRF], ElementTrait, String)] = LinkedHashMap()
  var fuMemsInPortsToRFMap : LinkedHashMap[String, (LinkedHashMap[String, ElementRF], ElementTrait, String)] = LinkedHashMap()

  if (submods.contains("FU")) {
    for (fu <- submods("FU")) {
      val (tmpInPortToRFMap, outComponent, outComponentOutPort) =
        addFu(aluInPorts, aluOutPort, aluOpList, aluSupBypass, false, dataWidth)

      fusInPortsToRFMap += (fu -> (tmpInPortToRFMap, outComponent, outComponentOutPort))
    }
  }

  if (submods.contains("FU_MEM")) {
    for (fuMem <- submods("FU_MEM")) {
      val (tmpInPortToRFMap, outComponent, outComponentOutPort) =
        addFu(aluInPorts, aluOutPort, aluOpList, aluSupBypass, true, dataWidth)

      fuMemsInPortsToRFMap += (fuMem -> (tmpInPortToRFMap, outComponent, outComponentOutPort))

    }
  }

  if (submods.contains("RF")) {
    for (rf <- submods("RF")) {
      val rf0 = new ElementRF(s"rf$rfCount", List(3, 2, 2, dataWidth))
      addElement(rf0)
      rfCount = rfCount + 1

      rf0.addOutPorts(rfOutPorts)
      rf0.addInPorts(rfInPorts)

      rfs += (rf -> rf0)
    }
  }

  // populate portToInConnections
  for ((src, dsts) <- connections) {
    for (dst <- dsts) {
      if (!portToInConnections.contains(dst)) {
        portToInConnections += (dst -> new Array[String](0))
      }

      val newVal : Array[String] = {
        if (src.endsWith("DP0_T")) {
          if (dst.startsWith("THIS")) {
            Array(src) ++ portToInConnections(dst)
          } else {
            portToInConnections(dst) :+ src
          }
        } else {
          portToInConnections(dst) :+ src
        }
      }
      portToInConnections += (dst -> newVal)
    }
  }

//  println("portToInConnections: ")
  portToInConnections.foreach(x => {
//    println("port: ", x._1)
//    println(x._2.mkString(", "))
  })

  // create muxes and add mux -> dst connection
  for ((port, inConnections) <- portToInConnections) {
    val portSplit = port.split("\\.")

    val comp = portSplit(0)
    val name = portSplit(1)

    if (!portNameToMuxNameMap.contains(name)) {
      throw new Exception(s"unexpected port name $name")
    }

    // TODO: remove hack
    val extraNeighbors =
      if (port == "FU0.DP0_I1") {
        1
      } else {
        0
      }

    val mux = new ElementMux(portNameToMuxNameMap(name), List(inConnections.size + extraNeighbors, dataWidth))
    addElement(mux)
    mux.addOutPorts(Array("out_0"))
    mux.addInPorts((0 until inConnections.size + extraNeighbors).map(i => s"input_$i").toArray)

    portToMux += (port -> mux)

    if (comp == "THIS") {
      if (!bigCaseIOToSmallCaseMap.contains(name)) {
        throw new Exception(s"unexpected name $name")
      }

      val realName = bigCaseIOToSmallCaseMap(name)
      addConnect(mux / "out_0" -> term(realName))
    } else {
      if (fusInPortsToRFMap.contains(comp)) {
        // connect to rf that is connected to alu
        val (tmpInPortToRFMap, _, _) = fusInPortsToRFMap(comp)

        if (tmpInPortToRFMap.contains(name)) {
          addConnect(mux / "out_0" -> tmpInPortToRFMap(name) / "input_0")
        } else {
          throw new Exception(s"tmpInPortsToRFMap for $comp not contains $name")
        }
      } else if (fuMemsInPortsToRFMap.contains(comp)) {
        val (tmpInPortToRFMap, _, _) = fuMemsInPortsToRFMap(comp)

        if (tmpInPortToRFMap.contains(name)) {
          addConnect(mux / "out_0" -> tmpInPortToRFMap(name) / "input_0")
        } else {
          throw new Exception(s"tmpInPortsToRFMap for $comp not contains $name")
        }
      } else if (rfs.contains(comp)) {
        addConnect(mux / "out_0" -> rfs(comp) / name)
      } else {
        throw new Exception("invalid port name in connections: " + port)
      }
    }
  }

  // add src -> mux (-> dst) connection
  for ((src, dsts) <- connections) {
    for (dst <- dsts) {
      val dstMux = portToMux(dst)

      val idx = portToInConnections(dst).indexOf(src)

      val portSplit = src.split("\\.")
      val comp = portSplit(0)
      val name = portSplit(1)
      if (comp == "THIS") {
        if (!bigCaseIOToSmallCaseMap.contains(name)) {
          throw new Exception(s"unexpected name $name")
        }

        val realName = bigCaseIOToSmallCaseMap(name)
        addConnect(term(realName) -> dstMux / ("input_" + idx))
      } else {
        if (fusInPortsToRFMap.contains(comp)) {
          val (_, outComponent, outComponentOutPort) = fusInPortsToRFMap(comp)
          addConnect(outComponent / outComponentOutPort -> dstMux / ("input_" + idx))
        } else if (fuMemsInPortsToRFMap.contains(comp)) {
          val (_, outComponent, outComponentOutPort) = fuMemsInPortsToRFMap(comp)
          addConnect(outComponent / outComponentOutPort -> dstMux / ("input_" + idx))
        } else if (rfs.contains(comp)) {
          addConnect(rfs(comp) / name -> dstMux / ("input_" + idx))
        } else {
          throw new Exception("invalid port name in connections: " + src)
        }
      }
    }

  }
  // TODO: remove hack
  doHacks()

  /**
   * Function for connecting ALU_OUT to muxI1.
   */
  def doHacks(): Unit = {
    // hack? add connection from ALU_OUT to muxI1
    val (_, aluOutComponent, aluOutComponentOutputPort) =
      if (isMemPE) {
        fuMemsInPortsToRFMap("FU0")
      } else {
        fusInPortsToRFMap("FU0")
      }

    val lastInputIdx = portToInConnections("FU0.DP0_I1").size
    addConnect(aluOutComponent / aluOutComponentOutputPort -> portToMux("FU0.DP0_I1") / s"input_$lastInputIdx")
  }

  /**
   * Function to add either a plain FU or mem FU
   * Returns a 3-tuple of
   * 1. Map of inPort to the RF that is connected to it
   * 2. The component which contains the outPort of the FU
   * 3. The name of the outPort on the outComponent
   * @param fuInPorts
   * @param fuOutPort
   * @param aluOpList
   * @param aluSupBypass
   * @param isMemFU
   * @param dataWidth
   * @return
   */
  def addFu(fuInPorts: Array[String], fuOutPort: String, aluOpList: List[OpEnum], aluSupBypass: Boolean,
            isMemFU: Boolean, dataWidth: Int) : (LinkedHashMap[String, ElementRF], ElementTrait, String) = {
    val constInPort = "input_const"

    /** An ALU that can perform some operations.
     * TODO: changes for predicate support??
     */
    val dp0 = new ElementAluGN(s"alu$aluCount", this.getHierarchyName()(0).toString() + "_" + s"alu$aluCount",aluOpList, aluSupBypass, List(dataWidth))
    addElement(dp0)
    aluCount = aluCount + 1

    dp0.addInPorts(Array("DP0_I1", "DP0_I2", "input_const", "DP0_P"))
    // dp0.addInPorts(fuInPorts.slice(0, fuInPorts.size - 1) :+ constInPort :+ fuInPorts.last)
    dp0.addOutPorts(Array(fuOutPort))

    val inPortToRFMap : LinkedHashMap[String, ElementRF] = LinkedHashMap()

    for (inPort <- fuInPorts) {
      val rf = new ElementRF("rf" + inPort, List(0, 1, 1, dataWidth))
      addElement(rf)

      rf.addOutPorts(Array("out_0"))
      rf.addInPorts(Array("input_0"))

      // rf -> dp
      addConnect(rf / "out_0" -> dp0 / inPort)

      inPortToRFMap += (inPort -> rf)
    }

    /** A const unit connected to the multiplexers.
     */
    val const0 = new ElementConst("const0", List(dataWidth))
    addElement(const0)
    const0.addOutPorts(Array("out_0"))

    addConnect(const0 / "out_0" -> dp0 / constInPort)

    if (isMemFU) {
      /** An LSU can perform load or store operation.
       */
      val LSU = new ElementLSU3("loadStoreUnit", List(dataWidth))
      LSU.addInPorts(Array("addr", "dataIn", "constIn", "pIn"))
      LSU.addOutPorts(Array("out"))
      addElement(LSU)

      addConnect(inPortToRFMap(fuInPorts(1)) / "out_0" -> LSU / "addr")
      addConnect(inPortToRFMap(fuInPorts(0)) / "out_0" -> LSU / "dataIn")
      addConnect(inPortToRFMap(fuInPorts(2)) / "out_0" -> LSU / "pIn")
      addConnect(const0 / "out_0" -> LSU / "constIn")

      val muxT = new ElementMux("muxT", List(2, dataWidth))
      muxT.addOutPorts(Array("out_0"))
      muxT.addInPorts(Array("input_0", "input_1"))
      addElement(muxT)

      /** A register with one input port and one output port.
       * This makes latency of memPE is equal to two
       */
      val rfALUO = new ElementRF("rfALUO", List(0, 1,1, dataWidth))
      rfALUO.addOutPorts(Array("out_0"))
      rfALUO.addInPorts(Array("input_0"))
      addElement(rfALUO)

      addConnect(dp0 / fuOutPort -> rfALUO / "input_0")
      addConnect(rfALUO / "out_0" -> muxT / "input_1")

      val rfLSUO = new ElementRF("rfLSUO", List(0, 1,1, dataWidth))
      rfLSUO.addOutPorts(Array("out_0"))
      rfLSUO.addInPorts(Array("input_0"))
      addElement(rfLSUO)

      addConnect(LSU / "out" -> rfLSUO / "input_0")
      addConnect(rfLSUO / "out_0" -> muxT / "input_0")

      val outComponent = muxT
      (inPortToRFMap, outComponent, "out_0")
    } else {
      (inPortToRFMap, dp0, fuOutPort)
    }
  }
}

/**
 * Parsed PE Block, with hardcoded values
 * @param name
 * @param useMuxBypass
 * @param opList
 * @param aluSupBypass
 * @param isMemPE
 * @param inPortsNeighbor
 * @param dataWidth
 */
class Parsed_PEBlock(name: String, useMuxBypass: Boolean, opList: List[OpEnum] = null,
                     aluSupBypass: Boolean = true, isMemPE: Boolean =true, inPortsNeighbor: Array[String] = null,
                     dataWidth: Int = 32) extends BlockTrait {
  initName(name)
  //Set configuration region.
  setConfigRegion()

  addOutPorts(Array("out_w", "out_e", "out_n", "out_s"))
  addInPorts(inPortsNeighbor)

  val neighborSize = inPortsNeighbor.size

  var aluOpList =
    if (opList == null) {
      //default
      List(OpEnum.ADD, OpEnum.MUL)
    }
    else {
      opList
    }

  val aluInPorts = Array("DP0_I1", "DP0_I2", "DP0_P")
  val aluOutPort = "DP0_T"

  /** An ALU that can perform some operations.
   */
  val alu0 =
    if (isMemPE) {
      new Parsed_ALU_LSU("FU0", aluInPorts, aluOutPort,
        aluOpList, aluSupBypass, dataWidth)
    } else {
      new Parsed_ALU("FU0", aluInPorts, aluOutPort,
        aluOpList, aluSupBypass, dataWidth)//TODO: changes for predicate support??
    }

  addBlock(alu0)

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

  for (i <- 0 until neighborSize) {
    addConnect(term(inPortsNeighbor(i)) -> muxI1 / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> muxI2 / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> muxP / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> muxWP0 / s"input_$i")
    addConnect(term(inPortsNeighbor(i)) -> muxWP1 / s"input_$i")
  }

  addConnect(rf0 / "out_0" -> muxI1 / s"input_$neighborSize")
  addConnect(rf0 / "out_1" -> muxI1 / ("input_" + (neighborSize + 1).toString))
  //  addConnect(const0 / "out_0" -> muxI1 / ("input_" + (neighborSize + 2).toString))
  addConnect(rf0 / "out_0" -> muxI2 / s"input_$neighborSize")
  addConnect(rf0 / "out_1" -> muxI2 / ("input_" + (neighborSize + 1).toString))
  //  addConnect(const0 / "out_0" -> muxI2 / ("input_" + (neighborSize + 2).toString))
  addConnect(rf0 / "out_0" -> muxP / s"input_$neighborSize")
  addConnect(rf0 / "out_1" -> muxP / ("input_" + (neighborSize + 1).toString))
  //  addConnect(const0 / "out_0" -> muxP / ("input_" + (neighborSize + 2).toString))

  addConnect(muxI1 / "out_0" -> alu0 / aluInPorts(0))
  addConnect(muxI2 / "out_0" -> alu0 / aluInPorts(1))
  addConnect(muxP / "out_0" -> alu0 / aluInPorts(2))

  addConnect(muxWP0 / "out_0" -> rf0 / "input_0")
  addConnect(muxWP1 / "out_0" -> rf0 / "input_1")

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

  addConnect(alu0 / aluOutPort -> muxWP0 / s"input_$neighborSize")
  addConnect(alu0 / aluOutPort -> muxWP1 / s"input_$neighborSize")
  addConnect(alu0 / aluOutPort -> muxNO / "input_0")
  addConnect(alu0 / aluOutPort -> muxEO / "input_0")
  addConnect(alu0 / aluOutPort -> muxWO / "input_0")
  addConnect(alu0 / aluOutPort -> muxSO / "input_0")

  addConnect(alu0 / aluOutPort -> muxI1 / ("input_" + (neighborSize + 2).toString))
}

class Parsed_ALU(name: String, inPorts : Array[String], outPort: String, aluOpList: List[OpEnum] = null,
                 aluSupBypass: Boolean, dataWidth: Int = 32)
  extends BlockTrait {

  val DP0_PREFIX_LEN = "DP0_".length

  initName(name)
  //Set configuration region.
  setConfigRegion()

  val constInPort = "input_const"

  addInPorts(inPorts)
  addOutPorts(Array(outPort))

  val dp0InPorts = inPorts.map(inPort => inPort.substring(DP0_PREFIX_LEN))
  val dp0OutPort = outPort.substring(DP0_PREFIX_LEN)

  /** An ALU that can perform some operations.
   * TODO: changes for predicate support??
   */
  val dp0 = new ElementAlu("dp0", aluOpList, aluSupBypass, List(dataWidth))
  // dp0.addInPorts(Array("input_a", "input_b", "input_const", "input_p"))

  dp0.addInPorts(dp0InPorts :+ constInPort)
  dp0.addOutPorts(Array(dp0OutPort))
  addElement(dp0)

  for (inPort <- inPorts) {
    val rf = new ElementRF("rf" + inPort, List(0, 1, 1, dataWidth))
    rf.addOutPorts(Array("out_0"))
    rf.addInPorts(Array("input_0"))
    addElement(rf)

    // in -> rf
    addConnect(term(inPort) -> rf / "input_0")
    // rf -> dp
    addConnect(rf / "out_0" -> dp0 / inPort.substring(DP0_PREFIX_LEN))
  }

  /** A const unit connected to the multiplexers.
   */
  val const0 = new ElementConst("const0", List(dataWidth))
  const0.addOutPorts(Array("out_0"))
  addElement(const0)

  addConnect(const0 / "out_0" -> dp0 / constInPort)
  addConnect(dp0 / dp0OutPort -> term(outPort))
}

class Parsed_ALU_LSU(name: String, inPorts: Array[String], outPort: String,
                     aluOpList: List[OpEnum] = null, aluSupBypass: Boolean, dataWidth: Int = 32)
  extends BlockTrait {

  val DP0_PREFIX_LEN = "DP0_".length

  initName(name)
  //Set configuration region.
  setConfigRegion()

  val constInPort = "input_const"

  addInPorts(inPorts)
  addOutPorts(Array(outPort))

  val dp0InPorts = inPorts.map(inPort => inPort.substring(DP0_PREFIX_LEN))
  val dp0OutPort = outPort.substring(DP0_PREFIX_LEN)

  /** An ALU that can perform some operations.
   * TODO: changes for predicate support??
   */
  val dp0 = new ElementAlu("dp0", aluOpList, aluSupBypass, List(dataWidth))
  // dp0.addInPorts(Array("input_a", "input_b", "input_const", "input_p"))

  dp0.addInPorts(dp0InPorts :+ constInPort)
  dp0.addOutPorts(Array(dp0OutPort))
  addElement(dp0)

  var rfs : Map[String, ElementRF] = Map()

  for (inPort <- inPorts) {
    val rf = new ElementRF("rf" + inPort, List(0, 1, 1, dataWidth))
    rf.addOutPorts(Array("out_0"))
    rf.addInPorts(Array("input_0"))
    addElement(rf)

    // in -> rf
    addConnect(term(inPort) -> rf / "input_0")
    // rf -> dp
    addConnect(rf / "out_0" -> dp0 / inPort.substring(DP0_PREFIX_LEN))

    rfs = rfs + (inPort -> rf)
  }

  /** A const unit connected to the multiplexers.
   */
  val const0 = new ElementConst("const0", List(dataWidth))
  const0.addOutPorts(Array("out_0"))
  addElement(const0)

  addConnect(const0 / "out_0" -> dp0 / constInPort)

  /** An LSU can perform load or store operation.
   */
  val LSU = new ElementLSU2("loadStoreUnit", List(dataWidth))
  LSU.addInPorts(Array("addr", "dataIn", "constIn"))
  LSU.addOutPorts(Array("out"))
  addElement(LSU)

  addConnect(rfs(inPorts(1)) / "out_0" -> LSU / "addr")
  addConnect(rfs(inPorts(0)) / "out_0" -> LSU / "dataIn")
  addConnect(const0 / "out_0" -> LSU / "constIn")

  val muxT = new ElementMux("muxT", List(2, dataWidth))
  muxT.addOutPorts(Array("out_0"))
  muxT.addInPorts(Array("input_0", "input_1"))
  addElement(muxT)

  /** A register with one input port and one output port.
   * This makes latency of memPE is equal to two
   */
  val rfALUO = new ElementRF("rfALUO", List(0, 1,1, dataWidth))
  rfALUO.addOutPorts(Array("out_0"))
  rfALUO.addInPorts(Array("input_0"))
  addElement(rfALUO)

  addConnect(dp0 / dp0OutPort -> rfALUO / "input_0")
  addConnect(rfALUO / "out_0" -> muxT / "input_1")

  val rfLSUO = new ElementRF("rfLSUO", List(0, 1,1, dataWidth))
  rfLSUO.addOutPorts(Array("out_0"))
  rfLSUO.addInPorts(Array("input_0"))
  addElement(rfLSUO)

  addConnect(LSU / "out" -> rfLSUO / "input_0")
  addConnect(rfLSUO / "out_0" -> muxT / "input_0")

  addConnect(muxT / "out_0" -> term(outPort))
}

/**
 * NOT USED YET, can ignore
 * @param name
 * @param inPorts
 * @param outPorts
 * @param regs
 * @param connections
 * @param dataWidth
 */
class Parsed_RF(name: String, inPorts: Array[String], outPorts: Array[String], regs: Array[String],
                connections: Map[String, Array[String]], dataWidth: Int = 32) extends BlockTrait {
  initName(name)
  //Set configuration region.
  setConfigRegion()

  addInPorts(inPorts)
  addOutPorts(outPorts)

  val numIn = inPorts.size
  val numOut = outPorts.size
  val numRegs = regs.size

  val rf0 = new ElementRF(name, List(log2Up(numRegs), numIn, numOut, dataWidth))
  addElement(rf0)

  rf0.addInPorts(inPorts)
  rf0.addOutPorts(outPorts)


}
