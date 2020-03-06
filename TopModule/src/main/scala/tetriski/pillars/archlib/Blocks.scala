package tetriski.pillars.archlib

import chisel3.util.log2Up
import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{BlockTrait, OpEnum, OpcodeTranslator}
//import tetriski.pillars.archlib.OpConst

import scala.collection.mutable.ArrayBuffer


class PEBlock(name: String) extends BlockTrait{
  //Eliminated

  setName(name)
  hierName.append(name)
  isConfigRegion = true

  addOutPorts(Array("out_0"))
  addInPorts(Array("input_0", "input_1", "input_2", "input_3"))

  val aluOpList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR, OpEnum.MUL)
  val supBypass = false
  val alu0 = new OpAlu("alu0", aluOpList, supBypass, List(32, 4))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  alu0.addOutPorts(Array("out_0"))
  alu0.addInPorts(Array("input_a", "input_b"))
  addModule(alu0)

  val mux0 = new OpMux("mux0", List(5, 32, 3))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_0, 1: input_1, 2: input_2, 3: input_3, 4: input_4
  mux0.addOutPorts(Array("out_0"))
  mux0.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4"))
  addModule(mux0)

  val mux1 = new OpMux("mux1", List(5, 32, 3))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_0, 1: input_1, 2: input_2, 3: input_3, 4: input_4
  mux1.addOutPorts(Array("out_0"))
  mux1.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4"))
  addModule(mux1)

  val rf0 = new OpRF("rf0", List(1, 1, 2, 32, 3 + 1))
  //port sequnces outs: 0: out_0, 1: out_1
  //port sequnces inputs: 0: input_0
  rf0.addOutPorts(Array("out_0", "out_1"))
  rf0.addInPorts(Array("input_0"))
  addModule(rf0)

  connectArray =
    ArrayBuffer(List(List("input_0"),List("mux0","input_0")),
      List(List("input_0"),List("mux1","input_0")),
      List(List("input_1"),List("mux0","input_1")),
      List(List("input_1"),List("mux1","input_1")),
      List(List("input_2"),List("mux0","input_2")),
      List(List("input_2"),List("mux1","input_2")),
      List(List("input_3"),List("mux0","input_3")),
      List(List("input_3"),List("mux1","input_3")),
      List(List("rf0", "out_0"),List("mux0","input_4")),
      List(List("rf0", "out_0"),List("mux1","input_4")),
      List(List("mux0","out_0"),List("alu0","input_a")),
      List(List("mux1","out_0"),List("alu0","input_b")),
      List(List("alu0","out_0"),List("rf0","input_0")),
      List(List("rf0","out_1"),List("out_0")))
}

class AdresPEBlock(name: String, useMuxBypass: Boolean, opList: List[OpEnum] = null,
                   aluSupBypass: Boolean = true, inPortsNeighbor: Array[String] = null,
                   dataWidth: Int = 32) extends BlockTrait{
  setName(name)
  hierName.append(name)
  isConfigRegion = true

  addOutPorts(Array("out"))
  addInPorts(inPortsNeighbor)

  val neighborSize = inPortsNeighbor.size

  var aluOpList = opList
  if(aluOpList == null){
    aluOpList = List(OpEnum.ADD, OpEnum.MUL)
    //  val aluOpList = List(OpEnum.ADD, OpEnum.SUB)
    //  val aluOpList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
    //    OpEnum.MUL, OpEnum.DIV, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
  }

  val alu0 = new OpAlu("alu0", aluOpList, aluSupBypass, List(dataWidth, 4))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  alu0.addOutPorts(Array("out_0"))
  alu0.addInPorts(Array("input_a", "input_b"))
  addModule(alu0)

  val mux0 = new OpMux("mux0", List(neighborSize + 2, dataWidth, log2Up(neighborSize + 2)))
  mux0.addOutPorts(Array("out_0"))
  mux0.addInPorts((0 until neighborSize + 2).map(i => "input_" + i.toString).toArray)
  //mux0.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4", "input_5", "input_6", "input_7"))
  addModule(mux0)

  val mux1 = new OpMux("mux1", List(neighborSize + 1, dataWidth, log2Up(neighborSize + 1)))
  mux1.addOutPorts(Array("out_0"))
  mux1.addInPorts((0 until neighborSize + 1).map(i => "input_" + i.toString).toArray)
  //mux1.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4", "input_5", "input_6"))
  addModule(mux1)


  val rf0 = new OpRF("rf0", List(1, 1, 2, dataWidth, 3))
  //port sequnces outs: 0: out_0
  //port sequnces inputs: 0: input_0
  rf0.addOutPorts(Array("out_0", "out_1"))
  rf0.addInPorts(Array("input_0"))
  addModule(rf0)

  val const0 = new OpConst("const0", List(dataWidth, dataWidth))
  const0.addOutPorts(Array("out_0"))
  addModule(const0)

//  addConnect(List("input_w"),List("mux0","input_0"))
//  addConnect(List("input_w"),List("mux1","input_0"))
//  addConnect(List("input_e"),List("mux0","input_1"))
//  addConnect(List("input_e"),List("mux1","input_1"))
//  addConnect(List("input_n"),List("mux0","input_2"))
//  addConnect(List("input_n"),List("mux1","input_2"))
//  addConnect(List("input_s"),List("mux0","input_3"))
//  addConnect(List("input_s"),List("mux1","input_3"))
//  addConnect(List("input_lsu"),List("mux0","input_4"))
//  addConnect(List("input_lsu"),List("mux1","input_4"))
//  addConnect(List("const0", "out_0"),List("mux0","input_5"))
//  addConnect(List("const0", "out_0"),List("mux1","input_5"))

  for(i <- 0 until neighborSize){
    addConnect(List(inPortsNeighbor(i)), List("mux0", "input_" + i.toString))
    addConnect(List(inPortsNeighbor(i)), List("mux1", "input_" + i.toString))
  }
  addConnect(List("const0", "out_0"),List("mux0","input_" + (neighborSize).toString))
  addConnect(List("const0", "out_0"),List("mux1","input_" + (neighborSize).toString))
  addConnect(List("rf0", "out_0"),List("mux0","input_" + (neighborSize + 1).toString))
  addConnect(List("mux0","out_0"),List("alu0","input_a"))
  addConnect(List("mux1","out_0"),List("alu0","input_b"))
  addConnect(List("alu0","out_0"),List("rf0","input_0"))


  if(useMuxBypass){
    val muxBp = new OpMux("muxBp", List(neighborSize, dataWidth, log2Up(neighborSize)))
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_0, 1: input_1, 2: input_2, 3: input_3, 4: input_4
    muxBp.addOutPorts(Array("out_0"))
    muxBp.addInPorts((0 until neighborSize).map(i => "input_" + i.toString).toArray)
//    muxBp.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4"))
    addModule(muxBp)

    val muxOut = new OpMux("muxOut", List(2, dataWidth, 1))
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_0, 1: input_1
    muxOut.addOutPorts(Array("out_0"))
    muxOut.addInPorts(Array("input_0", "input_1"))
    addModule(muxOut)

//    addConnect(List("input_w"),List("muxBp","input_0"))
//    addConnect(List("input_e"),List("muxBp","input_1"))
//    addConnect(List("input_n"),List("muxBp","input_2"))
//    addConnect(List("input_s"),List("muxBp","input_3"))
//    addConnect(List("input_lsu"),List("muxBp","input_4"))

    for(i <- 0 until neighborSize){
      addConnect(List(inPortsNeighbor(i)), List("muxBp", "input_" + i.toString))
    }

    addConnect(List("muxBp","out_0"),List("muxOut","input_1"))
    addConnect(List("rf0","out_1"),List("muxOut","input_0"))
    addConnect(List("muxOut","out_0"),List("out_0"))
  }else{
    addConnect(List("rf0","out_1"),List("out_0"))
  }
}

class AdresVLIWPEBlock(name: String, useMuxBypass: Boolean, opList: List[OpEnum] = null,
                       aluSupBypass: Boolean = true, inPortsNeighbor: Array[String] = null,
                       dataWidth: Int = 32) extends BlockTrait{
  setName(name)
  hierName.append(name)
  isConfigRegion = true

//  inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")

  addOutPorts(Array("out", "rf_out"))
  addInPorts(inPortsNeighbor ++ Array("input_rf_mux0", "input_rf_muxOut", "input_IO"))


  var aluOpList = opList
  if(aluOpList == null){
    aluOpList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
      OpEnum.MUL, OpEnum.DIV, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
  }

  val neighborSize = inPortsNeighbor.size

  val alu0 = new OpAlu("alu0", aluOpList, aluSupBypass, List(dataWidth, 4))
  //port sequnces outs: 0: out
  //port sequnces inputs: 0: input_a, 1: input_b
  alu0.addOutPorts(Array("out_0"))
  alu0.addInPorts(Array("input_a", "input_b"))
  addModule(alu0)

  val mux0 = new OpMux("mux0", List(neighborSize + 3, dataWidth, log2Up(neighborSize + 3)))
  mux0.addOutPorts(Array("out_0"))
  mux0.addInPorts((0 until neighborSize + 3).map(i => "input_" + i.toString).toArray)
  //mux0.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4", "input_5", "input_6", "input_7"))
  addModule(mux0)

  val mux1 = new OpMux("mux1", List(neighborSize + 2, dataWidth, log2Up(neighborSize + 2)))
  mux1.addOutPorts(Array("out_0"))
  mux1.addInPorts((0 until neighborSize + 2).map(i => "input_" + i.toString).toArray)
  //mux1.addInPorts(Array("input_0", "input_1", "input_2", "input_3", "input_4", "input_5", "input_6"))
  addModule(mux1)

  val const0 = new OpConst("const0", List(dataWidth, dataWidth))
  const0.addOutPorts(Array("out_0"))
  addModule(const0)

//  addConnect(List("input_w"),List("mux0","input_0"))
//  addConnect(List("input_w"),List("mux1","input_0"))
//  addConnect(List("input_e"),List("mux0","input_1"))
//  addConnect(List("input_e"),List("mux1","input_1"))
//  addConnect(List("input_n"),List("mux0","input_2"))
//  addConnect(List("input_n"),List("mux1","input_2"))
//  addConnect(List("input_s"),List("mux0","input_3"))
//  addConnect(List("input_s"),List("mux1","input_3"))
//  addConnect(List("input_lsu"),List("mux0","input_4"))
//  addConnect(List("input_lsu"),List("mux1","input_4"))
//  addConnect(List("input_IO"),List("mux0","input_5"))
//  addConnect(List("input_IO"),List("mux1","input_5"))
//  addConnect(List("const0", "out_0"),List("mux0","input_6"))
//  addConnect(List("const0", "out_0"),List("mux1","input_6"))
//  addConnect(List("input_rf_mux0"),List("mux0","input_7"))
  for(i <- 0 until neighborSize){
    addConnect(List(inPortsNeighbor(i)), List("mux0", "input_" + i.toString))
    addConnect(List(inPortsNeighbor(i)), List("mux1", "input_" + i.toString))
  }
  addConnect(List("input_IO"),List("mux0","input_" + (neighborSize).toString))
  addConnect(List("input_IO"),List("mux1","input_" + (neighborSize).toString))
  addConnect(List("const0", "out_0"),List("mux0","input_" + (neighborSize + 1).toString))
  addConnect(List("const0", "out_0"),List("mux1","input_" + (neighborSize + 1).toString))
  addConnect(List("input_rf_mux0"),List("mux0","input_" + (neighborSize + 2).toString))
  addConnect(List("mux0","out_0"),List("alu0","input_a"))
  addConnect(List("mux1","out_0"),List("alu0","input_b"))
  addConnect(List("alu0","out_0"),List("rf_out"))


  if(useMuxBypass){
    val muxBp = new OpMux("muxBp", List(neighborSize + 1, dataWidth, log2Up(neighborSize + 1)))
    muxBp.addOutPorts(Array("out_0"))
    muxBp.addInPorts((0 until neighborSize + 1).map(i => "input_" + i.toString).toArray)
    addModule(muxBp)

    val muxOut = new OpMux("muxOut", List(2, dataWidth, 1))
    //port sequnces outs: 0: out
    //port sequnces inputs: 0: input_0, 1: input_1
    muxOut.addOutPorts(Array("out_0"))
    muxOut.addInPorts(Array("input_0", "input_1"))
    addModule(muxOut)

//    addConnect(List("input_w"),List("muxBp","input_0"))
//    addConnect(List("input_e"),List("muxBp","input_1"))
//    addConnect(List("input_n"),List("muxBp","input_2"))
//    addConnect(List("input_s"),List("muxBp","input_3"))
//    addConnect(List("input_lsu"),List("muxBp","input_4"))
    for(i <- 0 until neighborSize){
      addConnect(List(inPortsNeighbor(i)), List("muxBp", "input_" + i.toString))
    }
    addConnect(List("input_IO"),List("muxBp","input_" + (neighborSize).toString))

    addConnect(List("muxBp","out_0"),List("muxOut","input_1"))
    addConnect(List("input_rf_muxOut"),List("muxOut","input_0"))
    addConnect(List("muxOut","out_0"),List("out_0"))
  }else{
    addConnect(List("input_rf_muxOut"), List("out_0"))
  }
}

class AdresIOBlock(name: String, numIn: Int, numOut: Int, numNeighbour: Int, dataWidth: Int = 32) extends BlockTrait{

  setName(name)
  hierName.append(name)
  isConfigRegion = true

  addOutPorts((0 to numOut).map(i => "out_" + i.toString).toArray)
  addOutPorts((0 to numNeighbour).map(i => "neighbour_out_" + i.toString).toArray)
  addInPorts((0 to numIn).map(i => "input_" + i.toString).toArray)
  addInPorts((0 to numNeighbour).map(i => "neighbour_input_" + i.toString).toArray)

  for(i <- 0 until numOut){
    val mux = new OpMux("muxN2O_" + i.toString, List(numNeighbour, dataWidth, log2Up(numNeighbour)))
    mux.addOutPorts(Array("out_0"))
    for(j <- 0 until numNeighbour){
      mux.addInPorts(Array("input_" + j.toString))
      addConnect(List(List("neighbour_input_" + j.toString),List(mux.getName(), "input_" + j.toString)))
    }
    addModule(mux)
    addConnect(List(List(mux.getName(),"out_0"),List("out_" + i.toString)))
  }

  for(i <- 0 until numNeighbour){
    val mux = new OpMux("muxI2N_" + i.toString, List(numIn, dataWidth, log2Up(numIn)))
    mux.addOutPorts(Array("out_0"))
    for(j <- 0 until numIn){
      mux.addInPorts(Array("input_" + j.toString))
      addConnect(List(List("input_" + j.toString),List(mux.getName(), "input_" + j.toString)))
    }
    addModule(mux)
    addConnect(List(List(mux.getName(),"out_0"),List("neighbour_out_" + i.toString)))
  }
  // println("IOBlock!!!!",connectArray)

}

class AdresLSUBlock(name: String, numNeighbour: Int, dataWidth: Int = 32) extends BlockTrait{

  setName(name)
  hierName.append(name)
  isConfigRegion = true

  addOutPorts(Array("out"))
  addInPorts((0 to numNeighbour).map(i => "neighbour_input_" + i.toString).toArray)

  val muxAddr = new OpMux("muxAddr", List(numNeighbour, dataWidth, log2Up(numNeighbour)))
  muxAddr.addOutPorts(Array("out"))
  for(j <- 0 until numNeighbour){
    muxAddr.addInPorts(Array("input_" + j.toString))
    addConnect(List(List("neighbour_input_" + j.toString),List(muxAddr.getName(), "input_" + j.toString)))
  }
  addModule(muxAddr)

  val muxDataIn = new OpMux("muxDataIn", List(numNeighbour, dataWidth, log2Up(numNeighbour)))
  muxDataIn.addOutPorts(Array("out"))
  for(j <- 0 until numNeighbour){
    muxDataIn.addInPorts(Array("input_" + j.toString))
    addConnect(List(List("neighbour_input_" + j.toString),List(muxDataIn.getName(), "input_" + j.toString)))
  }
  addModule(muxDataIn)

  // w = 32, config bit = 1
  val LSU = new OpLSU("loadStoreUnit", List(dataWidth, 1))
  LSU.addInPorts(Array("addr", "dataIn"))
  LSU.addOutPorts(Array("out"))
  addConnect(List(List(muxAddr.getName(), "out"), List(LSU.getName(), "addr")))
  addConnect(List(List(muxDataIn.getName(), "out"), List(LSU.getName(), "dataIn")))
  addConnect(List(List(LSU.getName(), "out"),List("out")))

  addModule(LSU)

}

class AdresGlobalRFBlock(name: String, numNeighbour: Int, dataWidth: Int = 32) extends BlockTrait{

  setName(name)
  hierName.append(name)
  isConfigRegion = true

  val inNum = numNeighbour
  val outNum = inNum * 2
  val log2Reg = log2Up(outNum)

  addOutPorts((0 to (outNum -1) * 2).map(i => "out_" + i.toString).toArray)
  addInPorts((0 to (inNum -1)).map(i => "input_" + i.toString).toArray)



  val global_rf = new OpRF("global_rf", List(log2Reg, inNum, outNum, dataWidth, log2Reg * (inNum + outNum) + 1))
  //  //port sequnces outs: 0: out_0
  //  //port sequnces inputs: 0: input_0
  global_rf.addOutPorts((0 to (outNum -1)).map(i => "out_" + i.toString).toArray)
  global_rf.addInPorts((0 to (inNum -1)).map(i => "input_" + i.toString).toArray)
  addModule(global_rf)


  for(i <- 0 until inNum){
    addConnect(List(global_rf.getName(), "out_" + (i * 2).toString), List("out_" + (i * 2).toString))
    addConnect(List(global_rf.getName(), "out_" + (i * 2 + 1).toString), List("out_" + (i * 2 + 1).toString))
    addConnect(List("input_" + i.toString), List(global_rf.getName(), "input_" + i.toString))
  }


}

class TileBlock(name: String, x: Int, y: Int, numIn: Int, numOut: Int,
                useMuxBypass: Boolean = true, dataWidth: Int = 32)
  extends BlockTrait {
  setName(name)
  hierName.append(name)
  addOutPorts((0 to numOut-1).map(i => "out_" + i.toString).toArray)
  addInPorts((0 to numIn-1).map(i => "input_" + i.toString).toArray)
  val ioBlock = new AdresIOBlock("ioBlock", numIn, numOut, x, dataWidth = dataWidth)
  addBlock(ioBlock)

  for(i <- 0 until numOut){
    addConnect(List(List(ioBlock.getName() + "/", "out_" + i.toString),List("out_" + i.toString)))
  }
  for(i <- 0 until numIn){
    addConnect(List(List("input_" + i.toString),List(ioBlock.getName() + "/", "input_" + i.toString)))
  }


  var peMap = Map[Int, AdresPEBlock]()
  for (j <- 0 until y){
    for (i <- 0 until x){
      val inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s")
      val opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR, OpEnum.MUL)
      val pe = new AdresPEBlock("pe_" + j.toString + "_" + i.toString, opList = opList,
        useMuxBypass = useMuxBypass, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
      peMap = peMap + ((i + j * x) -> pe)
      addBlock(pe)
    }
  }
  for (j <- 0 until y){
    for (i <- 0 until x) {
      val peCurrent = peMap(i + j * x)
      val peN = peMap(i + ((j + 1) % y) * x)
      val peS = peMap(i + (((j - 1) + y) % y) * x)
      val peE = peMap((i + 1) % x + j * x)
      val peW = peMap(((i - 1) + x) % x + j * x)
      if(j != y - 1){
        connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peS.getName() + "/", "input_n")))
      }else{
        connectArray.append(List(List(ioBlock.getName() + "/" ,"neighbour_out_" + i.toString),
          List(peS.getName() + "/", "input_n")))
        connectArray.append(List(List(peS.getName() + "/", "out_0"),
          List(ioBlock.getName() + "/" ,"neighbour_input_" + i.toString)))
      }
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peN.getName() + "/", "input_s")))
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peE.getName() + "/", "input_w")))
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peW.getName() + "/", "input_e")))
    }
  }

}

class TileLSUBlock(name: String, x: Int, y: Int, numIn: Int, numOut: Int,
                   useMuxBypass: Boolean = true, dataWidth: Int = 32)
  extends BlockTrait {
  setName(name)
  hierName.append(name)
  addOutPorts((0 to numOut-1).map(i => "out_" + i.toString).toArray)
  addInPorts((0 to numIn-1).map(i => "input_" + i.toString).toArray)
  val ioBlock = new AdresIOBlock("ioBlock", numIn, numOut, x, dataWidth = dataWidth)
  addBlock(ioBlock)

  for(i <- 0 until numOut){
    addConnect(List(List(ioBlock.getName() + "/", "out_" + i.toString),List("out_" + i.toString)))
  }
  for(i <- 0 until numIn){
    addConnect(List(List("input_" + i.toString),List(ioBlock.getName() + "/", "input_" + i.toString)))
  }


  var peMap = Map[Int, AdresPEBlock]()
  for (j <- 0 until y){
    for (i <- 0 until x){
      val inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")
      val opList = List(OpEnum.ADD, OpEnum.MUL)
      val pe = new AdresPEBlock("pe_" + j.toString + "_" + i.toString, opList = opList,
        useMuxBypass = useMuxBypass, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
      peMap = peMap + ((i + j * x) -> pe)
      addBlock(pe)
    }
  }
  for (j <- 0 until y){
    for (i <- 0 until x) {
      val peCurrent = peMap(i + j * x)
      val peN = peMap(i + ((j + 1) % y) * x)
      val peS = peMap(i + (((j - 1) + y) % y) * x)
      val peE = peMap((i + 1) % x + j * x)
      val peW = peMap(((i - 1) + x) % x + j * x)
      if(j != y - 1){
        connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peS.getName() + "/", "input_n")))
      }else{
        connectArray.append(List(List(ioBlock.getName() + "/" ,"neighbour_out_" + i.toString),
          List(peS.getName() + "/", "input_n")))
        connectArray.append(List(List(peS.getName() + "/", "out_0"),
          List(ioBlock.getName() + "/" ,"neighbour_input_" + i.toString)))
      }
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peN.getName() + "/", "input_s")))
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peE.getName() + "/", "input_w")))
      connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peW.getName() + "/", "input_e")))
    }
  }

  // PEs in same row share a LoadStoreUnit
  for (j <- 0 until y){
    val lsuBlock = new AdresLSUBlock("lsu_" + j.toString, x)
    addBlock(lsuBlock)
    for (i <- 0 until x){
      val pe = peMap(i + j * x)
      addConnect(List(pe.getName() + "/", "out_0"), List(lsuBlock.getName() + "/", "neighbour_input_" + i.toString))
      addConnect(List(lsuBlock.getName() + "/", "out"), List(pe.getName() + "/", "input_lsu"))
    }
  }

}

class TileCompleteBlock(name: String, x: Int, y: Int, numIn: Int, numOut: Int, useMuxBypass: Boolean = true,
                        isReduceArch: Boolean = false, isFullArch: Boolean = false,
                        isToroid: Boolean = true, dataWidth: Int = 32)
  extends BlockTrait {
  setName(name)
  hierName.append(name)
  addOutPorts((0 to numOut-1).map(i => "out_" + i.toString).toArray)
  addInPorts((0 to numIn-1).map(i => "input_" + i.toString).toArray)



  val ioBlock = new AdresIOBlock("ioBlock", numIn, numOut, x, dataWidth = dataWidth)
  addBlock(ioBlock)

  val globalRFBlock = new AdresGlobalRFBlock("globalRFBlock", x, dataWidth = dataWidth)
  addBlock(globalRFBlock)

  for(i <- 0 until numOut){
    addConnect(List(List(ioBlock.getName() + "/", "out_" + i.toString),List("out_" + i.toString)))
  }
  for(i <- 0 until numIn){
    addConnect(List(List("input_" + i.toString),List(ioBlock.getName() + "/", "input_" + i.toString)))
  }

  var peMap = Map[Int, BlockTrait]()
  for (j <- 0 until y){
    for (i <- 0 until x){
      if(j  == 0){
        var inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")
        if(!isToroid){
          if(i == 0){
            inPortsNeighbor = Array("input_e", "input_s", "input_lsu")
          }else if(i == x -1){
            inPortsNeighbor = Array("input_w", "input_s", "input_lsu")
          }else{
            inPortsNeighbor = Array("input_w", "input_e", "input_s", "input_lsu")
          }
        }
        var opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
          OpEnum.MUL, OpEnum.DIV, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
        val pe = new AdresVLIWPEBlock("pe_" + j.toString + "_" + i.toString, opList = opList,
          useMuxBypass = useMuxBypass, inPortsNeighbor = inPortsNeighbor, dataWidth = dataWidth)
        peMap = peMap + ((i + j * x) -> pe)
        addBlock(pe)
      }else{
        var inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")
        if(!isToroid){
          if(j != y -1){
            if(i == 0){
              inPortsNeighbor = Array("input_e", "input_n", "input_s", "input_lsu")
            }else if(i == x -1){
              inPortsNeighbor = Array("input_w", "input_n", "input_s", "input_lsu")
            }else{
              inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_s", "input_lsu")
            }
          }else{
            if(i == 0){
              inPortsNeighbor = Array("input_e", "input_n", "input_lsu")
            }else if(i == x -1){
              inPortsNeighbor = Array("input_w", "input_n", "input_lsu")
            }else{
              inPortsNeighbor = Array("input_w", "input_e", "input_n", "input_lsu")
            }
          }
        }
        var opList = List(OpEnum.ADD, OpEnum.MUL)

        if(isFullArch){
          opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
            OpEnum.MUL, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
        }else if(isReduceArch){
          if((i % 2) == 0){
            opList = List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR,
              OpEnum.MUL, OpEnum.SHLL, OpEnum.SHRA, OpEnum.SHRL)
          }else{
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
  for (j <- 0 until y){
    for (i <- 0 until x) {
      val peCurrent = peMap(i + j * x)
      val peN = peMap(i + ((j + 1) % y) * x)
      val peS = peMap(i + (((j - 1) + y) % y) * x)
      val peE = peMap((i + 1) % x + j * x)
      val peW = peMap(((i - 1) + x) % x + j * x)
      if(j == 0){
        connectArray.append(List(List(ioBlock.getName() + "/" ,"neighbour_out_" + i.toString),
          List(peCurrent.getName() + "/", "input_IO")))
        connectArray.append(List(List(peCurrent.getName() + "/", "out_0"),
          List(ioBlock.getName() + "/" ,"neighbour_input_" + i.toString)))
        connectArray.append(List(List(peCurrent.getName() + "/", "rf_out"),
          List(globalRFBlock.getName() + "/" ,"input_" + i.toString)))
        connectArray.append(List(List(globalRFBlock.getName() + "/" , "out_" + (i * 2).toString),
          List(peCurrent.getName() + "/", "input_rf_mux0")))
        connectArray.append(List(List(globalRFBlock.getName() + "/" , "out_" + (i * 2 + 1).toString),
          List(peCurrent.getName() + "/", "input_rf_muxOut")))
      }
      if(isToroid){
        connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peS.getName() + "/", "input_n")))
        connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peN.getName() + "/", "input_s")))
        connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peE.getName() + "/", "input_w")))
        connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peW.getName() + "/", "input_e")))
      }else{
        if(j != y -1){
          connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peS.getName() + "/", "input_n")))
        }
        if(j != 0){
          connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peN.getName() + "/", "input_s")))
        }
        if(i != x -1){
          connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peE.getName() + "/", "input_w")))
        }
        if(i != 0){
          connectArray.append(List(List(peCurrent.getName() + "/", "out_0"), List(peW.getName() + "/", "input_e")))
        }
      }
    }
  }

  // PEs in same row share a LoadStoreUnit
  for (j <- 0 until y){
    val lsuBlock = new AdresLSUBlock("lsu_" + j.toString, x, dataWidth = dataWidth)
    addBlock(lsuBlock)
    for (i <- 0 until x){
      val pe = peMap(i + j * x)
      addConnect(List(pe.getName() + "/", "out_0"), List(lsuBlock.getName() + "/", "neighbour_input_" + i.toString))
      addConnect(List(lsuBlock.getName() + "/", "out"), List(pe.getName() + "/", "input_lsu"))
    }
  }

}