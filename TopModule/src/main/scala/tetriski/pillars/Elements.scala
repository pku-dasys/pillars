package tetriski.pillars

//############### Currently Unused #################
//The 'MemUnit'/'MuxAddr'/'MuxData' are the submodules of the 'MemPort',
//we abstract them and put them in the MemTrait.
trait MemTrait extends ModuleTrait {
  val memUnit = new MemUnit
  val muxAddr = new MuxAddr
  val muxData = new MuxData
}

class MemUnit extends ModuleTrait {
}

class MuxAddr extends ModuleTrait {
}

class MuxData extends ModuleTrait {
}

class MemPort extends MemTrait {
}

//############### Currently Unused #################


class OpAlu(name: String, params: List[Int]) extends ModuleTrait {
  //Module ID 0
  setTypeID(0)
  //Support add, sub, and, or, xor
  setSupOps(List("add", "sub", "and", "or", "xor"))
  //4 bit configuration
  //setConfigBit(4)

  //setWidth(width)
  setParams(params)
  setName(name)

  addInternalNodesNum(1)
}

class OpRF(name: String, params: List[Int]) extends ModuleTrait {

  //Module ID 1
  setTypeID(1)

  setSupOps(List())
  //4 bit configuration
  //setConfigBit(3)

  //setWidth(width)
  setParams(params)
  setName(name)

  addInternalNodesNum(Math.pow(2, params(0)).toInt)
}

class OpMux(name: String, params: List[Int]) extends ModuleTrait {

  //Module ID 2
  setTypeID(2)

  setSupOps(List())
  //4 bit configuration
  //setConfigBit(3)

  //setWidth(width)
  setParams(params)
  setName(name)

  addInternalNodesNum(1)
}

class OpConst(name: String, params: List[Int]) extends ModuleTrait {
  //Module ID 3
  setTypeID(3)
  //Support 5 to 1 mux
  setSupOps(List("const"))
  //4 bit configuration
  //setConfigBit(32)

  //setWidth(width)
  setParams(params)
  setName(name)

  addInternalNodesNum(1)
}


class Block(name: String) extends BlockTrait {
  setName(name)
  hierName.append(name)
}