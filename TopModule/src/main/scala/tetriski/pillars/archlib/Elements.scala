package tetriski.pillars.archlib

import tetriski.pillars.core.OpEnum.OpEnum
import tetriski.pillars.core.{BlockTrait, ModuleTrait, OpEnum, OpcodeTranslator}
import tetriski.pillars.core.MRRGMode._


class OpAlu(name: String, aluOpList: List[OpEnum], supBypass: Boolean, params: List[Int]) extends ModuleTrait {
  //Module ID 0
  setTypeID(0)
  //Default
  setSupOps(aluOpList)
//  setSupOps(List(OpEnum.ADD, OpEnum.SUB, OpEnum.AND, OpEnum.OR, OpEnum.XOR, OpEnum.MUL))
  //4 bit configuration
  //setConfigBit(4)
  val aluFunSelect = OpcodeTranslator.getAluFunSelect(aluOpList, supBypass)

  //setWidth(width)
  setParams(aluFunSelect +: params)
  setName(name)

  //addInternalNodesNum(1)
 // support passby
  if(supBypass){
    addInternalNodesNum(2)
  }else{
    addInternalNodesNum(1)
  }



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

  addInternalNodesNum(Math.pow(2, params.head).toInt)
  setMRRGMode(REG_MODE)
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

  setSupOps(List(OpEnum.CONST))
  //4 bit configuration
  //setConfigBit(32)

  //setWidth(width)
  setParams(params)
  setName(name)

  addInternalNodesNum(1)
}

class OpLSU(name: String, params: List[Int]) extends ModuleTrait {
  //Module ID 4
  setTypeID(4)

  setSupOps(List(OpEnum.LOAD, OpEnum.STORE))
  //4 bit configuration
  //setConfigBit(32)

  //setWidth(width)
  setParams(params)
  setName(name)

  addInternalNodesNum(1)
  setMRRGMode(MEM_MODE)
}


class Block(name: String) extends BlockTrait {
  setName(name)
  hierName.append(name)
}