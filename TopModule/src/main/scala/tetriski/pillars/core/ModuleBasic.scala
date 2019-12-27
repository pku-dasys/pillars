package tetriski.pillars.core

import OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer

//ModuleBasic is basic information of a module
trait ModuleBasic {
  var typeID = -1
  var moduleID = -1
  var width = -1
  var name = ""
  var supOps = new ArrayBuffer[OpEnum]
  var params = new ArrayBuffer[Int]
  var configBit = 0
  var configArray = new ArrayBuffer[Int]

  def setModuleID(arg: Int): Unit = {
    moduleID = arg
  }

  def setTypeID(arg: Int): Unit = {
    typeID = arg
  }

  def setWidth(arg: Int): Unit = {
    width = arg
  }

  def setName(arg: String): Unit = {
    name = arg
  }

  def setSupOps(arg: List[OpEnum]): Unit = {
    arg.foreach(t => supOps.append(t))
  }

  def setParams(arg: List[Int]): Unit = {
    arg.foreach(t => params.append(t))
    setConfigBit(arg(arg.length-1))
  }


  def setConfigBit(arg: Int): Unit = {
    configBit = arg
    for (i <- 0 until arg)
    configArray.append(0)
  }

  def getModuleID(): Int = {
    moduleID
  }

  def getTypeID(): Int = {
    typeID
  }

  def getName(): String = {
    name
  }

  def getWidth(): Int = {
    params(params.length-2)
  }

  def getSupOps(): ArrayBuffer[OpEnum] = {
    supOps
  }

  def getParams(): List[Int] = {
    params.toList
  }

  def getConfigBit(): Int = {
    configBit
  }


}
