package tetriski.pillars.core

import OpEnum.OpEnum

import scala.collection.mutable.ArrayBuffer
import tetriski.pillars.hardware.PillarsConfig._

//ModuleBasic is basic information of a module
trait ElementBasic {
  var typeID = -1
  var moduleID = -1
  var width = -1
  var name = ""
  var supOps = new ArrayBuffer[OpEnum]
  var params = new ArrayBuffer[Int]
  var configBit = 0
  var configArray = new ArrayBuffer[Int]
  var waitCycles = new Array[Int](II_UPPER_BOUND)
  var skews = new Array[Int](II_UPPER_BOUND)

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
    supOps.clear()
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

  def setWaitCycle(waitCycle: Int, II: Int): Unit ={
    waitCycles(II) = waitCycle
  }

  def setSkew(skew: Int, II: Int): Unit ={
    skews(II) = skew
  }

  def resetWaitCycle(): Unit ={
    for(i <- 0 until II_UPPER_BOUND){
      waitCycles(i) = 0
    }
  }

  def resetSkew(): Unit ={
    for(i <- 0 until II_UPPER_BOUND){
      skews(i) = 0
    }
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

  def getWaitCycles(): Array[Int] = {
    waitCycles
  }

  def getSchedule(): Array[Int] = {
    var ret = new Array[Int](II_UPPER_BOUND)
    for(i <- 0 until II_UPPER_BOUND){
      var skew = skews(i)
      if(skew < 0){
        skew = Math.pow(2, LOG_SCHEDULE_SIZE).toInt - skew
      }
      val waitCycle = waitCycles(i)
      val sche = (skew << LOG_SCHEDULE_SIZE) + waitCycle
      ret(i) = sche
    }
    ret
  }


}
