package tetriski.pillars.archlib

import play.api.libs.json._
import scala.collection.mutable.LinkedHashMap

class PillarsArch(json: JsObject) {
  val peConnections : LinkedHashMap[String, Array[String]] = LinkedHashMap()
  val memPEConnections : LinkedHashMap[String, Array[String]] = LinkedHashMap()
  val submods : LinkedHashMap[String, Array[String]] = LinkedHashMap()
  val memPESubmods : LinkedHashMap[String, Array[String]] = LinkedHashMap()

  var aluInPorts : Array[String] = Array()
  var aluOutPort : String = ""

  var rfInPorts : Array[String] = Array()
  var rfOutPorts : Array[String] = Array()

  init()

  def init(): Unit = {
    initPeConnections((json \ "ARCH" \ "PE" \ "CONNECTIONS").get.as[JsObject], peConnections)
    initPeConnections((json \ "ARCH" \ "PE_MEM" \ "CONNECTIONS").get.as[JsObject], memPEConnections)
    initSubmods((json \ "ARCH" \ "PE" \ "SUBMODS").get.as[JsObject], submods)
    initSubmods((json \ "ARCH" \ "PE_MEM" \ "SUBMODS").get.as[JsObject], memPESubmods)
    initFU()
    initRF()
    show()
  }

  def initPeConnections(connection : JsObject, outMap : LinkedHashMap[String, Array[String]]): Unit = {
    for ((src, dstsVal) <- connection.fields) {
      val dsts = dstsVal.as[JsArray]
      var convertedDsts = Array[String]()
      for (dstVal <- dsts.value) {
        val dst = dstVal.as[String]
        convertedDsts = convertedDsts :+ dst
      }
      outMap += (src -> convertedDsts)
    }
  }

  def initSubmods(submodsJson: JsObject, outMap : LinkedHashMap[String, Array[String]]): Unit = {
    for ((component, instancesArrVal) <- submodsJson.fields) {
      val instancesArr = instancesArrVal.as[JsArray]
      var convertedInstances = Array[String]()
      for (instanceObjVal <- instancesArr.value) {
        val instanceObj = instanceObjVal.as[JsObject]
        val instance = (instanceObj \ "name").as[String]
        convertedInstances = convertedInstances :+ instance
      }
      outMap += (component -> convertedInstances)
    }
  }

  def initFU(): Unit = {
    val fuJson = (json \ "ARCH" \ "FU").get.as[JsObject]
    val fuInputsJson = (fuJson \ "INPUTS").get.as[JsArray]
    val fuOutputJson = (fuJson \ "OUTPUTS").get.as[JsArray]
    for (fuInputVal <- fuInputsJson.value) {
      val fuInput = fuInputVal.as[String]
      aluInPorts = aluInPorts :+ fuInput
    }
    aluOutPort = fuOutputJson.value(0).as[String]
  }

  def initRF(): Unit = {
    val rfJson = (json \ "ARCH" \ "RF").get.as[JsObject]
    val rfInputsJson = (rfJson \ "INPUTS").get.as[JsArray]
    val rfOutputJson = (rfJson \ "OUTPUTS").get.as[JsArray]
    for (rfInputVal <- rfInputsJson.value) {
      val rfInput = rfInputVal.as[String]
      rfInPorts = rfInPorts :+ rfInput
    }
    for (rfOutputVal <- rfOutputJson.value) {
      val rfOutput = rfOutputVal.as[String]
      rfOutPorts = rfOutPorts :+ rfOutput
    }
  }

  def show() : Unit = {
    println("PE CONNECTIONS:")
    for ((src, dsts) <- peConnections) {
      print(src + ": ")
      println(dsts.mkString(", "))
    }
    println("MEM PE CONNECTIONS:")
    for ((src, dsts) <- memPEConnections) {
      print(src + ": ")
      println(dsts.mkString(", "))
    }
    println("PE SUBMODS:")
    for ((comp, instances) <- submods) {
      print(comp + ": ")
      println(instances.mkString(", "))
    }
    println("MEM PE SUBMODS:")
    for ((comp, instances) <- memPESubmods) {
      print(comp + ": ")
      println(instances.mkString(", "))
    }
    println(aluInPorts.mkString(", "))
    println(aluOutPort)

    println(rfInPorts.mkString(", "))
    println(rfOutPorts.mkString(", "))
  }
}