package tetriski.pillars.core

import java.io.{File, PrintWriter}

import play.api.libs.json._
import tetriski.pillars.mapping.DFG
import tetriski.pillars.testers.AppTestHelper

import scala.io.Source

object JsonParser extends App {
  def readJson(filename: String = "runtime.json") = {
    val in = Source.fromFile(filename).getLines().reduce(_ + _)
    val json = Json.parse(in)
    RuntimeInfo.read(json)
  }

  def writeJson(runtimeInfo: RuntimeInfo, filename: String = "runtime.json"): Unit = {
    val json = RuntimeInfo.write(runtimeInfo)
    val out = Json.prettyPrint(json)
    val writer = new PrintWriter(new File(filename))
    writer.write(out)
    writer.close()
  }

  def dumpRuntimeInfo(simulationHelper: SimulationHelper, appTestHelper: AppTestHelper, dfg: DFG): Unit = {
    val inputOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.INPUT).map(op => op.name)
    val inputPortNums = simulationHelper.inputPorts
    val inputToPortData = appTestHelper.getInputPortData()
    val inputToPort = (0 until inputOpNames.size).map(i => InputToPort(inputOpNames(i),
      inputToPortData(inputPortNums(i)).toList)).toList

    val outputOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.OUTPUT).map(op => op.name)
    val outputPortNums = simulationHelper.outPorts
    val outputFromPortData = appTestHelper.getOutPortRefs()
    val outputFromPort = (0 until outputOpNames.size).map(i => OutputFromPort(outputOpNames(i),
      outputFromPortData(outputPortNums(i)).toList)).toList

    val inputToSRAM = appTestHelper.inDataMap.map(m => InputToSRAM(m._1(0), m._1(1), m._2.toList)).toList

    val outputFromSRAM = appTestHelper.outDataMap.map(m => OutputFromSRAM(m._1(0), m._1(1), m._2.toList)).toList

    val constOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.CONST).map(op => op.name)
    val constValueArray = simulationHelper.constArray
    val constValue = (0 until constOpNames.size).map(i => ConstValue(constOpNames(i), constValueArray(i))).toList

    val counterOpNames = dfg.opNodes.filter(op => op.opcode == OpEnum.INCR).map(op => op.name)
    val counterArray = simulationHelper.counterArray
    val counterConfig = (0 until counterOpNames.size).map(i => CounterConfig(constOpNames(i),
      counterArray(i).init, counterArray(i).step, counterArray(i).end, counterArray(i).freq)).toList

    val runtimeInfo = RuntimeInfo(inputToPort, outputFromPort, inputToSRAM
      , outputFromSRAM, constValue, counterConfig)

    writeJson(runtimeInfo)
  }

}

case class InputToPort(opName: String,
                       data: List[Int])

object InputToPort {
  implicit val inputToPortFormats = Json.format[InputToPort]

  def write(inputToPort: InputToPort) = {
    Json.toJson(inputToPort)
  }

  def read(jsonInputToPort: JsValue) = {
    jsonInputToPort.as[InputToPort]
  }
}

case class OutputFromPort(opName: String,
                          expectedData: List[Int])

object OutputFromPort {
  implicit val outputFromPortFormats = Json.format[OutputFromPort]

  def write(outputFromPort: OutputFromPort) = {
    Json.toJson(outputFromPort)
  }

  def read(jsonOutputFromPort: JsValue) = {
    jsonOutputFromPort.as[OutputFromPort]
  }
}

case class ConstValue(opName: String,
                      value: Int)

object ConstValue {
  implicit val constValue = Json.format[ConstValue]

  def write(constValue: ConstValue) = {
    Json.toJson(constValue)
  }

  def read(jsonConstValue: JsValue) = {
    jsonConstValue.as[ConstValue]
  }
}

case class CounterConfig(opName: String,
                         init: Int,
                         step: Int,
                         end: Int,
                         freq: Int) {
  def getConfig(width: Int) = {
    val config: BigInt = (freq << (width * 3)) + (end << (width * 2)) + (step << width) + init
    config
  }
}

object CounterConfig {
  implicit val counterConfig = Json.format[CounterConfig]

  def write(counterConfig: CounterConfig) = {
    Json.toJson(counterConfig)
  }

  def read(jsonCounterConfig: JsValue) = {
    jsonCounterConfig.as[CounterConfig]
  }
}

case class InputToSRAM(SRAMID: Int,
                       offset: Int,
                       data: List[Int])

object InputToSRAM {
  implicit val inputToSRAMFormats = Json.format[InputToSRAM]

  def write(inputToSRAM: InputToSRAM) = {
    Json.toJson(inputToSRAM)
  }

  def read(jsonInputToSRAM: JsValue) = {
    jsonInputToSRAM.as[InputToSRAM]
  }
}

case class OutputFromSRAM(SRAMID: Int,
                          offset: Int,
                          expectedData: List[Int])

object OutputFromSRAM {
  implicit val outputFromSRAMFormats = Json.format[OutputFromSRAM]

  def write(outputFromSRAM: OutputFromSRAM) = {
    Json.toJson(outputFromSRAM)
  }

  def read(jsonOutputFromSRAM: JsValue) = {
    jsonOutputFromSRAM.as[OutputFromSRAM]
  }
}

case class RuntimeInfo(inputToPort: List[InputToPort],
                       outputFromPort: List[OutputFromPort],
                       inputToSRAM: List[InputToSRAM],
                       outputFromSRAM: List[OutputFromSRAM],
                       constValue: List[ConstValue],
                       counterConfig: List[CounterConfig]
                      )

object RuntimeInfo {
  def write(runtimeInfo: RuntimeInfo) = {
    JsObject(Seq(
      "inputToPort" -> Json.toJson(runtimeInfo.inputToPort),
      "outputFromPort" -> Json.toJson(runtimeInfo.outputFromPort),
      "inputToSRAM" -> Json.toJson(runtimeInfo.inputToSRAM),
      "outputFromSRAM" -> Json.toJson(runtimeInfo.outputFromSRAM),
      "constValue" -> Json.toJson(runtimeInfo.constValue),
      "counterConfig" -> Json.toJson(runtimeInfo.counterConfig)
    ))
  }

  def read(jsonRuntimeInfo: JsValue) = {
    val inputToPort = (jsonRuntimeInfo \ "inputToPort").as[List[InputToPort]]
    val outputFromPort = (jsonRuntimeInfo \ "outputFromPort").as[List[OutputFromPort]]
    val inputToSRAM = (jsonRuntimeInfo \ "inputToSRAM").as[List[InputToSRAM]]
    val outputFromSRAM = (jsonRuntimeInfo \ "outputFromSRAM").as[List[OutputFromSRAM]]
    val constValue = (jsonRuntimeInfo \ "constValue").as[List[ConstValue]]
    val counterConfig = (jsonRuntimeInfo \ "counterConfig").as[List[CounterConfig]]
    RuntimeInfo(inputToPort, outputFromPort, inputToSRAM, outputFromSRAM, constValue, counterConfig)
  }
}
