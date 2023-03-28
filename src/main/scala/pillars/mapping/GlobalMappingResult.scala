package pillars.mapping

import java.io.FileWriter

import scala.collection.mutable.ArrayBuffer

object GlobalMappingResult {
  var archName = ""
  var II = 0
  var mappingDfg = Array[String]()
  val mappingResult = new ArrayBuffer[String]()
  val elapsedTimes = new ArrayBuffer[Double]()
  val usedBypassALUs = new ArrayBuffer[Int]()
  val usedFuncALUs = new ArrayBuffer[Int]()

  def addResult(result: String, currentElapsedTime: Double, usedBypassALU: Int, usedFuncALU: Int): Unit = {
    mappingResult.append(result)
    elapsedTimes.append(currentElapsedTime)
    usedBypassALUs.append(usedBypassALU)
    usedFuncALUs.append(usedFuncALU)
  }

  def reset(): Unit = {
    archName = ""
    II = 0
    mappingDfg = Array[String]()
    mappingResult.clear()
    elapsedTimes.clear()
    usedBypassALUs.clear()
    usedFuncALUs.clear()
  }

  def getSuccessCount(): Double = {
    var count = 0
    for (result <- mappingResult) {
      if (result.contains("success")) {
        count += 1
      }
    }
    count
  }

  def initInfo(dfgs: Array[String], name: String, ii: Int): Unit = {
    reset()
    II = ii
    archName = name
    mappingDfg = dfgs
  }

  def printEach(filename: String): Unit = {
    val outputFile = new FileWriter(filename, true)
    for (i <- 0 until mappingDfg.size) {
      outputFile.write(archName + "\t\t" + II + "\t\t" + mappingDfg(i) + "\t\t"
        + mappingResult(i) + "\t\t" + usedBypassALUs(i) + "\t\t" + usedFuncALUs(i) + "\t\t" + elapsedTimes(i) + "\n")
    }
    outputFile.flush()
    outputFile.close()
  }

  def printAll(filename: String): Unit = {
    val outputFile = new FileWriter(filename, true)
    val num: Double = mappingDfg.size
    val successCount = getSuccessCount()
    var successElapsedTime: Double = 0
    var usedFuncALU: Double = 0
    var usedBypassALU: Double = 0
    for (i <- 0 until mappingDfg.size) {
      if (mappingResult(i).contains("success")) {
        successElapsedTime += elapsedTimes(i)
        usedFuncALU += usedFuncALUs(i)
        usedBypassALU += usedBypassALUs(i)
      }
    }
    outputFile.write(archName + "\t\t" + II + "\t\t" + successCount / num + "\t\t" +
      elapsedTimes.reduce(_ + _) / num + "\t\t" + usedFuncALU / usedBypassALU
      + "\t\t" + successElapsedTime / successCount + "\n")
    outputFile.flush()
    outputFile.close()
  }
}
