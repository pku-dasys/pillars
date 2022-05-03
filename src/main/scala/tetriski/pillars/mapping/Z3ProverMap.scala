package tetriski.pillars.mapping

import java.io.FileWriter
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import org.apache.logging.log4j.LogManager
import com.microsoft.z3._

import tetriski.pillars.core.OpEnum.OpEnum

class Z3ProverMap {
  val logger = LogManager.getLogger(this.getClass().getName())

  var filename: String = ""

  var ii = 1
  var useRelativeSkew = false
  var skewLimit = 2
  var maxLatency = 15
  
  var result = "fail"
  var elapsedTime: Long = 0
  var usedBypassAlu = 0
  var usedFuncAlu = 0
  var ringCheckPass = false

  val dfgOpNodeName = ArrayBuffer[String]()
  val dfgValNodeName = ArrayBuffer[String]()

  val dfgOpNodeOut = ArrayBuffer[Int]()
  val dfgOpNodeOpcode = ArrayBuffer[Int]()
  val dfgValNodeOut = ArrayBuffer[ArrayBuffer[Int]]()
  val dfgValNodeOutputOperand = ArrayBuffer[ArrayBuffer[Int]]()

  val connectList = ArrayBuffer[ArrayBuffer[String]]()
  val dfgMultiInputMap = Map[String, ArrayBuffer[String]]()
  val dfgSelfJoinMap = Map[String, Int]()
  val dfgCommutativeSet = Set[Int]()
  val dfgOpOperand = Map[Int, ArrayBuffer[Int]]()
  val dfgValB2opMap = Map[String, Int]()

  val mrrgFuncName = ArrayBuffer[String]()
  val mrrgFuncFaninId = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgFuncFaninType = ArrayBuffer[ArrayBuffer[Int]]() // type in {0,1}?
  val mrrgFuncFanoutId = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgFuncFanoutType = ArrayBuffer[ArrayBuffer[Int]]() // type in {0,1}?
  val mrrgRoutName = ArrayBuffer[String]()
  val mrrgRoutFaninId = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgRoutFaninType = ArrayBuffer[ArrayBuffer[Int]]() // type in {0,1}?
  val mrrgRoutFanoutId = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgRoutFanoutType = ArrayBuffer[ArrayBuffer[Int]]() // type in {0,1}?
  val mrrgLatency = Map[String,Int]()
  val mrrgFuncOpcodes = ArrayBuffer[ArrayBuffer[OpEnum]]()

  val dfgRelativeSkewMap = Map[String, Int]()
  val dfgCommutatedSet = Set[String]()
  val waitSkewMap = Map[String, Int]()
  val dfgLatencyMap = Map[String, Int]()

  val regMap = Map[Int, Int]()
  val regConnect = Map[Int, ArrayBuffer[Int]]()
  val func2regMap = Map[Int, ArrayBuffer[Int]]()
  val reg2funcMap = Map[Int, ArrayBuffer[ArrayBuffer[Int]]]()
  val func2funcMap = Map[Int, ArrayBuffer[ArrayBuffer[Int]]]()

  def solve() {
    val ctx = new Context()
    val opt = ctx.mkOptimize()
    val x = ctx.mkIntConst("x")
    val y = ctx.mkIntConst("y")
    opt.Add(ctx.mkAnd(ctx.mkLt(y, ctx.mkInt(5))), ctx.mkLt(x, ctx.mkInt(2)))
    opt.Add(ctx.mkLt(ctx.mkSub(y, x), ctx.mkInt(1)))
    opt.MkMaximize(ctx.mkAdd(x, y))
    println(opt.Check())
    println(opt.getModel())
  }

  def omtMap(fw: FileWriter): Double = {
    logger.fatal("FIXME: def omtMap(fw: FileWriter): Double")
    -1
  }

  def omtMap(separatedPR: Boolean, scheduleControl: Boolean): ArrayBuffer[ArrayBuffer[Int]] = {
    logger.fatal("FIXME: def omtMap(separatedPR: Boolean, scheduleControl: Boolean): ArrayBuffer[ArrayBuffer[Int]]")
    ArrayBuffer[ArrayBuffer[Int]]()
  }
}