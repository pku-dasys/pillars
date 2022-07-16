package tetriski.pillars.mapping

import java.io.FileWriter
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import org.apache.logging.log4j.LogManager
import com.microsoft.z3._

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
  val mrrgFuncFanoutId = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgRoutName = ArrayBuffer[String]()
  val mrrgRoutFaninId = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgRoutFanoutId = ArrayBuffer[ArrayBuffer[Int]]()

  val mrrgLatency = Map[String, Int]()
  val mrrgFuncOpcodes = ArrayBuffer[ArrayBuffer[Int]]()

  val MRRG_ROUT_NODE_TYPE = 0
  val MRRG_FUNC_NODE_TYPE = 1
  val mrrgFuncFaninType = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgFuncFanoutType = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgRoutFaninType = ArrayBuffer[ArrayBuffer[Int]]()
  val mrrgRoutFanoutType = ArrayBuffer[ArrayBuffer[Int]]()

  val dfgRelativeSkewMap = Map[String, Int]()
  val dfgCommutatedSet = Set[String]()
  val waitSkewMap = Map[String, Int]()
  val dfgLatencyMap = Map[String, Int]()

  val fixedMapRelation = Map[Int, Set[Int]]()
  val regMap = Map[Int, Int]()
  val regConnect = Map[Int, ArrayBuffer[Int]]()
  val func2regMap = Map[Int, ArrayBuffer[Int]]()
  val reg2funcMap = Map[Int, ArrayBuffer[ArrayBuffer[Int]]]()
  val func2funcMap = Map[Int, ArrayBuffer[ArrayBuffer[Int]]]()

  def demo() {
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

  def getOmtModel(
      ctx: Context,
      separatedPR: Boolean,
      scheduleControl: Boolean
  ): Array[Optimize] = {
    val constrPlace = ArrayBuffer[BoolExpr]()
    val constrRoute = ArrayBuffer[BoolExpr]()

    val varF = Map[(Int, Int), IntExpr]() // funcNode variables
    val varR = Map[(Int, Int), IntExpr]() // routNode variables
    val varS = Map[(Int, Int, Int), IntExpr]() // path variables

    // initialize F(*,*)
    for ((funcNodeName, funcNodeId) <- mrrgFuncName.zipWithIndex;
         (opNodeName, opNodeId) <- dfgOpNodeName.zipWithIndex) {
      val isLegal =
        if (mrrgFuncOpcodes(funcNodeId)
              .indexOf(dfgOpNodeOpcode(opNodeId)) >= 0) {
          if (fixedMapRelation.contains(opNodeId)) {
            if (fixedMapRelation(opNodeId).contains(funcNodeId)) true else false
          } else true
        } else false
      if (isLegal) { // implicit constraits of functional legality
        val name = s"F( ${funcNodeName}, ${opNodeName} )"
        val v = ctx.mkIntConst(name)
        varF.put((funcNodeId, opNodeId), v)
      }
    }
    // initialize R(*,*)
    for ((routNodeName, routNodeId) <- mrrgRoutName.zipWithIndex;
         (valNodeName, valNodeId) <- dfgValNodeName.zipWithIndex) {
      val name = s"R( ${routNodeName}, ${valNodeName} )"
      val v = ctx.mkIntConst(name)
      varR.put((routNodeId, valNodeId), v)

      // initialize S(*,*,*)
      for (opNodeId <- dfgValNodeOut(valNodeId)) {
        val opNodeName = dfgOpNodeName(opNodeId)
        val name = s"S( ${routNodeName}, ${valNodeName}, ${opNodeName} )"
        val v = ctx.mkIntConst(name)
        varS.put((routNodeId, valNodeId, opNodeId), v)
      }
    }

    val zero = ctx.mkInt(0)
    val one = ctx.mkInt(1)

    def bound[T](varList: Map[T, IntExpr]): Iterable[BoolExpr] = {
      varList.map {
        // bound v in {0,1}
        case (_, v) => ctx.mkAnd(ctx.mkLe(zero, v), ctx.mkLe(v, one))
      }
    }
    constrPlace.appendAll(bound(varF))
    constrRoute.appendAll(bound(varR))
    constrRoute.appendAll(bound(varS))

    constrPlace.appendAll(constrFunctionExclusivity(ctx, varF))
    constrPlace.appendAll(constrOperationPlacement(ctx, varF))

    constrRoute.appendAll(constrRoutingResource(ctx, varR, varS))
    constrRoute.appendAll(constrRoutingExclusivity(ctx, varR))
    constrRoute.appendAll(constrMultiplexerExclusivity(ctx, varR))

    if (separatedPR) {
      logger.fatal("FIXME: separatedPR not yet supported")
      //constrFanoutRouting(modelR, S, afterPlacementF, false);
      //constrInitialFanout(modelR, S, afterPlacementF, false);
      // hack
      constrRoute.appendAll(constrFanoutRouting(ctx, varS, varF))
      constrRoute.appendAll(constrInitialFanout(ctx, varS, varF))
    } else {
      constrRoute.appendAll(constrFanoutRouting(ctx, varS, varF))
      constrRoute.appendAll(constrInitialFanout(ctx, varS, varF))
    }

    if (scheduleControl) {
      logger.fatal("FIXME: scheduleControl not yet supported")
    }

    val objRoute = ctx.mkAdd(varR.map(_._2).toSeq: _*)

    val solver = ctx.mkOptimize()
    //solver.MkMinimize(objRoute)
    solver.MkMinimize(ctx.mkInt(0))
    (constrPlace ++ constrRoute).foreach(solver.Add(_))
    logger.trace(solver.toString())

    logger.debug(solver.Check())

    //logger.debug(solver.getModel())
    val model = solver.getModel()
    def logVar(v: IntExpr) = {
      val c = model.getConstInterp(v)
      if (c.equals(one)) {
        logger.debug(s"${c} = ${v.getFuncDecl().getName()}")
      }
    }
    for ((_, v) <- varF) logVar(v)
    for ((_, v) <- varR) logVar(v)
    for ((_, v) <- varS) logVar(v)

    Array(solver, solver) // hack
  }

  def constrFunctionExclusivity(
      ctx: Context,
      varF: Map[(Int, Int), IntExpr]
  ): Iterable[BoolExpr] = {
    val one = ctx.mkInt(1)
    varF.groupBy { case ((funcNodeId, _), _) => funcNodeId }.flatMap {
      case (_, vf) => {
        val varList = vf.map(_._2).toSeq
        if (varList.size > 0) {
          val sum = ctx.mkAdd(varList: _*)
          // every MRRG funcNode is used at most once:
          //   sum_* F(funcNodeId,*) <= 1
          Some(ctx.mkLe(sum, one))
        } else {
          None
        }
      }
    }
  }

  def constrOperationPlacement(
      ctx: Context,
      varF: Map[(Int, Int), IntExpr]
  ): Iterable[BoolExpr] = {
    val one = ctx.mkInt(1)
    varF.groupBy { case ((_, opNodeId), _) => opNodeId }.map {
      case (_, vf) => {
        val varList = vf.map(_._2).toSeq
        assert(varList.size > 0)
        val sum = ctx.mkAdd(varList: _*)
        // every DFG opNode is placed exactly once:
        //   sum_* F(*,opNodeId) == 1
        ctx.mkEq(sum, one)
      }
    }
  }

  def constrRoutingResource(
      ctx: Context,
      varR: Map[(Int, Int), IntExpr],
      varS: Map[(Int, Int, Int), IntExpr]
  ): Iterable[BoolExpr] = {
    val vs = varS.groupBy { case ((x, y, _), _) => (x, y) }
    varR.flatMap {
      case ((routNodeId, valNodeId), r) => {
        vs((routNodeId, valNodeId)).map {
          case (_, s) =>
            // path (valNode ~> opNode) implies MRRG routNode usage:
            //   S(routeNodeId,valNodeId,*) <= R(routeNodeId,valNodeId)
            ctx.mkLe(s, r)
        }
      }
    }
  }

  def constrRoutingExclusivity(
      ctx: Context,
      varR: Map[(Int, Int), IntExpr]
  ): Iterable[BoolExpr] = {
    val one = ctx.mkInt(1)
    varR.groupBy { case ((routNodeId, _), _) => routNodeId }.flatMap {
      case (_, vr) => {
        val varList = vr.map(_._2).toSeq
        if (varList.size > 0) {
          val sum = ctx.mkAdd(varList: _*)
          // every MRRG routNode is used at most once:
          //   sum_* R(routNodeId,*) <= 1
          Some(ctx.mkLe(sum, one))
        } else {
          None
        }
      }
    }
  }

  def constrMultiplexerExclusivity(
      ctx: Context,
      varR: Map[(Int, Int), IntExpr]
  ): Iterable[BoolExpr] = {
    val one = ctx.mkInt(1)
    varR.flatMap {
      case ((routNodeId, valNodeId), r) => {
        val faninRoutNodeIds = mrrgRoutFaninId(routNodeId).filter(
          mrrgRoutFaninType(_) == MRRG_ROUT_NODE_TYPE
        )
        if (faninRoutNodeIds.size > 1) {
          val varList = faninRoutNodeIds.map(i => varR((i, valNodeId)))
          val sum = ctx.mkAdd(varList: _*)
          Seq(
            // active routNode imples active fan-in routNodes:
            //   R(routNodeId, valNodeId) <= sum_{* in fanin(routeNodeId)} R(*, valNodeId)
            ctx.mkLe(r, sum),
            // no more than one active fan-in routNodes for the same valNode:
            //   (may happen when a value from one MUX input loops back to the other MUX input)
            //   sum_{* in fanin(routeNodeId)} R(*, valNodeId) <= 1
            ctx.mkLe(sum, one)
          )
        } else {
          None
        }
      }
    }
  }

  def constrFanoutRouting(
      ctx: Context,
      varS: Map[(Int, Int, Int), IntExpr],
      varF: Map[(Int, Int), IntExpr]
  ): Iterable[BoolExpr] = {
    val exprList = ArrayBuffer[BoolExpr]()
    for (routNodeId <- mrrgRoutName.indices; valNodeId <- dfgValNodeName.indices) {
      for ((opNodeId, i) <- dfgValNodeOut(valNodeId).zipWithIndex) {
        val operand = dfgValNodeOutputOperand(valNodeId)(i) // what is operand?
        val varList = ArrayBuffer[IntExpr]()
        for ((fanoutId, j) <- mrrgRoutFanoutId(routNodeId).zipWithIndex) {
          mrrgRoutFanoutType(routNodeId)(j) match {
            case MRRG_ROUT_NODE_TYPE =>
              varList.append(varS((fanoutId, valNodeId, opNodeId)))
            case MRRG_FUNC_NODE_TYPE => {
              if (mrrgFuncFaninId(fanoutId).size > operand) {
                if (dfgCommutativeSet.contains(opNodeId) || mrrgFuncFaninId(
                      fanoutId
                    )(operand) == routNodeId) {
                  if (varF.contains((fanoutId, opNodeId))) {
                    varList.append(varF((fanoutId, opNodeId)))
                  }
                }
              }
            }
          }
        }
        if (varList.size > 0) {
          val sum = ctx.mkAdd(varList: _*)
          exprList.append(
            ctx.mkLe(varS((routNodeId, valNodeId, opNodeId)), sum)
          )
        }
      }
    }
    exprList
  }

  def constrInitialFanout(
      ctx: Context,
      varS: Map[(Int, Int, Int), IntExpr],
      varF: Map[(Int, Int), IntExpr]
  ): Iterable[BoolExpr] = {
    val exprList = ArrayBuffer[BoolExpr]()
    varF.foreach {
      case ((funcNodeId, opNodeId), f) => {
        if (dfgOpNodeOut(opNodeId) != -1) {
          val valNodeId = dfgOpNodeOut(opNodeId)
          for (funcNodeFanoutId <- mrrgFuncFanoutId(funcNodeId)) {
            for (valNodeFanoutId <- dfgValNodeOut(valNodeId)) {
              // F(funcNodeId,opNodeId) == S(funcNodeFanoutId,valNodeId,valNodeFanoutId)
              //   where funcNodeId->funcNodeFanoutId and opNodeId->valNodeId
              val expr = ctx.mkEq(
                f,
                varS((funcNodeFanoutId, valNodeId, valNodeFanoutId))
              )
              exprList.append(expr)
            }
          }
        }
      }
    }
    exprList
  }

  def omtMap(fw: FileWriter): Double = {
    logger.fatal("FIXME: def omtMap(fw: FileWriter): Double")
    -1
  }

  def omtMap(
      separatedPR: Boolean,
      scheduleControl: Boolean
  ): ArrayBuffer[ArrayBuffer[Int]] = {
    val ctx = new Context() // TO-DO: provide configuration parameters
    Global.setParameter("parallel.enable", "true");
    getOmtModel(ctx, separatedPR, scheduleControl)
    ctx.close()

    logger.fatal("FIXME: complete omtMap(separatedPR, scheduleControl)")
    ArrayBuffer[ArrayBuffer[Int]]()
  }
}
