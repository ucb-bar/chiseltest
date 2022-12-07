// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal

import chisel3._
import chisel3.experimental.{annotate, ChiselAnnotation, RunFirrtlTransform}
import chisel3.util.{log2Ceil, ShiftRegisters}
import chiseltest.formal.FirrtlUtils.findClockAndReset
import firrtl.annotations._
import firrtl._
import firrtl.options.Dependency
import firrtl.transforms.EnsureNamedStatements

import scala.collection.mutable

/** Delays a signal for verification purposes.
  * Any stop, assert, assign or cover statement that the result is used in will be automatically guarded
  * by the time necessary for all past values to become available.
  * E.g.:
  * @example {{{
  *   assert(past(out) === past(past(in)))
  * }}}
  * is equivalent to:
  * @example {{{
  *   when(cyclesSinceReset >= 2.U) {
  *     assert(RegNext(out) === RegNext(RegNext(in)))
  *   }
  * }}}
  *
  * @warn: Currently `Past` is only supported in [[chisel3.Module]] with a single reset or clock domains.
  *        Please file an issue with a detailed description of your use-case and the intended behavior
  *        and we will try to add support for you usage of `withReset` or `withClock`.
  */
object past {
  def apply[T <: Data](signal: T, delayBy: Int = 1): T = {
    require(delayBy >= 0, f"We cannot see into the future!")
    if (delayBy == 0) {
      signal
    } else {
      // we create a shift register to delay the signal + an annotation to ensure it will be used safely!
      val out = (0 until delayBy).foldLeft(signal)((p, _) => makeReg(p))
      // make a wire so that no-one can overwrite the shift register
      WireInit(out)
    }
  }

  private def makeReg[T <: Data](prev: T): T = {
    val past = RegNext(prev)
    annotate(new ChiselAnnotation with RunFirrtlTransform {
      override def transformClass = classOf[SafePastSignalsPass]
      override def toFirrtl = PastSignalAnnotation(past.toTarget)
    })
    past
  }
}

object rose {

  /** returns true if the signal was low in the previous cycle and is now high */
  def apply(signal: Bool): Bool = !past(signal) && signal
}

object fell {

  /** returns true if the signal was high in the previous cycle and is now low */
  def apply(signal: Bool): Bool = past(signal) && !signal
}

object stable {

  /** returns true if the signal was the same in the previous cycle as it is now */
  def apply(signal: Data): Bool = past(signal).asUInt === signal.asUInt
}

object changed {

  /** returns true if the signal was the different in the previous cycle as it is now */
  def apply(signal: Data): Bool = past(signal).asUInt =/= signal.asUInt
}

case class PastSignalAnnotation(target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): PastSignalAnnotation = copy(target = n)
}

class SafePastSignalsPass extends Transform with DependencyAPIMigration {
  override def prerequisites =
    firrtl.stage.Forms.LowForm :+ Dependency(EnsureNamedStatements)
  override def invalidates(a: Transform) = false

  override def execute(state: CircuitState): CircuitState = {
    val (pastAnnos, otherAnnos) = state.annotations.partition(_.isInstanceOf[PastSignalAnnotation])
    if (pastAnnos.isEmpty) return state

    val pastAnnosByModule = pastAnnos.collect {
      case a @ PastSignalAnnotation(target) if target.circuit == state.circuit.main => target.module -> a
    }.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
    val circuit = state.circuit.mapModule(m => onModule(m, pastAnnosByModule.getOrElse(m.name, Seq())))
    state.copy(circuit = circuit, annotations = otherAnnos)
  }

  private def onModule(dm: ir.DefModule, annos: Seq[PastSignalAnnotation]): ir.DefModule = dm match {
    case m: ir.Module if annos.nonEmpty =>
      val isPastReg = annos.map(_.target.ref).toSet
      // if there are no past registers in this module, there is nothing to do
      if (isPastReg.isEmpty) return m

      val delays = findPastDelays(m, isPastReg)
      // if no positive delays were found, there is nothing to do
      if (delays.isEmpty) {
        logger.warn(
          s"[${m.name}] signals delayed with Past(...) aren't used in any assert/assume statements: "
            + isPastReg.mkString(", ")
        )
        return m
      }

      // create a module level cycle counter
      val namespace = Namespace(m)
      val (clock, reset) = findClockAndReset(m)
      val (counterStmts, delayExpr) = makeCycleCounter(delays.map(_._2), namespace, clock, reset)

      // map delays to guards and add guards to statements
      val guards = delays.map { case (n, d) => n -> delayExpr(d) }.toMap
      val bodyWithGuards = m.body.mapStmt(addDelayGuard(m.name, _, guards.get))

      // combine all statements
      val body = ir.Block(counterStmts :+ bodyWithGuards)
      m.copy(body = body)
    case other => other
  }

  import FirrtlUtils._

  private def addDelayGuard(m: String, s: ir.Statement, guard: String => Option[ir.Expression]): ir.Statement =
    s match {
      case v: ir.Verification =>
        guard(v.name) match {
          case None => v
          case Some(g) =>
            logger.info(
              s"[$m] ${v.op} statement ${v.name} is disabled until ${g.serialize} cycles after reset. ${v.info.serialize}"
            )
            v.withEn(Utils.and(v.en, g))
        }
      case other => other.mapStmt(addDelayGuard(m, _, guard))
    }

  private def makeCycleCounter(
    delays:    Seq[Int],
    namespace: Namespace,
    clock:     ir.Expression,
    reset:     ir.Expression
  ): (Seq[ir.Statement], Map[Int, ir.Expression]) = {
    val maxDelay = delays.max
    val (count, isActive, countStmts) = makeSaturatingCounter(
      ir.NoInfo,
      namespace.newName("_cycles"),
      namespace.newName("_cycles_active"),
      maxDelay,
      clock,
      reset
    )
    val width = count.tpe.asInstanceOf[ir.UIntType].width
    val stmtsAndRefs = delays.distinct.map { after =>
      val isAfter = if (after == maxDelay) { Utils.not(isActive) }
      else {
        // count >= after
        ir.DoPrim(PrimOps.Geq, List(count, ir.UIntLiteral(after, width)), List(), Utils.BoolType)
      }
      val node = ir.DefNode(ir.NoInfo, namespace.newName(s"_after_$after"), isAfter)
      (node, (after, ir.Reference(node).copy(flow = SourceFlow)))
    }
    (countStmts ++ stmtsAndRefs.map(_._1), stmtsAndRefs.map(_._2).toMap)
  }

  // determines the number of `past` delays feeding into a verification statement
  private def findPastDelays(m: ir.Module, isPastReg: String => Boolean): Seq[(String, Int)] = {
    // determine the names of verification statements + the graph of register dependencies
    val analysis =
      AnalysisCtx(mutable.HashMap[String, Set[String]](), mutable.ArrayBuffer[String](), mutable.HashSet[String]())
    m.foreachStmt(findRegisterInputs(_, analysis))

    analysis.verificationOps
      .map(name => name -> findPastDelay(name, analysis.inp.get, isPastReg))
      .filter(_._2 > 0)
      .toSeq
  }

  // this function resolves combinatorial read ports
  private def getFanIn(from: String, inp: String => Option[Set[String]]): Seq[String] = inp(from) match {
    case None => Seq()
    case Some(localFanIn) =>
      val (readPort, reg) = localFanIn.toSeq.partition(_.contains('.'))
      reg ++ readPort.flatMap(getFanIn(_, inp))
  }

  private def findPastDelay(from: String, inp: String => Option[Set[String]], isPastReg: String => Boolean): Int = {
    val localFanIn = getFanIn(from, inp)
    val pastRegChildren = localFanIn.filter(isPastReg)
    if (pastRegChildren.isEmpty) { 0 }
    else { pastRegChildren.map(findPastDelay(_, inp, isPastReg)).max + 1 }
  }

  private case class AnalysisCtx(
    inp:             mutable.HashMap[String, Set[String]],
    verificationOps: mutable.ArrayBuffer[String],
    combReadMem:     mutable.HashSet[String])

  private def findRegisterInputs(s: ir.Statement, ctx: AnalysisCtx): Unit = s match {
    case ir.DefNode(_, name, value) => ctx.inp(name) = getRegisters(value, ctx)
    case w: ir.DefWire => throw new RuntimeException(s"Unexpected wire: $w")
    case ir.Connect(_, loc, expr) =>
      loc match {
        case ir.Reference(name, _, RegKind, _) =>
          ctx.inp(name) = getRegisters(expr, ctx)
        case ir.SubField(ir.SubField(ir.Reference(mem, _, MemKind, _), port, _, _), _, _, _)
            if ctx.combReadMem.contains(mem) =>
          ctx.inp(s"$mem.$port") = ctx.inp.getOrElse(s"$mem.$port", Set()) | getRegisters(expr, ctx)
        case _ =>
        // ignore other connections
      }
    case v: ir.Verification =>
      ctx.verificationOps.append(v.name)
      ctx.inp(v.name) = Seq(v.pred, v.en).map(getRegisters(_, ctx)).reduce(_ | _)
    case m: ir.DefMemory if m.readLatency == 0 => ctx.combReadMem.add(m.name)
    case other => other.foreachStmt(findRegisterInputs(_, ctx))
  }

  private def getRegisters(e: ir.Expression, ctx: AnalysisCtx): Set[String] = e match {
    case ir.Reference(name, _, RegKind, _) => Set(name)
    case ir.Reference(name, _, _, _)       => ctx.inp.getOrElse(name, Set())
    case ir.SubField(ir.SubField(ir.Reference(mem, _, MemKind, _), port, _, _), field, _, _)
        if ctx.combReadMem.contains(mem) =>
      // combinatorial read ports are not necessarily in SSA and need to be resolved at the end of the analysis
      Set(s"$mem.$port")
    case _: ir.SubField => Set() // ignoring sub modules
    case ir.Mux(cond, tval, fval, _) => Seq(cond, tval, fval).map(getRegisters(_, ctx)).reduce(_ | _)
    case ir.DoPrim(_, args, _, _)    => args.map(getRegisters(_, ctx)).reduce(_ | _)
    case ir.ValidIf(cond, value, _)  => Seq(cond, value).map(getRegisters(_, ctx)).reduce(_ | _)
    case _: ir.Literal => Set()
  }

}
