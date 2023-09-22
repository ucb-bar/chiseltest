// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.annotations._
import firrtl2.options.Dependency
import firrtl2.passes.{ExpandWhens, ExpandWhensAndCheck}
import firrtl2.stage.Forms
import firrtl2.stage.TransformManager.TransformDependency
import firrtl2.transforms.DedupModules

import scala.collection.mutable

case class LineCoverageAnnotation(target: ReferenceTarget, lines: Coverage.Lines)
    extends SingleTargetAnnotation[ReferenceTarget]
    with CoverageInfo {
  override def duplicate(n: ReferenceTarget) = copy(target = n)
}

case object SkipLineCoverageAnnotation extends NoTargetAnnotation

object LineCoveragePass extends Transform {
  val Prefix = "l"

  override def prerequisites: Seq[TransformDependency] = Forms.Checks
  // we can run after deduplication which should make things faster
  override def optionalPrerequisites: Seq[TransformDependency] = Seq(Dependency[DedupModules])
  // line coverage does not work anymore after whens have been expanded
  override def optionalPrerequisiteOf: Seq[TransformDependency] =
    Seq(Dependency[ExpandWhensAndCheck], Dependency(ExpandWhens))
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    if (state.annotations.contains(SkipLineCoverageAnnotation)) {
      logger.info("[LineCoverage] skipping due to SkipLineCoverage annotation")
      return state
    }
    val newAnnos = mutable.ListBuffer[Annotation]()
    val c = CircuitTarget(state.circuit.main)
    val ignoreMods = Coverage.collectModulesToIgnore(state)
    val circuit = state.circuit.mapModule(onModule(_, c, newAnnos, ignoreMods))
    val annos = newAnnos.toList ++ state.annotations
    CircuitState(circuit, annos)
  }

  private case class ModuleCtx(
    annos:     mutable.ListBuffer[Annotation],
    namespace: Namespace,
    m:         ModuleTarget,
    clk:       ir.Expression)

  private def onModule(m: ir.DefModule, c: CircuitTarget, annos: mutable.ListBuffer[Annotation], ignore: Set[String])
    : ir.DefModule =
    m match {
      case mod: ir.Module if !ignore(mod.name) =>
        Builder.findClock(mod, logger) match {
          case Some(clock) =>
            val namespace = Namespace(mod)
            namespace.newName(Prefix)
            val ctx = ModuleCtx(annos, namespace, c.module(mod.name), clock)
            // we always cover the body, even if the module only contains nodes and cover statements
            val bodyInfo = onStmt(mod.body, ctx).copy(_2 = true)
            val body = addCover(bodyInfo, ctx)
            mod.copy(body = body)
          case None =>
            mod
        }
      case other => other
    }

  private def onStmt(s: ir.Statement, ctx: ModuleCtx): (ir.Statement, Boolean, Seq[ir.Info]) = s match {
    case c @ ir.Conditionally(_, _, conseq, alt) =>
      val truInfo = onStmt(conseq, ctx)
      val falsInfo = onStmt(alt, ctx)
      val doCover = truInfo._2 || falsInfo._2
      val stmt = c.copy(conseq = addCover(truInfo, ctx), alt = addCover(falsInfo, ctx))
      (stmt, doCover, List(c.info))
    case ir.Block(stmts) =>
      val s = stmts.map(onStmt(_, ctx))
      val block = ir.Block(s.map(_._1))
      val doCover = s.map(_._2).foldLeft(false)(_ || _)
      val infos = s.flatMap(_._3)
      (block, doCover, infos)
    case ir.EmptyStmt => (ir.EmptyStmt, false, List())
    // if there is only a cover statement, we do not explicitly try to cover that line
    case v: ir.Verification if v.op == ir.Formal.Cover => (v, false, List(v.info))
    // nodes are always side-effect free, so they should not be covered unless there is another operation in the block
    case n:     ir.DefNode => (n, false, List(n.info))
    case other: ir.HasInfo => (other, true, List(other.info))
    case other => (other, false, List())
  }

  private def addCover(info: (ir.Statement, Boolean, Seq[ir.Info]), ctx: ModuleCtx): ir.Statement = {
    val (stmt, doCover, infos) = info
    if (!doCover) { stmt }
    else {
      val name = ctx.namespace.newName(Prefix)
      val lines = Coverage.infosToLines(infos)
      ctx.annos.prepend(LineCoverageAnnotation(ctx.m.ref(name), lines))
      val cover =
        ir.Verification(ir.Formal.Cover, ir.NoInfo, ctx.clk, Utils.True(), Utils.True(), ir.StringLit(""), name)
      ir.Block(cover, stmt)
    }
  }
}
