// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.annotations._
import firrtl2.options.Dependency
import firrtl2.stage.Forms
import firrtl2.stage.TransformManager.TransformDependency

import scala.collection.mutable

case class FsmCoverageAnnotation(
  stateReg:    ReferenceTarget,
  states:      Seq[(String, ReferenceTarget)],
  transitions: Seq[((String, String), ReferenceTarget)])
    extends MultiTargetAnnotation
    with CoverageInfo {
  override def targets = Seq(Seq(stateReg)) ++ states.map(s => Seq(s._2)) ++ transitions.map(t => Seq(t._2))

  override def duplicate(n: Seq[Seq[Target]]) = {
    assert(n.length == 1 + states.length + transitions.length)
    n.foreach(e => assert(e.length == 1, "Cover points and state registers should not be split up!"))
    val targets = n.map(_.head.asInstanceOf[ReferenceTarget])
    val r = copy(
      stateReg = targets.head,
      states = states.map(_._1).zip(targets.slice(1, states.length + 1)),
      transitions = transitions.map(_._1).zip(targets.drop(1 + states.length))
    )
    r
  }
}

case object SkipFsmCoverageAnnotation extends NoTargetAnnotation

object FsmCoveragePass extends Transform {
  val Prefix = "f"

  override def prerequisites: Seq[TransformDependency] =
    Forms.LowForm ++ Seq(Dependency(FsmInfoPass), Dependency(RegisterResetAnnotationPass))
  override def invalidates(a: Transform): Boolean = false

  override def execute(state: CircuitState): CircuitState = {
    if (state.annotations.contains(SkipFsmCoverageAnnotation)) {
      logger.info("[FsmCoverage] skipping due to SkipFsmCoverage annotation")
      return state
    }

    // collect FSMs in modules that are not ignored
    val ignoreMods = Coverage.collectModulesToIgnore(state)
    val infos = state.annotations.collect { case a: FsmInfoAnnotation if !ignoreMods(a.target.module) => a }

    // if there are no FSMs there is nothing to do
    if (infos.isEmpty) return state

    // instrument FSMs
    val registerResets = state.annotations.collect { case a: RegisterResetAnnotation => a }
    val newAnnos = mutable.ListBuffer[Annotation]()
    val c = CircuitTarget(state.circuit.main)
    val circuit = state.circuit.mapModule(onModule(_, c, newAnnos, infos, registerResets))
    state.copy(circuit = circuit, annotations = newAnnos.toList ++ state.annotations)
  }

  private def onModule(
    m:      ir.DefModule,
    c:      CircuitTarget,
    annos:  mutable.ListBuffer[Annotation],
    infos:  Seq[FsmInfoAnnotation],
    resets: Seq[RegisterResetAnnotation]
  ): ir.DefModule = m match {
    case mod: ir.Module =>
      val fsms = infos.filter(_.target.module == mod.name)
      if (fsms.isEmpty) { mod }
      else {
        val isFsm = fsms.map(_.target.ref).toSet
        val fsmRegs = findFsmRegs(mod.body, isFsm)
        val stmts = new mutable.ListBuffer[ir.Statement]()
        val ctx = ModuleCtx(c.module(mod.name), stmts, Namespace(mod))
        val toReset = RegisterResetAnnotationPass.findResetsInModule(ctx.m, resets)
        val fsmAnnos = fsms.map { f => onFsm(f, fsmRegs.find(_.name == f.target.ref).get, ctx, toReset.get) }
        annos ++= fsmAnnos
        val newBody = ir.Block(mod.body +: stmts.toList)

        mod.copy(body = newBody)
      }
    case other => other
  }

  private case class ModuleCtx(m: ModuleTarget, stmts: mutable.ListBuffer[ir.Statement], namespace: Namespace)

  private def onFsm(fsm: FsmInfoAnnotation, reg: ir.DefRegister, ctx: ModuleCtx, toReset: String => Option[String])
    : Annotation = {
    val info = reg.info
    val clock = reg.clock
    val reset = toReset(reg.name).map(ir.Reference(_, Utils.BoolType, NodeKind, SourceFlow)).getOrElse(Utils.False())
    val notReset = Utils.not(reset)
    val regRef = ir.Reference(reg)
    val regWidth = firrtl2.bitWidth(reg.tpe)
    def inState(s: BigInt): ir.Expression = Utils.eq(regRef, ir.UIntLiteral(s, ir.IntWidth(regWidth)))

    // cover state when FSM is _not_ in reset
    val states = fsm.states.map { case (id, stateName) =>
      val name = ctx.namespace.newName(reg.name + "_" + stateName)
      ctx.stmts.append(ir.Verification(ir.Formal.Cover, info, clock, inState(id), notReset, ir.StringLit(""), name))
      stateName -> ctx.m.ref(name)
    }

    // create a register to hold the previous state
    val prevState =
      Builder.makeRegister(ctx.stmts, info, ctx.namespace.newName(reg.name + "_prev"), reg.tpe, clock, regRef)
    def inPrevState(s: BigInt): ir.Expression = Utils.eq(prevState, ir.UIntLiteral(s, ir.IntWidth(regWidth)))

    // create a register to track if the previous state is valid
    val prevValid = Builder.makeRegister(
      ctx.stmts,
      info,
      ctx.namespace.newName(reg.name + "_prev_valid"),
      Utils.BoolType,
      clock,
      notReset
    )

    // create a transition valid signal
    val transitionValid = ir.Reference(ctx.namespace.newName(reg.name + "_t_valid"), Utils.BoolType, NodeKind)
    ctx.stmts.append(ir.DefNode(info, transitionValid.name, Utils.and(notReset, prevValid)))

    val idToName = fsm.states.toMap
    val transitions = fsm.transitions.map { case (from, to) =>
      val (fromName, toName) = (idToName(from), idToName(to))
      val name = ctx.namespace.newName(reg.name + "_" + fromName + "_to_" + toName)
      ctx.stmts.append(
        ir.Verification(
          ir.Formal.Cover,
          info,
          clock,
          Utils.and(inPrevState(from), inState(to)),
          transitionValid,
          ir.StringLit(""),
          name
        )
      )
      (fromName, toName) -> ctx.m.ref(name)
    }

    FsmCoverageAnnotation(ctx.m.ref(reg.name), states, transitions)
  }

  private def printFsmInfo(fsm: FsmInfoAnnotation): Unit = {
    val toName = fsm.states.toMap
    println(s"[${fsm.target.module}.${fsm.target.name}] Found FSM")
    if (fsm.start.nonEmpty) {
      println(s"Start: ${toName(fsm.start.get)}")
    }
    println("Transitions:")
    fsm.transitions.foreach(t => println(s"${toName(t._1)} -> ${toName(t._2)}"))
  }

  private def findFsmRegs(s: ir.Statement, isFsm: String => Boolean): Seq[ir.DefRegister] = s match {
    case r: ir.DefRegister if isFsm(r.name) => List(r)
    case ir.Block(stmts) => stmts.flatMap(findFsmRegs(_, isFsm))
    case _: ir.Conditionally => throw new RuntimeException("Unexpected when statement! Expected LoFirrtl.")
    case _ => List()
  }
}
