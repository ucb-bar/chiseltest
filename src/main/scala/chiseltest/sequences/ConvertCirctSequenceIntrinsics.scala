package chiseltest.sequences

import firrtl2._
import firrtl2.annotations.{Annotation, CircuitTarget, PresetRegAnnotation}
import firrtl2.options.Dependency
import firrtl2.passes.ExpandWhensAndCheck
import firrtl2.stage.Forms
import firrtl2.transforms.DedupModules

import scala.collection.mutable

/** Replaces circt sequence intrinsics with a synthesizable version. */
object ConvertCirctSequenceIntrinsics extends Transform {
  override def invalidates(a: Transform) = false
  override def prerequisites = Forms.Resolved
  // this should run before we remove whens and deduplication
  override def optionalPrerequisiteOf = Seq(
    Dependency[ExpandWhensAndCheck],
    Dependency[DedupModules]
  )

  private val HasBeenReset = "circt_has_been_reset"
  private val LtlDisable = "circt_ltl_disable"
  private val LtlClock = "circt_ltl_clock"
  // Assume, Assert, Cover
  private val VerifOps = Set("circt_verif_assert", "circt_verif_assume", "circt_verif_cover")

  private val Intrinsics = Set(
    HasBeenReset,
    LtlDisable,
    LtlClock
  ) | VerifOps

  private def findIntrinsicMapping(circuit: ir.Circuit): Map[String, String] =
    circuit.modules.collect { case e: ir.ExtModule if Intrinsics.contains(e.defname) => e.name -> e.defname }.toMap

  override protected def execute(state: CircuitState): CircuitState = {
    // scan ext modules to see if there are any CIRCT intrinsics handled by this pass
    val intrinsics = findIntrinsicMapping(state.circuit)
    if (intrinsics.isEmpty) { // early exit / nothing to do
      return state
    }

    // remove intrinsics ext modules
    val withoutIntrinsicsModules = state.circuit.modules.filter {
      case e: ir.ExtModule if Intrinsics.contains(e.defname) => false
      case _ => true
    }

    // replace intrinsics in modules
    val c = CircuitTarget(state.circuit.main)
    val newAnnos = mutable.ListBuffer[Annotation]()
    val modules = withoutIntrinsicsModules.map {
      case m: ir.Module =>
        val (mod, annos) = onModule(c, m, intrinsics)
        newAnnos ++= annos
        mod
      case other => other
    }

    val circuit = state.circuit.copy(modules = modules)

    // TODO
    println(circuit.serialize)
    state.copy(circuit = circuit, annotations = newAnnos.toSeq ++: state.annotations)
  }

  private def onModule(c: CircuitTarget, m: ir.Module, intrinsics: Map[String, String]): (ir.Module, AnnotationSeq) = {
    println(m.serialize)
    val ctx = Ctx(intrinsics, Namespace(m))
    val body = onStmt(ctx)(m.body)
    val newAnnos = ctx.presetRegs.toSeq.map(name => PresetRegAnnotation(c.module(m.name).ref(name)))
    val finalModule = m.copy(body = body)
    (finalModule, newAnnos)
  }

  private case class IntrinsicInstance(name: String, intrinsic: String, info: ir.Info)
  private case class Ctx(
    /** maps the name of a module to the intrinsic it represents */
    moduleToIntrinsic: Map[String, String],
    /** namespace of the current module */
    namespace: Namespace,
    /** maps the name of an instance to the intrinsic that its module represents */
    instToIntrinsic: mutable.HashMap[String, IntrinsicInstance] = mutable.HashMap(),
    /** keeps track of all inputs to a intrinsic instance */
    inputs: mutable.HashMap[String, List[(String, ir.Expression)]] = mutable.HashMap(),
    /** Keeps track of any intrinsic outputs that we have generated. Currently only used for `HasBeenReset` */
    outputs: mutable.HashMap[(String, String), ir.Expression] = mutable.HashMap(),
    /** Maps intrinsic instance name to IR Node */
    nodes: mutable.HashMap[String, Node] = mutable.HashMap(),
    /** Keeps track of registers that need to be preset annotated (to implement HasBeenReset) */
    presetRegs: mutable.ListBuffer[String] = mutable.ListBuffer(),
    /** Avoids duplicate has_been_reset trackers. */
    hasBeenResetCache: mutable.HashMap[(String, String), ir.Expression] = mutable.HashMap()) {
    def isIntrinsicMod(module: String): Boolean = moduleToIntrinsic.contains(module)

    def isIntrinsicInst(module: String): Boolean = instToIntrinsic.contains(module)
  }

  private def onStmt(ctx: Ctx)(s: ir.Statement): ir.Statement = s.mapExpr(onExpr(ctx)) match {
    // connecting an intrinsic input
    case ir.Connect(_, ir.SubField(ir.Reference(instIn, _, _, _), portIn, _, _), expr) if ctx.isIntrinsicInst(instIn) =>
      expr match {
        case ir.SubField(ir.Reference(instOut, _, _, _), portOut, _, _) if ctx.isIntrinsicInst(instOut) =>
          connectIntrinsics(ctx, instIn, portIn, instOut, portOut)
        case other =>
          // remember all inputs
          val old = ctx.inputs.getOrElse(instIn, List())
          ctx.inputs(instIn) = (portIn, other) +: old

          // for some intrinsics, connecting inputs triggers an action
          val intrinsicName = ctx.instToIntrinsic(instIn).intrinsic
          if (intrinsicName == HasBeenReset) {
            onHasBeenResetInput(ctx, instIn)
          } else {
            ir.EmptyStmt
          }
      }
    // connect an intrinsic output
    case ir.Connect(_, loc, ir.SubField(ir.Reference(inst, _, _, _), port, _, _)) if ctx.isIntrinsicInst(inst) =>
      throw new NotImplementedError(
        s"Unexpected intrinsic output which is not directly connected to an intrinsic input!: ${s.serialize}"
      )
    case ir.DefInstance(info, name, module, _) if ctx.isIntrinsicMod(module) =>
      ctx.instToIntrinsic(name) = IntrinsicInstance(name, ctx.moduleToIntrinsic(module), info)
      ir.EmptyStmt
    case other => other.mapStmt(onStmt(ctx))
  }

  private def connectIntrinsics(ctx: Ctx, instIn: String, portIn: String, instOut: String, portOut: String)
    : ir.Statement = {
    val intrinsicName = ctx.instToIntrinsic(instIn).intrinsic
    // take action according to the intrinsic
    if (VerifOps.contains(intrinsicName)) {
      onVerificationOp(ctx, instIn)
    } else {
      println(s"TODO: ${instIn}.$portIn <= ${instOut}.$portOut")
      ir.EmptyStmt
    }
  }

  private def onVerificationOp(ctx: Ctx, inst: String): ir.Statement = {
    // there should be exactly one input
    val (inputName, input) = ctx.inputs(inst).head
    assert(inputName == "property")

    ir.EmptyStmt
  }

  private def onHasBeenResetInput(ctx: Ctx, inst: String): ir.Statement = {
    val inputs = ctx.inputs(inst)
    val (clock, reset) = (inputs.find(_._1 == "clock").map(_._2), inputs.find(_._1 == "reset").map(_._2))
    (clock, reset) match {
      case (Some(clockExpr), Some(resetExpr)) =>
        val key = (clockExpr.serialize, resetExpr.serialize)
        val (stmt, output) = ctx.hasBeenResetCache.get(key) match {
          case Some(output) => (ir.EmptyStmt, output)
          case None         =>
            // generate has been reset
            val (stmt, output) = buildHasBeenReset(ctx, inst, clockExpr, resetExpr)
            ctx.hasBeenResetCache(key) = output
            (stmt, output)
        }
        ctx.outputs((inst, "out")) = output
        stmt

      case _ => ir.EmptyStmt
    }
  }

  private def buildHasBeenReset(ctx: Ctx, inst: String, clock: ir.Expression, reset: ir.Expression)
    : (ir.Statement, ir.Expression) = {
    val instanceInfo = ctx.instToIntrinsic(inst)
    val info = instanceInfo.info
    val regName = ctx.namespace.newName("has_been_reset_reg")
    val outName = ctx.namespace.newName("has_been_reset")
    // register starts out as false
    val reg = ir.DefRegister(info, regName, Utils.BoolType, clock, Utils.False(), Utils.False())
    val regRef = ir.Reference(reg).copy(flow = SinkFlow)
    ctx.presetRegs.addOne(regName)
    // when reset becomes true, we set out register to 1
    val update = ir.Conditionally(info, reset, ir.Connect(info, regRef, Utils.True()), ir.EmptyStmt)
    // the output indicates whether the circuits _has been reset_ **and** that reset is not currently active
    val outExpr = Utils.and(regRef.copy(flow = SourceFlow), Utils.not(reset))
    val outNode = ir.DefNode(info, outName, outExpr)
    (ir.Block(Seq(reg, update, outNode)), ir.Reference(outNode).copy(flow = SourceFlow))
  }

  private def onExpr(ctx: Ctx)(e: ir.Expression): ir.Expression = e match {
    case ref @ ir.SubField(ir.Reference(inst, _, _, _), port, _, _) =>
      ctx.outputs.getOrElse((inst, port), ref)
    case other => other.mapExpr(onExpr(ctx))

  }
}
