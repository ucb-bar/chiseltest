// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends

import firrtl.analyses.InstanceKeyGraph
import firrtl.analyses.InstanceKeyGraph.InstanceKey
import firrtl.annotations._
import firrtl.options.Dependency
import firrtl.passes.InlineAnnotation
import firrtl.stage.Forms
import firrtl._
import firrtl.backends.experimental.smt.random.DefRandom

private case class DoNotInlineAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget) = copy(target = n)
}

/** Allows us to track the names of state elements through the inlining process */
private case class StateAnnotation(target: ReferenceTarget, pathName: String, dataBits: Int, depth: Int)
    extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): StateAnnotation = copy(target = n)
}

private object StateAnnotation {
  def apply(target: ReferenceTarget, tpe: ir.Type, depth: BigInt = -1): StateAnnotation = {
    new StateAnnotation(target, toPathName(target), firrtl.bitWidth(tpe).toInt, depth.toInt)
  }

  private def toPathName(t: Target): String = {
    t.tokens.flatMap {
      case TargetToken.Ref(r)      => Some(r)
      case TargetToken.Instance(i) => Some(i)
      case TargetToken.Field(f)    => Some(f)
      case _: TargetToken.OfModule => None
      case other => throw new RuntimeException(s"Unexpected token $other")
    }.mkString(".")
  }
}

/** Annotates the complete hierarchy to be flattened. */
private object FlattenPass extends Transform with DependencyAPIMigration {
  override def prerequisites = Forms.LowForm
  // this pass relies on modules not being dedupped yet (TODO: review that assumption!)
  override def optionalPrerequisiteOf = Seq(
    Dependency[firrtl.passes.InlineInstances] // this pass generates annotations for the InlineInstances pass!
  )

  override def optionalPrerequisites = Seq(
    // we want to trace the renaming of the registers created my the mem delay pass
    Dependency(firrtl.passes.memlib.VerilogMemDelays)
  )
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: firrtl.CircuitState): firrtl.CircuitState = {
    val doNotInline = state.annotations.collect {
      case DoNotInlineAnnotation(target) if target.circuit == state.circuit.main => target.module
    }
    val iGraph = InstanceKeyGraph(state.circuit)
    val children = iGraph.getChildInstances.toMap

    // we tag every module to be inlined unless it is explicitly marked as doNotInline
    val cRef = CircuitTarget(state.circuit.main)
    val main = cRef.module(state.circuit.main)
    val inlineAnnos = inlines(main, main)(children, doNotInline.toSet, iGraph.moduleMap)

    val annos = state.annotations.filterNot(_.isInstanceOf[DoNotInlineAnnotation]) ++ inlineAnnos
    state.copy(annotations = annos)
  }

  private def inlines(
    relTarget: ModuleTarget,
    absTarget: IsModule
  )(
    implicit children: Map[String, Seq[InstanceKey]],
    doNotInline:       Set[String],
    getModule:         String => ir.DefModule
  ): AnnotationSeq = {
    if (doNotInline.contains(relTarget.module)) { Seq() }
    else {
      val stateAnnos = findStates(absTarget, getModule(relTarget.module))
      val childAnnos = children(relTarget.module).flatMap(c =>
        inlines(relTarget.targetParent.module(c.module), absTarget.instOf(c.name, c.module))
      )
      if (relTarget.circuit == relTarget.module) { // never inline the main module
        stateAnnos ++: childAnnos
      } else {
        InlineAnnotation(toName(relTarget)) +: stateAnnos ++: childAnnos
      }
    }
  }

  private def findStates(m: IsModule, dm: ir.DefModule): Seq[StateAnnotation] = dm match {
    case mod: ir.Module => findStates(m, mod.body)
    case _ => List()
  }

  private def findStates(m: IsModule, s: ir.Statement): Seq[StateAnnotation] = s match {
    case reg:  ir.DefRegister   => List(StateAnnotation(m.ref(reg.name), reg.tpe))
    case rand: DefRandom        => List(StateAnnotation(m.ref(rand.name), rand.tpe))
    case mem:  ir.DefMemory     => List(StateAnnotation(m.ref(mem.name), mem.dataType, mem.depth))
    case b:    ir.Block         => b.stmts.flatMap(findStates(m, _))
    case _:    ir.Conditionally => throw new RuntimeException("Not low form!")
    case _ => List()
  }

  /** the InlineInstances pass uses Name instead of Target */
  private def toName(m: ModuleTarget): ModuleName = ModuleName(m.module, CircuitName(m.circuit))

  def getStateMap(main: String, annos: AnnotationSeq): Map[String, String] = {
    annos.collect {
      case StateAnnotation(target, pathName, _, _) if target.circuit == main && target.module == main =>
        target.ref -> pathName
    }.toMap
  }
  def getMemoryDepths(main: String, annos: AnnotationSeq): Map[String, Int] = {
    annos.collect {
      case StateAnnotation(target, pathName, _, depth)
          if target.circuit == main && target.module == main && depth > 0 =>
        pathName -> depth
    }.toMap
  }
}
