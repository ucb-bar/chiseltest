// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import chisel3.util.log2Ceil
import firrtl._
import firrtl.annotations._
import firrtl.options.Dependency
import firrtl.transforms._

/** adds an assumption to the toplevel module that all resets are active in the first cycle */
private object AddResetAssumptionPass extends Transform with DependencyAPIMigration {
  // run on lowered firrtl
  override def prerequisites = Seq(
    Dependency(firrtl.passes.ExpandWhens),
    Dependency(firrtl.passes.LowerTypes),
    Dependency(firrtl.transforms.RemoveReset),
    // try to work around dead code elimination removing our registers
    Dependency[firrtl.transforms.DeadCodeElimination]
  )
  override def invalidates(a: Transform) = false
  // since we generate PresetRegAnnotations, we need to run after preset propagation
  override def optionalPrerequisites = Seq(Dependency[PropagatePresetAnnotations])
  // we want to run before the actual Verilog is emitted
  override def optionalPrerequisiteOf = firrtl.stage.Forms.BackendEmitters

  import FirrtlUtils._
  override def execute(state: CircuitState): CircuitState = {
    val resetLength = getResetLength(state.annotations)
    if (resetLength == 0 || resetIsPreset(state.circuit.main, state.annotations)) return state

    val main = state.circuit.modules.collectFirst { case m: ir.Module if m.name == state.circuit.main => m }.get
    val (clock, reset) = findClockAndReset(main)
    val namespace = Namespace(main)

    // add a port for the preset init
    val preset = ir.Port(ir.NoInfo, namespace.newName("_preset"), ir.Input, ir.AsyncResetType)
    val presetAnno = PresetAnnotation(CircuitTarget(main.name).module(main.name).ref(preset.name))

    // make a saturating reset counter
    val (_, resetPhaseRef, counterStmts) = makeSaturatingCounter(
      ir.NoInfo,
      namespace.newName("_resetCount"),
      namespace.newName("_resetPhase"),
      resetLength,
      clock,
      ir.Reference(preset).copy(flow = SourceFlow)
    )

    // add assumption that reset is active
    val resetActive = ir.Verification(
      ir.Formal.Assume,
      ir.NoInfo,
      clock,
      pred = reset,
      en = resetPhaseRef,
      msg = ir.StringLit(""),
      name = namespace.newName("_resetActive")
    )

    // collect all our statements and add them to the main module
    val stmts = counterStmts :+ resetActive
    val instrumented = main.copy(ports = main.ports :+ preset, body = ir.Block(main.body +: stmts))

    // substitute instrumented main and add annotations
    val otherMods = state.circuit.modules.filterNot(_.name == state.circuit.main)
    state.copy(
      circuit = state.circuit.copy(modules = instrumented +: otherMods),
      annotations = presetAnno +: state.annotations
    )
  }

  def getResetLength(annos: AnnotationSeq): Int = {
    annos.collect { case ResetOption(n) => n }.distinct.toList match {
      case List()    => 1 // default
      case List(one) => one
      case more =>
        throw new RuntimeException(s"Received multiple disagreeing reset options! " + more.mkString(", "))
    }
  }

  private def resetIsPreset(main: String, annos: AnnotationSeq): Boolean = {
    annos.collectFirst {
      case PresetAnnotation(target) if target.circuit == main && target.module == main && target.ref == "reset" => true
    }.getOrElse(false)
  }
}
