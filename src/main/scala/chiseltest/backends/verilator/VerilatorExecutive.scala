package chiseltest.backends.verilator

import chisel3.MultiIOModule
import chisel3.stage.{ChiselCircuitAnnotation, DesignAnnotation}
import chiseltest.backends.BackendExecutive
import chiseltest.internal.BackendInstance
import firrtl._
import firrtl.annotations.ReferenceTarget
import firrtl.options.{Dependency, PhaseManager}
import firrtl.stage.CompilerAnnotation

object VerilatorExecutive extends BackendExecutive {
  def start[T <: MultiIOModule](dutGen: () => T, annotationSeq: AnnotationSeq): BackendInstance[T] = {
    // run firrtl stage.
    val firrtlAnnotations = new PhaseManager(
      Seq(
        Dependency[chisel3.stage.phases.MaybeAspectPhase],
        Dependency[chisel3.stage.phases.Convert],
        Dependency[firrtl.stage.FirrtlPhase],
        Dependency[chisel3.stage.phases.Elaborate])
    ).transformOrder.foldLeft(AnnotationSeq(annotationSeq ++ Seq(
      chisel3.stage.ChiselGeneratorAnnotation(dutGen),
      CompilerAnnotation(new VerilogCompiler())
    )))((a, f) => f.transform(a))
    val dut = firrtlAnnotations.collect { case DesignAnnotation(m) => m }.head.asInstanceOf[T]
    // run tester2 stage
    val tester2Annotations = (new VerilatorCompiler).transform(firrtlAnnotations)
    new VerilatorBackend(tester2Annotations)
  }
}
