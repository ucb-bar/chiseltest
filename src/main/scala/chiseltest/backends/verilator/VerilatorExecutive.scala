package chiseltest.backends.verilator

import chisel3.MultiIOModule
import chisel3.stage.ChiselCircuitAnnotation
import chiseltest.backends.BackendExecutive
import chiseltest.internal.BackendInstance
import firrtl._
import firrtl.annotations.ReferenceTarget
import firrtl.options.{Dependency, PhaseManager}
import firrtl.stage.CompilerAnnotation

object VerilatorExecutive extends BackendExecutive {
  def start[T <: MultiIOModule](dutGen: () => T, annotationSeq: AnnotationSeq): BackendInstance[T] = {
    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()
    // chisel stage.
    val chiselAnnotations = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ chisel3.stage.ChiselGeneratorAnnotation(dutGen))
    // run firrtl stage.
    val firrtlAnnotations = new PhaseManager(
      Seq(Dependency[chisel3.stage.phases.MaybeAspectPhase], Dependency[chisel3.stage.phases.Convert], Dependency[firrtl.stage.FirrtlPhase]),
      Seq(Dependency[chisel3.stage.phases.Elaborate])
    ).transformOrder.foldLeft(chiselAnnotations :+ CompilerAnnotation(new VerilogCompiler()))((a, f) => f.transform(a))
    // run tester2 stage
    val tester2Annotations = (new VerilatorCompiler).transform(firrtlAnnotations)
    val dut = getTopModule(chiselAnnotations.collect { case x: ChiselCircuitAnnotation => x }.head.circuit).asInstanceOf[T]
    new VerilatorBackend(dut, tester2Annotations)
  }
}
