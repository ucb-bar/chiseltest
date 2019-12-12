// See LICENSE for license details.

package chiseltest.backends.treadle

import chiseltest.backends.BackendExecutive
import chiseltest.internal._
import chisel3.experimental.DataMirror
import chisel3.MultiIOModule
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import firrtl.annotations.ReferenceTarget
import firrtl.stage.CompilerAnnotation
import firrtl.transforms.{CheckCombLoops, CombinationalPath}
import treadle.stage.TreadleTesterPhase
import treadle.{TreadleCircuitStateAnnotation, TreadleFirrtlFormHint, TreadleTesterAnnotation}

object TreadleExecutive extends BackendExecutive {
  import firrtl._

  def componentToName(component: ReferenceTarget): String = {
    component.name
  }

  def start[T <: MultiIOModule](dutGen: () => T, testersAnnotationSeq: AnnotationSeq): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)

    // This provides an opportunity to translate from top level generic flags to backend specific annos
    var annotationSeq = (new OptionsAdapter).transform(testersAnnotationSeq)

    // This produces a chisel circuit annotation, a later pass will generate a firrtl circuit
    // Can't do both at once currently because generating the latter deletes the former
    annotationSeq = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ generatorAnnotation)

    val circuit = annotationSeq.collect { case x: ChiselCircuitAnnotation => x }.head.circuit
    val dut = getTopModule(circuit).asInstanceOf[T]
    val portNames = DataMirror.modulePorts(dut).flatMap { case (name, data) =>
      getDataNames(name, data).toList
    }.toMap

    // This generates the firrtl circuit needed by the TreadleTesterPhase
    annotationSeq = (new ChiselStage).run(annotationSeq ++ Seq(CompilerAnnotation(new LowFirrtlCompiler)))

    // This generates a TreadleTesterAnnotation with a treadle tester instance
    annotationSeq = TreadleTesterPhase.transform(annotationSeq :+ TreadleFirrtlFormHint(LowForm))

    val treadleTester = annotationSeq.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(
        s"TreadleTesterPhase could not build a treadle tester from these annotations" +
        annotationSeq.mkString("Annotations:\n", "\n  ", "")
      )
    )

    val circuitState = annotationSeq.collectFirst { case TreadleCircuitStateAnnotation(s) => s }.get
    val pathAnnotations = (new CheckCombLoops).execute(circuitState).annotations
    val paths = pathAnnotations.collect { case c: CombinationalPath => c }

    val pathsAsData = combinationalPathsToData(dut, paths, portNames, componentToName)

    new TreadleBackend(dut, portNames, pathsAsData, treadleTester)
  }
}
