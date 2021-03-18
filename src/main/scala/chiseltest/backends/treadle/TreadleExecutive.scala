// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.treadle

import chiseltest.backends.BackendExecutive
import chiseltest.internal._
import chisel3.experimental.DataMirror
import chisel3.Module
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import chiseltest.coverage.Coverage
import firrtl.annotations.ReferenceTarget
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.{CheckCombLoops, CombinationalPath}
import treadle.stage.TreadleTesterPhase
import treadle.{TreadleCircuitStateAnnotation, TreadleFirrtlFormHint, TreadleTesterAnnotation}

object TreadleExecutive extends BackendExecutive {
  import firrtl._

  def componentToName(component: ReferenceTarget): String = {
    component.name
  }

  def start[T <: Module](dutGen: () => T, testersAnnotationSeq: AnnotationSeq): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)

    // This provides an opportunity to translate from top level generic flags to backend specific annos
    val transformedOptions = (new OptionsAdapter).transform(testersAnnotationSeq)

    // This produces a chisel circuit annotation, a later pass will generate a firrtl circuit
    // Can't do both at once currently because generating the latter deletes the former
    val elaborated = (new chisel3.stage.phases.Elaborate).transform(transformedOptions :+ generatorAnnotation)

    val circuit = elaborated.collect { case x: ChiselCircuitAnnotation => x }.head.circuit
    val dut = getTopModule(circuit).asInstanceOf[T]
    val portNames = DataMirror
      .modulePorts(dut)
      .flatMap { case (name, data) =>
        getDataNames(name, data).toList
      }
      .toMap

    // This generates the firrtl circuit needed by the TreadleTesterPhase
    val compiled = (new ChiselStage).run(elaborated ++ Seq(RunFirrtlTransformAnnotation(new LowFirrtlEmitter)))

    // This generates a TreadleTesterAnnotation with a treadle tester instance
    val annotationSeq = (new TreadleTesterPhase).transform(compiled)

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

    val coverageAnnotations = Coverage.collectCoverageAnnotations(compiled)

    new TreadleBackend(dut, portNames, pathsAsData, treadleTester, coverageAnnotations)
  }
}
