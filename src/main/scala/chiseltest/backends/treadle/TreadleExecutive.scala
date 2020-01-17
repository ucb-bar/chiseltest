/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

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
