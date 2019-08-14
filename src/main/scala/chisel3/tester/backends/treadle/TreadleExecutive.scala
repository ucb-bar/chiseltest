// See LICENSE for license details.

package chisel3.tester.backends.treadle

import chisel3.experimental.{DataMirror, MultiIOModule}
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage, NoRunFirrtlCompilerAnnotation}
import chisel3.tester.backends.BackendExecutive
import chisel3.tester.internal._
import firrtl.annotations.ReferenceTarget
import firrtl.transforms.{CheckCombLoops, CombinationalPath}
import treadle.stage.TreadleTesterPhase
import treadle.{TreadleCircuitStateAnnotation, TreadleTesterAnnotation}

object TreadleExecutive extends BackendExecutive {
  import firrtl._

  def componentToName(component: ReferenceTarget): String = {
    component.name
  }

  def start[T <: MultiIOModule](dutGen: () => T, testersAnnotationSeq: AnnotationSeq): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    val annotationSeq = (new OptionsAdapter).transform(testersAnnotationSeq)

    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)
    val circuit = generatorAnnotation.elaborate.toSeq.head match {
      case g: ChiselCircuitAnnotation => g.circuit
      case _ =>
        throw new Exception(s"TreadleTesterPhase could not retrieve elaborated circuit")
    }
    val dut = getTopModule(circuit).asInstanceOf[T]
    val portNames = DataMirror.modulePorts(dut).flatMap { case (name, data) =>
      getDataNames(name, data).toList
    }.toMap

    val chiseledAnnotations = (new ChiselStage).run(
      annotationSeq ++ Seq(generatorAnnotation, NoRunFirrtlCompilerAnnotation)
    )

    val treadledAnnotations = TreadleTesterPhase.transform(chiseledAnnotations)

    val treadleTester = treadledAnnotations.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
      throw new Exception(
        s"TreadleTesterPhase could not build a treadle tester from these annotations" +
        chiseledAnnotations.mkString("Annotations:\n", "\n  ", "")
      )
    )

    val circuitState = treadledAnnotations.collectFirst { case TreadleCircuitStateAnnotation(s) => s }.get
    val pathAnnotations = (new CheckCombLoops).execute(circuitState).annotations
    val paths = pathAnnotations.collect {
      case c: CombinationalPath => c
    }

    val pathsAsData = combinationalPathsToData(dut, paths, portNames, componentToName)

    new TreadleBackend(dut, portNames, pathsAsData, treadleTester)
  }
}
