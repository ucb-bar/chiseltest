package chiseltest.backends

import firrtl.AnnotationSeq
import treadle.{TreadleCircuitStateAnnotation, TreadleTester}
import treadle.stage.phases.PrepareAst

object TreadleBackend {
  def compileDut(annotations: AnnotationSeq) = {
    val treadleAnnotations = (new PrepareAst).transform(annotations)
    (treadleAnnotations :+ SimulatorInterfaceAnnotation(
      new TreadleInterface(new TreadleTester(treadleAnnotations))
    )).filterNot {
      case _: TreadleCircuitStateAnnotation => true
      case _ => false
    }
  }
}
