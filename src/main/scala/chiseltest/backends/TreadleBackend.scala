// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import firrtl.AnnotationSeq
import treadle.stage.phases.PrepareAst
import treadle.{TreadleCircuitStateAnnotation, TreadleTester}

/** [[SimulatorBackend]] for Treadle.
  * Interface should be [[TreadleInterface]].
  */
object TreadleBackend extends SimulatorBackend {
  def compileDut(annotations: AnnotationSeq): AnnotationSeq = {
    val treadleAnnotations = (new PrepareAst).transform(annotations)
    (treadleAnnotations :+ SimulatorInterfaceAnnotation(
      new TreadleInterface(new TreadleTester(treadleAnnotations))
    )).filterNot {
      case _: TreadleCircuitStateAnnotation => true
      case _ => false
    }
  }
}
