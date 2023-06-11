// SPDX-License-Identifier: Apache-2.0

package treadle2.stage.phases

import firrtl2.options.{Dependency, Phase}
import firrtl2.passes.memlib.VerilogMemDelays
import firrtl2.stage.{FirrtlCircuitAnnotation, Forms}
import firrtl2.transforms.BlackBoxSourceHelper
import firrtl2.{AnnotationSeq, CircuitState}
import treadle2.TreadleCircuitStateAnnotation
import treadle2.utils.AugmentPrintf

/** Call a bunch of transforms so TreadleTester can operate
  */
class PrepareAst extends Phase {
  private val targets = Seq(
    Dependency(firrtl2.transforms.EnsureNamedStatements),
    Dependency[BlackBoxSourceHelper],
    Dependency[AugmentPrintf],
    Dependency[HandleFormalStatements]
  ) ++ Forms.LowFormOptimized ++
    Seq(Dependency(VerilogMemDelays))

  private def compiler = new firrtl2.stage.transforms.Compiler(targets, currentState = Nil)

  private val transforms = compiler.flattenedTransformOrder

  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {

    annotationSeq.flatMap {
      case FirrtlCircuitAnnotation(circuit) =>
        val state = CircuitState(circuit, annotationSeq)
        val newState = transforms.foldLeft(state) { case (prevState, transform) =>
          transform.runTransform(prevState)
        }
        Some(TreadleCircuitStateAnnotation(newState))
      case other =>
        Some(other)
    }
  }
}
