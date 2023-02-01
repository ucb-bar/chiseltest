// SPDX-License-Identifier: Apache-2.0

package treadle.stage.phases

import firrtl.options.{Dependency, Phase}
import firrtl.passes.memlib.VerilogMemDelays
import firrtl.stage.{FirrtlCircuitAnnotation, Forms}
import firrtl.transforms.BlackBoxSourceHelper
import firrtl.{AnnotationSeq, CircuitState}
import treadle.TreadleCircuitStateAnnotation
import treadle.coverage.pass.AddCoverageExpressions
import treadle.utils.AugmentPrintf

/** Call a bunch of transforms so TreadleTester can operate
  */
class PrepareAst extends Phase {
  private val targets = Seq(
    Dependency(firrtl.transforms.EnsureNamedStatements),
    Dependency(AddCoverageExpressions),
    Dependency[BlackBoxSourceHelper],
    Dependency[AugmentPrintf],
    Dependency[HandleFormalStatements]
  ) ++ Forms.LowFormOptimized ++
    Seq(Dependency(VerilogMemDelays))

  private def compiler = new firrtl.stage.transforms.Compiler(targets, currentState = Nil)

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
