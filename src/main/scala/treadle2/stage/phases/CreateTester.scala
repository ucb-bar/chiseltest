// SPDX-License-Identifier: Apache-2.0

package treadle2.stage.phases

import firrtl2.AnnotationSeq
import firrtl2.options.Phase
import firrtl2.stage.FirrtlCircuitAnnotation
import treadle2.{TreadleCircuitStateAnnotation, TreadleTester, TreadleTesterAnnotation}

object CreateTester extends Phase {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    if (
      a.exists {
        case FirrtlCircuitAnnotation(_)       => true
        case TreadleCircuitStateAnnotation(_) => true
        case _                                => false
      }
    ) {
      val tester = new TreadleTester(a)
      a :+ TreadleTesterAnnotation(tester)
    } else {
      a
    }
  }
}
