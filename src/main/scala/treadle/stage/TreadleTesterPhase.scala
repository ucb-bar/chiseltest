// SPDX-License-Identifier: Apache-2.0

package treadle.stage

import firrtl.AnnotationSeq
import firrtl.options.Phase
import treadle.stage.phases._

/** When returns the annotation list with a TreadleTester constructed
  * from either a circuit, a file, or a string
  */
class TreadleTesterPhase extends Phase {
  private val phases: Seq[Phase] = Seq(
    GetFirrtlAst,
    SetImplicitOutputInfo,
    new PrepareAst,
    CreateTester
  )

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {
    phases.foldLeft(annotations)((a, f) => f.transform(a))
  }
}
