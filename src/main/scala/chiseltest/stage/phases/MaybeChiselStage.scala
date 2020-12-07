// SPDX-License-Identifier: Apache-2.0

package chiseltest.stage.phases

import firrtl.{AnnotationSeq, EmittedVerilogCircuitAnnotation}
import firrtl.options.{Dependency, Phase}

/** Invoke [[chisel3.stage.ChiselStage]] to elaborate design if Verilog does not already exist. */
class MaybeChiselStage extends Phase {
  override def prerequisites = Seq(Dependency[AddDefaults])

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations.collectFirst {
    case _: EmittedVerilogCircuitAnnotation => annotations
  }.getOrElse {
    (new chisel3.stage.ChiselStage).execute(Array("-X", "verilog"), annotations)
  }
}
