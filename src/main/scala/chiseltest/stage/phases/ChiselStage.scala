package chiseltest.stage.phases

import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase}

/** Invoke [[chisel3.stage.ChiselStage]] to elaborate design if Verilog does not already exist. */
class ChiselStage extends Phase {
  override def prerequisites = Seq(Dependency[AddDefaults])

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq = annotations.collectFirst {
    case chisel3.stage.ChiselGeneratorAnnotation(_) => annotations
  }.getOrElse {
    (new chisel3.stage.ChiselStage).transform(annotations)
  }
}
