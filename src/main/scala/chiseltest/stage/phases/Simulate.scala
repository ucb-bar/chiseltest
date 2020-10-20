package chiseltest.stage.phases

import chiseltest.internal.ThreadedBackend
import chiseltest.stage.ChiselTestOptions
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase, Viewer}

class Simulate extends Phase {
  override def prerequisites: Seq[Dependency[Phase]] = Seq(Dependency[CompileDut])

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq =
    new ThreadedBackend(annotations).run.filterNot {
      case _: ExportedSingalsAnnotation      => true
      case _: TopCombinationalPathAnnotation => true
      case _ => false
    }
}
