package chiseltest.stage.phases

import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, OptionsException, Phase, TargetDirAnnotation}

class Checks extends Phase {
  override def prerequisites = Seq.empty

  override def optionalPrerequisites = Seq(Dependency[Checks])

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    if (annotations.collectFirst { case TestFunctionAnnotation(t) => t }.isEmpty)
      throw new OptionsException(s"no test function provided.")

    val backendAnnotation = annotations.collect {
      case t: BackendAnnotation => t
    }

    if (backendAnnotation.size > 1)
      throw new OptionsException(s"Only one backend is allowed.")

    if (backendAnnotation.isEmpty)
      throw new OptionsException(s"Please specify a backend.")

    if (annotations.collectFirst { case TargetDirAnnotation(t) => t }.isEmpty)
      throw new OptionsException(s"ChiselTest must explicitly specify a target dir.")

    annotations
  }
}
