// SPDX-License-Identifier: Apache-2.0

package chiseltest.stage.phases

import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase, TargetDirAnnotation}

import java.io.File
import scala.reflect.io.Directory

/** add default [[TargetDirAnnotation]] and [[BackendAnnotation]]. */
class AddDefaults extends Phase {
  override def prerequisites: Seq[Dependency[Phase]] = Seq(Dependency[Checks])

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq.empty

  override def invalidates(a: Phase) = false

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {

    /** Add [[TargetDirAnnotation]] by [[TestNameAnnotation]] */
    val targetDirAnnotation = TargetDirAnnotation(new java.io.File(annotations.collectFirst {
      case TestNameAnnotation(name) =>
        s"test_run_dir" + java.io.File.separator + name + java.io.File.separator + System.currentTimeMillis
    }.get).getAbsolutePath)

    /** If not specify backend, use treadle by default. */
    val backendAnnotation = annotations.collectFirst {
      case t: BackendAnnotation => t
    }.getOrElse(TreadleBackendAnnotation)

    annotations.filter {
      case _: TargetDirAnnotation => false
      case _: BackendAnnotation   => false
      case _ => true
    } :+ backendAnnotation :+ targetDirAnnotation
  }
}
