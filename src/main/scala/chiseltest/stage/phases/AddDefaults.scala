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

    /** Get TargetDirAnnotation and convert it to absolute path to make simulators happier. */
    val targetDirAnnotation = TargetDirAnnotation(
      new java.io.File(annotations.collectFirst {
        /* https://github.com/freechipsproject/firrtl/issues/1929 */
        case TargetDirAnnotation(".") =>
          annotations.collectFirst {
            case TestNameAnnotation(name) => TargetDirAnnotation(s"test_run_dir" + java.io.File.separator + name)
          }.get
        case t: TargetDirAnnotation => t
      }.getOrElse(TargetDirAnnotation(s"test_run_dir" + java.io.File.separator + name)).directory).getAbsolutePath
    )
    new Directory(new File(targetDirAnnotation.directory)).deleteRecursively()

    val backendAnnotation = annotations.collect {
      case t: BackendAnnotation => t
    }

    /** If not specify backend, use treadle by default. */
    val backend = if (backendAnnotation.isEmpty) Seq(TreadleBackendAnnotation) else backendAnnotation

    annotations.filter {
      case _: TargetDirAnnotation => false
      case _: BackendAnnotation   => false
      case _ => true
    } ++ backend :+ targetDirAnnotation
  }
}
