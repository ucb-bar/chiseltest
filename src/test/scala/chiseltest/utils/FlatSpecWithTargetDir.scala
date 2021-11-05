// SPDX-License-Identifier: Apache-2.0

package chiseltest.utils

import chiseltest.experimental.sanitizeFileName
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import org.scalatest.{Outcome, Tag}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.DynamicVariable


/** Extends this to get a FlatSpec with a convenient way to get a unique target directory for your test */
abstract class FlatSpecWithTargetDir extends AnyFlatSpec {


  // Provide test fixture data as part of 'global' context during test runs
  private val scalaTestContext = new DynamicVariable[Option[NoArgTest]](None)
  protected def configMap: org.scalatest.ConfigMap = scalaTestContext.value.get.configMap

  override def withFixture(test: NoArgTest): Outcome = {
    require(scalaTestContext.value.isEmpty)
    scalaTestContext.withValue(Some(test)) {
      super.withFixture(test)
    }
  }

  protected def targetDirAnno = TargetDirAnnotation("test_run_dir/" + getTestName)
  protected def targetDir = os.pwd / os.RelPath(targetDirAnno.directory)
  protected def withTargetDir(annos: AnnotationSeq): AnnotationSeq = {
    if (annos.exists(_.isInstanceOf[TargetDirAnnotation])) { annos
    } else { targetDirAnno +: annos }
  }


  def getTestName: String = {
    sanitizeFileName(scalaTestContext.value.get.name)
  }
}
