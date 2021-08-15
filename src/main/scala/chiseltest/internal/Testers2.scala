// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3.Module
import chiseltest.TestResult
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.ShellOption

import scala.util.DynamicVariable

case object CachingAnnotation extends NoTargetAnnotation

case object UseSingleThreadingAnnotation extends NoTargetAnnotation {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "t-single-threads",
      toAnnotationSeq = _ => Seq(UseSingleThreadingAnnotation),
      helpText = "disable fork/join and timescope API for your test."
    )
  )
}

object Context {
  class Instance(val backend: BackendInterface, val env: TestEnvInterface) {}

  private var context = new DynamicVariable[Option[Instance]](None)

  def run[T <: Module](backend: BackendInstance[T], env: TestEnvInterface, testFn: T => Unit): TestResult = {
    require(context.value.isEmpty)
    val annotations = context.withValue(Some(new Instance(backend, env))) {
      backend.run(testFn)
    }
    new TestResult(annotations)
  }

  def apply(): Instance = context.value.get
}
