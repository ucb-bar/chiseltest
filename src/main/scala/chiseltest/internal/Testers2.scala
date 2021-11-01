// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3.Module
import chiseltest.TestResult
import firrtl.annotations.NoTargetAnnotation

import scala.util.DynamicVariable

/** Hint for the backend to try and re-use a compiled simulation from a prior run.
  * @warn this is an experimental option and might be removed in the next release without warning
  */
case object CachingAnnotation extends NoTargetAnnotation

/** Use this option to have all simulator interactions printed to stdout.
  * @warn this is an experimental option and might be removed in the next release without warning
  */
case object PrintPeekPoke extends NoTargetAnnotation

/** This option may speed up your test, but any use of `fork` or `timescope` will fail.
  * @warn this is an experimental option and might be removed in the next release without warning
  */
case object NoThreadingAnnotation extends NoTargetAnnotation

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
