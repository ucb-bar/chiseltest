// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3.MultiIOModule
import firrtl.AnnotationSeq
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation

import scala.util.DynamicVariable

object Context {
  class Instance(val backend: BackendInterface, val env: TestEnvInterface) {}

  private var context = new DynamicVariable[Option[Instance]](None)

  def run[T <: MultiIOModule](backend: BackendInstance[T], env: TestEnvInterface, testFn: T => Unit) {
    require(context.value.isEmpty)
    context.withValue(Some(new Instance(backend, env))) {
      backend.run(testFn)
    }
  }

  def apply(): Instance = context.value.get
}
