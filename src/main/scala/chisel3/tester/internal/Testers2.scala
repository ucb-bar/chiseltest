// See LICENSE for license details.

package chisel3.tester.internal

import scala.util.DynamicVariable
import chisel3.experimental.MultiIOModule

import scala.util.DynamicVariable

// Internal common set of options to be understood by all backends
case class TesterOptions(
  name: String,
  writeVcd: Boolean
)

object Context {
  class Instance(val backend: BackendInterface, val env: TestEnvInterface) {
  }

  private var context = new DynamicVariable[Option[Instance]](None)

  def run[T <: MultiIOModule](backend: BackendInstance[T], env: TestEnvInterface, testFn: T => Unit) {
    require(context.value == None)
    context.withValue(Some(new Instance(backend, env))) {
      backend.run(testFn)
    }
  }

  def apply(): Instance = context.value.get
}
