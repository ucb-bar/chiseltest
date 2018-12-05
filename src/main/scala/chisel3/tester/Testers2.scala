// See LICENSE for license details.

package chisel3.tester

import scala.util.DynamicVariable
import chisel3.experimental.MultiIOModule
import firrtl.ExecutionOptionsManager

// Internal common set of options to be understood by all backends
private[tester] case class TesterOptions(
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

  // TODO: better integration points for default tester selection
  // TODO: add TesterOptions (from chisel-testers) and use that to control default tester selection.
  def createDefaultTester[T <: MultiIOModule](dutGen: () => T, testOptions: TesterOptions,
      execOptions: Option[ExecutionOptionsManager]
      ): BackendInstance[T] = {
    TreadleExecutive.start(dutGen, testOptions, execOptions)
  }

  def apply(): Instance = context.value.get
}
