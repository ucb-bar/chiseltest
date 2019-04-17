// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.experimental.MultiIOModule
import firrtl.ExecutionOptionsManager

/** Your warranty is now void.
  *
  * experimental contains cutting edge features that are, well, experimental, and carry no
  * expectation of long-term support. We may break experimental APIs at any time. These may not
  * work as expected, or may have unforeseen side effects, or may be powerful yet dangerous.
  *
  * You have been warned.
  */
package object experimental {
  type TesterOptions = chisel3.tester.internal.TesterOptions
  val TesterOptions = chisel3.tester.internal.TesterOptions  // expose this internal object, whose "API" is unstable

  implicit class ChiselScalatestOptionBuilder[T <: MultiIOModule](x: ChiselScalatestTester#TestBuilder[T]) {
    def withExecOptions(opt: firrtl.ExecutionOptionsManager): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, Some(opt), x.testOptions)
    }
    def withTesterOptions(opt: TesterOptions): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, x.execOptions, Some(opt))
    }
  }

//  implicit class ChiselScalatestTesterOptions(x: ChiselScalatestTester) {
//    // Experimental API, individual options must be specified with named args
//    def test[T <: MultiIOModule](dutGen: => T, dummy: Int = 0,
//        execOptions: Option[ExecutionOptionsManager] = None, testOptions: Option[TesterOptions] = None
//        )(testFn: T => Unit) {
//      x.runTest(defaults.createDefaultTester(() => dutGen,
//        testOptions.getOrElse(x.getTestOptions), execOptions))(testFn)
//    }
//  }

  implicit class uncheckedPokeableClock(x: Clock) {

  }
}
