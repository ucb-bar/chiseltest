// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.core.ActualDirection  // TODO needs to be a public API
import chisel3.experimental.{DataMirror, MultiIOModule}
import chisel3.tester.internal._
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

  /** This provides a quick and dirty way to poke clocks. Inter-thread dependency is NOT checked,
    * so it is up to you to understand the thread ordering semantics to use this correctly.
    * Note that thread ordering IS deterministic, so you will NOT get a nondeterministic test.
    */
  implicit class UncheckedPokeableClock(signal: Clock) {
    def high(): Unit = {
      if (DataMirror.directionOf(signal) != ActualDirection.Input) {
        throw new UnpokeableException("Cannot only poke inputs")
      }
      Context().backend.pokeClock(signal, true)
    }

    def low(): Unit = {
      if (DataMirror.directionOf(signal) != ActualDirection.Input) {
        throw new UnpokeableException("Cannot only poke inputs")
      }
      Context().backend.pokeClock(signal, false)
    }
  }
}
