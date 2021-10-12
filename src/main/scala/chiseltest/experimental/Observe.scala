// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental

import chisel3._
import chisel3.util.experimental._

/** Add this trait to your wrapper module to observe on submodule signals using the `observe` method. This avoids
  * cluttering the original module with signals that would be used only by the test harness.
  */
trait Observer { this: Module =>

  /** The method `observe` allows one to bring to the wrapped module (top), signals from instantiated sub-modules so
    * they can be tested by peek/poke chiseltest.
    *
    * Usage:
    *
    * {{{
    * import chiseltest.experimental.Observer
    *
    * // This is the module to be tested
    * class SubModule extends Module {
    *     val reg  = Reg(UInt(6.W))
    *     reg := 42.U
    *   }
    *
    *   // Top module which instantiates the submodule
    *   class Top extends Module {
    *     val submodule = Module(new SubModule)
    *   }
    *
    *   // Here we create a wrapper extending the Top module adding the observer
    *   class TopWrapper extends Top with Observer {
    *     val observed_reg  = observe(submodule.reg)
    *   }
    *
    * }}}
    *
    * The `Top` module can be tested with `chiseltest` and `scalatest` like:
    * {{{
    *   it should "observe a submodule Reg by using BoringUtils" in {
    *     test(new TopWrapper) { c =>
    *       c.observed_reg.expect(42.U)
    *     }
    *   }
    * }}}
    *
    * @param signal
    *   the signal to be observed
    * @return
    *   a signal with the same format to be tested on Top module's spec.
    */
  protected def observe[T <: Data](signal: T): T = {
    val ob = IO(Output(chiselTypeOf(signal)))
    ob := 0.U.asTypeOf(chiselTypeOf(signal))
    BoringUtils.bore(signal, Seq(ob))
    ob
  }
}
