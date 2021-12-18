// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental

import chisel3._
import chisel3.util.experimental._
import chisel3.experimental.IO

object observe {
  def apply[T <: Data](signal: T): T = {
    val wire = WireInit(0.U.asTypeOf(chiselTypeOf(signal)))
    BoringUtils.bore(signal, Seq(wire))
    wire
  }
}

object expose {

  /** The method `expose` allows one to bring to a wrapped module, signals from instantiated sub-modules so
    * they can be tested by peek/poke chiseltest. This avoid cluttering the original module with signals that
    * would be used only by the test harness.
    *
    * Usage:
    *
    * {{{
    * import chisel3._
    *
    * // This is the module and submodule to be tested
    * class SubModule extends Module {
    *   val reg  = Reg(UInt(6.W))
    *   reg := 42.U
    * }
    *
    * // Top module which instantiates the submodule
    * class Top extends Module {
    *   val submodule = Module(new SubModule)
    * }
    *
    * // Generate the main project Verilog
    * object Proj extends App {
    *  (new chisel3.stage.ChiselStage).emitVerilog(
    *    new Top()
    *  )}
    * }}}
    *
    * Then in your spec, test the `Top` module with `chiseltest` and `scalatest` while exposing the signal from
    * the submodule to the testbench:
    * {{{
    * import chisel3._
    * import chiseltest._
    * import org.scalatest._
    *
    * import flatspec._
    * import matchers.should._
    * import chiseltest.experimental._
    *
    * // Here we create a wrapper extending the Top module adding the exposed signals
    * class TopWrapper extends Top {
    *   // Expose the submodule "reg" Register
    *   val exposed_reg  = expose(submodule.reg)
    * }
    *
    * it should "expose a submodule Reg by using BoringUtils" in {
    *   test(new TopWrapper) { c =>
    *     c.exposed_reg.expect(42.U)
    *   }
    * }
    * }}}
    *
    * @param signal
    *   the signal to be exposed
    * @return
    *   a signal with the same format to be tested on Top module's spec.
    */
  def apply[T <: Data](signal: T): T = {
    val ob = IO(Output(chiselTypeOf(signal)))
    ob := 0.U.asTypeOf(chiselTypeOf(signal))
    BoringUtils.bore(signal, Seq(ob))
    ob
  }
}
