// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal.examples

import chisel3._
import chiseltest._
import chiseltest.formal._
import chiseltest.experimental._
import chiseltest.iotesters.DecoupledGcd
import org.scalatest.flatspec.AnyFlatSpec

class FormalGcdSpec extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  "GCD" should "pass" taggedAs FormalTag in {
    verify(new DecoupledGcdSpec(new DecoupledGcd(4)), Seq(BoundedCheck(20), DefaultBackend))
  }
}

/** check formal properties of the DecoupledGcd implementation */
class DecoupledGcdSpec(makeDut: => DecoupledGcd) extends Module {
  // create an instance of our DUT and expose its I/O
  val dut = Module(makeDut)
  val input = IO(chiselTypeOf(dut.input))
  input <> dut.input
  val output = IO(chiselTypeOf(dut.output))
  output <> dut.output

  // create a cross module binding to inspect internal state
  val busy = observe(dut.busy)

  // do not accept new inputs while busy
  when(busy) {
    assert(!input.fire)
  }

  // only release outputs when busy
  when(output.fire) {
    assert(output.fire)
  }

  // when there was no transactions, busy should not change
  when(past(!input.fire && !output.fire)) {
    assert(stable(busy))
  }

  // when busy changed from 0 to 1, an input was accepted
  when(rose(busy)) {
    assert(past(input.fire))
  }

  // when busy changed from 1 to 0, an output was transmitted
  when(fell(busy)) {
    assert(past(output.fire))
  }
}
