// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class AssertFail extends Module {
  val io = IO(new Bundle {
    val input = Input(UInt(2.W))
  })

  assert(io.input === 3.U) // This should not be triggered
}

/*
Prior to this fix this test would fail because
the GenericBackend would reset the inputs to their initial values
and then call a final step and the assert would fire.
Generic backend no longer calls the final step
 */
class AssertTest extends AnyFreeSpec with ChiselScalatestTester {
  "Internally test resets input but it should not call a final step, as that incorrectly triggers the assert" in {
    test(new AssertFail).withAnnotations(Seq()) { dut =>
      dut.io.input.poke(3.U)
      dut.clock.step()
    }
  }
}
