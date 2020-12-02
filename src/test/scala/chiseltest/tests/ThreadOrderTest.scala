// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ThreadOrderTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ChiselTest thread ordering"

  it should "run child threads immediately, then on future cycles before parent threads" in {
    // TODO: can this be done without using shared variables?
    test(new StaticModule(0.U)) { c =>
      var flag: Int = 0
      fork {
        while (true) {
          flag += 1
          c.clock.step(1)
        }
      }
      // Fork runs immediately, increments then blocks
      assert(flag == 1)
      c.clock.step(1)
      // Fork should run before this thread is scheduled
      assert(flag == 2)
    }
  }
}
