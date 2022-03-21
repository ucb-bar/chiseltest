// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class SecondClock[T <: Data](ioLit: T) extends Module {
  val out = IO(Output(chiselTypeOf(ioLit)))
  val clock2 = IO(Input(Clock()))
  out := ioLit

}

class GetStepTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "check steps in single clock" in {
   test(new StaticModule(0.U)) { c =>
     c.clock.getStep should be (0)
     c.clock.step()
     c.clock.getStep should be (1)
     c.clock.step(10)
     c.clock.getStep should be (11)
   }
  }

  // This would work but it's currently not supported with the error:
  // java.lang.IllegalArgumentException: requirement failed: Currently only single clock circuits are supported!
  ignore should "check steps on multiple clocks" in {
   test(new SecondClock(0.U)) { c =>
     c.clock.getStep should be (0)
     c.clock2.getStep should be (0)
     c.clock.step()
     c.clock.getStep should be (1)
     c.clock2.step(2)
     c.clock2.getStep should be (2)
     c.clock.step(10)
     c.clock.getStep should be (11)
     c.clock2.step(10)
     c.clock2.getStep should be (12)
   }
  }
}