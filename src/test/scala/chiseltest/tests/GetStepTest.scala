// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import chiseltest.internal.NoThreadingAnnotation
import org.scalatest.flatspec.AnyFlatSpec



class GetStepTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Testers2 clock getStep"

  private def runTest(c: StaticModule[_]): Unit = {
    assert(c.clock.getStep == 0)
    c.clock.step()
    assert(c.clock.getStep == 1)
    c.clock.step(10)
    assert(c.clock.getStep == 11)
  }

  it should "check steps in single clock" in {
    test(new StaticModule(0.U))(runTest)
  }

  it should "check steps in single clock with single threaded backend" in {
    test(new StaticModule(0.U)).withAnnotations(Seq(NoThreadingAnnotation))(runTest)
  }

  private def randomClockStepper(rand: scala.util.Random, c: StaticModule[_], maxCycles: Int): Unit = {
    var count = 0
    while(count < maxCycles) {
      val delta = rand.nextInt(3) + 1
      c.clock.step(delta)
      count += delta
      assert(c.clock.getStep == count)
    }
  }

  it should "check steps with parallel threads" in {
    val maxCycles = 1000
    val rand = new scala.util.Random(123)
    test(new StaticModule(0.U)) { c =>
      c.clock.setTimeout(maxCycles * 2)
      // thread 1
      fork { randomClockStepper(rand, c, maxCycles) }
      // thread 2
      fork { randomClockStepper(rand, c, maxCycles) }
      // thread 0
      randomClockStepper(rand, c, maxCycles)
    }
  }
}
