// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest.iotesters._
import chisel3.util.Counter
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation}
import chiseltest.simulator.RequiresVerilator
import org.scalatest.freespec.AnyFreeSpec


class SecondClockDrivesRegisterSpec extends AnyFreeSpec with ChiselScalatestTester {
  class SecondClock extends Module {
    val inClock = IO(Input(Bool()))
    val out = IO(Output(UInt(8.W)))

    withClock(inClock.asClock) {
      out := Counter(true.B, 8)._1
    }
  }

  class SecondClockTester(c: SecondClock) extends PeekPokeTester(c) {
    poke(c.inClock, 0)
    expect(c.out, 0)

    // Main clock should do nothing
    step(1)
    expect(c.out, 0)
    step(1)
    expect(c.out, 0)

    // Output should advance on rising edge, even without main clock edge
    poke(c.inClock, 1)
    expect(c.out, 1)

    step(1)
    expect(c.out, 1)

    // Repeated, 1should do nothing
    poke(c.inClock, 1)
    expect(c.out, 1)

    // and again
    poke(c.inClock, 0)
    expect(c.out, 1)
    poke(c.inClock, 1)
    expect(c.out, 2)
  }

  "poking a clock should flip register" - {

    "should work with Treadle" in {
      test(new SecondClock).runPeekPoke(new SecondClockTester(_))
    }

    "should work with Verilator" taggedAs RequiresVerilator in {
      test(new SecondClock).withAnnotations(Seq(VerilatorBackendAnnotation)).runPeekPoke(new SecondClockTester(_))
    }
  }
}
