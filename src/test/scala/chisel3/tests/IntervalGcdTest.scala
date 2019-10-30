// See README.md for license details.

package chisel3.tests

// See README.md for license details.

import chisel3._
import chisel3.experimental._
import org.scalatest._
import chisel3.tester._
import chisel3.tester.experimental.TestOptionBuilder._


/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */
class GCD extends Module {
  val io = IO(new Bundle {
    val value1        = Input(Interval(range"[0,1000]"))
    val value2        = Input(Interval(range"[0,1000]"))
    val loadingValues = Input(Bool())
    val outputGCD     = Output(Interval(range"[0,1000]"))
    val outputValid   = Output(Bool())
  })

  val x  = Reg(Interval(range"[0,1000]"))
  val y  = Reg(Interval(range"[0,1000]"))

  when(x > y) { x := (x - y).clip(x) }
    .otherwise { y := (y - x).clip(y) }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD := x
  io.outputValid := y === 0.I
}

class IntervalGcdTest extends FreeSpec with ChiselScalatestTester with Matchers {
  "intervals should work nicely with testers2" in {
    test(new GCD) { c =>
      c.io.value1.poke(22.I)
      c.io.value2.poke(77.I)
      c.io.loadingValues.poke(true.B)
      c.clock.step()
      c.io.loadingValues.poke(false.B)
      while(! c.io.outputValid.peek().litToBoolean) {
        c.clock.step()
      }
      c.io.outputGCD.expect(11.I)
      c.io.outputGCD.peek().litValue should be (BigInt(11))
    }
  }
}

