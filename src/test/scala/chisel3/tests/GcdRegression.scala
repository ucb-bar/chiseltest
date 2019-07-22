package chisel3.tests

import chisel3._
import chisel3.tester._
import chisel3.tests.{PassthroughModule, StaticModule}
import chisel3.tester.experimental.TestOptionBuilder._
import chisel3.tester.internal.VerilatorBackendAnnotation
import chisel3.util.{Decoupled, Valid}
import org.scalatest._

class GcdRegression extends FreeSpec with ChiselScalatestTester with Matchers {
  "run gcd many times" in {
    test(new RealGCD2) { c =>
      for {
        i <- 1 to 10
        j <- 1 to 10
      } {
        val (gcd_value, _) = GCDCalculator.computeGcdResultsAndCycles(i, j)

        c.io.RealGCD2in.bits.a.poke(i.U)
        c.io.RealGCD2in.bits.b.poke(j.U)
        c.io.RealGCD2in.valid.poke(true.B)

        var count = 0
        while(c.io.RealGCD2out.valid.peek() == BigInt(0) && count < 20) {
          c.clock.step(1)
          count += 1
        }
        if(count > 30) {
          // println(s"Waited $count cycles on gcd inputs $i, $j, giving up")
          System.exit(0)
        }
        c.io.RealGCD2out.bits.expect(gcd_value.U)
        c.clock.step(1)
      }
    }
  }
}

object RealGCD2 {
  val num_width = 16
}

object GCDCalculator {
  def computeGcdResultsAndCycles(a: Int, b: Int, depth: Int = 1): (Int, Int) = {
    if(b == 0) {
      (a, depth)
    }
    else {
      computeGcdResultsAndCycles(b, a%b, depth+1 )
    }
  }
}

class RealGCD2Input extends Bundle {
  private val theWidth = RealGCD2.num_width
  val a = UInt(theWidth.W)
  val b = UInt(theWidth.W)
}

class RealGCD2 extends Module {
  private val theWidth = RealGCD2.num_width
  val io  = IO(new Bundle {
    // we use quirky names here to test fixed bug in verilator backend
    val RealGCD2in  = Flipped(Decoupled(new RealGCD2Input()))
    val RealGCD2out = Valid(UInt(theWidth.W))
  })

  val x = Reg(UInt(theWidth.W))
  val y = Reg(UInt(theWidth.W))
  val p = RegInit(false.B)

  val ti = RegInit(0.U(theWidth.W))
  ti := ti + 1.U

  io.RealGCD2in.ready := !p

  when (io.RealGCD2in.valid && !p) {
    x := io.RealGCD2in.bits.a
    y := io.RealGCD2in.bits.b
    p := true.B
  }

  when (p) {
    when (x > y)  { x := y; y := x }
      .otherwise    { y := y - x }
  }

  io.RealGCD2out.bits  := x
  io.RealGCD2out.valid := y === 0.U && p
  when (io.RealGCD2out.valid) {
    p := false.B
  }
}

