// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import java.io.File
import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.iotesters._
import chiseltest.simulator.RequiresVerilator
import firrtl.options.TargetDirAnnotation
import treadle2.chronometry.Timer
import org.scalatest.flatspec.AnyFlatSpec

object RealGCD2 {
  val num_width = 16
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

class GCDPeekPokeTester(c: RealGCD2, maxX: Int = 10, maxY: Int = 10, showTiming: Boolean = false)
  extends PeekPokeTester(c)  {
  val timer = new Timer

  timer("overall") {
    for {
      i <- 1 to maxX
      j <- 1 to maxY
    } {
      val (gcd_value, _) = GCDCalculator.computeGcdResultsAndCycles(i, j)

      timer("operation") {
        poke(c.io.RealGCD2in.bits.a, i)
        poke(c.io.RealGCD2in.bits.b, j)
        poke(c.io.RealGCD2in.valid, 1)

        var count = 0
        while (peek(c.io.RealGCD2out.valid) == BigInt(0) && count < 20000) {
          step(1)
          count += 1
        }
        if (count > 30000) {
          // println(s"Waited $count cycles on gcd inputs $i, $j, giving up")
          System.exit(0)
        }
        expect(c.io.RealGCD2out.bits, gcd_value)
        step(1)
      }
    }
  }
  if(showTiming) {
    println(s"\n${timer.report()}")
  }
}

class GCDSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "GCDSpec"

  it should "compute gcd excellently" in {
    test(new RealGCD2).runPeekPoke(new GCDPeekPokeTester(_))
  }

  it should "run with verilator" taggedAs RequiresVerilator in {
    test(new RealGCD2).withAnnotations(Seq(VerilatorBackendAnnotation)).runPeekPoke(new GCDPeekPokeTester(_))
  }

  // this is a change from the old iotesters behavior which would always create a VCD when using Verilator
  it should "require an explicit annotation to create a vcd" taggedAs RequiresVerilator in {
    val target = TargetDirAnnotation("test_run_dir/gcd_make_vcd")
    test(new RealGCD2).withAnnotations(Seq(target, VerilatorBackendAnnotation, WriteVcdAnnotation))
      .runPeekPoke(new GCDPeekPokeTester(_))

    val vcd = os.pwd / os.RelPath(target.directory) / "RealGCD2.vcd"
    assert(os.exists(vcd))
  }

  it should "run verilator with larger input vector to run regressions" taggedAs RequiresVerilator in {
    test(new RealGCD2).withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke(new GCDPeekPokeTester(_, 100, 1000, showTiming = true))
  }


}

