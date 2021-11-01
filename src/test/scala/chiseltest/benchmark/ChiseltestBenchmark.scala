// SPDX-License-Identifier: Apache-2.0

package chiseltest.benchmark

import chiseltest._
import chisel3._
import chiseltest.internal.NoThreadingAnnotation
import chiseltest.simulator.SimulatorAnnotation
import chiseltest.simulator.benchmark.DecoupledGcd
import firrtl.options.TargetDirAnnotation
import treadle.chronometry.Timer

object ChiseltestBenchmark extends App {
  private def computeGcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)

  private def runTest(dut: DecoupledGcd, testValues: Iterable[(BigInt, BigInt, BigInt)]): Long = {
    var cycles = 0L
    dut.reset.poke(true.B)
    dut.clock.step(2)
    cycles += 2
    dut.reset.poke(false.B)

    dut.output.ready.poke(true.B)
    for((i, j, expected) <- testValues) {
      dut.input.bits.value1.poke(i.U)
      dut.input.bits.value2.poke(j.U)
      dut.input.valid.poke(true.B)
      dut.clock.step(1)
      cycles += 1

      while(!dut.output.valid.peek().litToBoolean) {
        dut.clock.step(1)
        cycles += 1
      }
      dut.output.bits.gcd.expect(expected.U)
      dut.output.valid.expect(true.B)
    }

    cycles
  }

  // select and load simulator
  val sim: SimulatorAnnotation = args.headOption match {
    case None => VerilatorBackendAnnotation
    case Some(name) => name.toLowerCase match {
      case "verilator" => VerilatorBackendAnnotation
      case "treadle" => TreadleBackendAnnotation
      case "iverilog" => IcarusBackendAnnotation
      case "vcd" => VcsBackendAnnotation
      case other => throw new RuntimeException(s"Unknown simulator option: $other")
    }
  }
  val simName = sim.getSimulator.name

  val targetDir = TargetDirAnnotation("test_run_dir/gcd_benchmark_chiseltest_and_" + simName)
  val annos = Seq(targetDir, sim, NoThreadingAnnotation)

  val repetitions = 6
  val numMax = 200
  val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y), computeGcd(x, y))
  val t = new Timer
  var cycles = 0L

  RawTester.test(new DecoupledGcd(bitWidth = 60), annos) { dut =>
    (0 until repetitions).foreach { _ =>
      t("sim-gcd-chiseltest-" + simName) {
        cycles += runTest(dut, testValues)
      }
    }
  }

  println(t.report())
  println(s"$cycles cycles")
}
