// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.benchmark

import chiseltest.simulator._
import firrtl.options.TargetDirAnnotation
import treadle2.chronometry.Timer

object SimulatorBenchmark extends App {
  private def computeGcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)

  private def runTest(dut: SimulatorContext, testValues: Iterable[(BigInt, BigInt, BigInt)]): Long = {
    var cycles = 0L
    dut.poke("reset", 1)
    dut.step(2)
    cycles += 2
    dut.poke("reset", 0)

    dut.poke("output_ready", 1)
    for((i, j, expected) <- testValues) {
      dut.poke("input_bits_value1", i)
      dut.poke("input_bits_value2", j)
      dut.poke("input_valid", 1)
      dut.step(1)
      cycles += 1

      while(dut.peek("output_valid") == 0) {
        dut.step(1)
        cycles += 1
      }
      assert(dut.peek("output_bits_gcd") == expected)
      assert(dut.peek("output_valid") == 1)
    }

    cycles
  }

  // select and load simulator
  val sim = args.headOption match {
    case None => VerilatorBackendAnnotation.getSimulator
    case Some(name) => name.toLowerCase match {
      case "verilator" => VerilatorBackendAnnotation.getSimulator
      case "treadle2" => TreadleBackendAnnotation.getSimulator
      case "iverilog" => IcarusBackendAnnotation.getSimulator
      case "vcd" => VcsBackendAnnotation.getSimulator
      case other => throw new RuntimeException(s"Unknown simulator option: $other")
    }
  }
  assert(sim.isAvailable)
  println(s"Using ${sim.name}")

  val targetDir = TargetDirAnnotation("test_run_dir/gcd_benchmark_" + sim.name)


  // elaborate the design and compile to low firrtl
  val (highFirrtl, _) = Compiler.elaborate(() => new DecoupledGcd(bitWidth = 60), Seq(targetDir))
  val lowFirrtl = Compiler.toLowFirrtl(highFirrtl)
  println(s"Compiled ${lowFirrtl.circuit.main}")
  val dut = sim.createContext(lowFirrtl)

  val repetitions = 6
  val numMax = 200
  val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y), computeGcd(x, y))
  val t = new Timer
  var cycles = 0L
  (0 until repetitions).foreach { _ =>
    t("sim-gcd-" + sim.name) {
      cycles += runTest(dut, testValues)
    }
  }

  println(t.report())
  println(s"$cycles cycles")
}
