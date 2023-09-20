// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

import firrtl2.options.Dependency
import firrtl2.stage.{FirrtlSourceAnnotation, Forms}
import scopt.OptionParser
import treadle2.TreadleTester

case class Config(benches: Seq[String], sim: String, warmupRun: Boolean)

class ArgumentParser extends OptionParser[Config]("synthesizer") {
  head("fsim benchmark", "0.1")
  opt[String]("bench").required().action((a, config) => config.copy(benches = config.benches :+ a))
  opt[String]("sim").action((a, config) => config.copy(sim = a))
}

case class Bench(
  name:           String,
  getCircuit:     String => String,
  runTest:        Simulation => Unit,
  runTreadleTest: TreadleTester => Unit)
case class Result(nsCompile: Long, nsRun: Long, steps: Int)
object Benchmark {
  val Benches = Seq(
    Bench("gcd_16", _ => GCDBench.circuitSrc(16), GCDBench.fsimTest(_, 10, 500), GCDBench.treadleTest(_, 10, 500)),
    Bench("gcd_44", _ => GCDBench.circuitSrc(44), GCDBench.fsimTest(_, 10, 500), GCDBench.treadleTest(_, 10, 500)),
    Bench("gcd_64", _ => GCDBench.circuitSrc(64), GCDBench.fsimTest(_, 10, 500), GCDBench.treadleTest(_, 10, 500))
  )

  private val DefaultConfig = Config(benches = Seq(), sim = "fsim", warmupRun = true)

  def main(args: Array[String]): Unit = {
    val parser = new ArgumentParser()
    val conf = parser.parse(args, DefaultConfig).get
    conf.benches.foreach { benchName =>
      val bench =
        Benches.find(_.name == benchName).getOrElse(throw new RuntimeException(s"Unknown benchmark: $benchName"))
      val res = runBench(conf, bench)
      printResult(bench.name, conf.sim, res)
    }

  }

  def printResult(bench: String, sim: String, res: Result): Unit = {
    println(
      s"${bench} on ${sim}: " +
        s"${secondString(res.nsRun)}, ${res.steps} cycles, ${freqString(res.nsRun, res.steps)}, " +
        s"${secondString(res.nsCompile)} to compile"
    )
  }

  private def secondString(ns: Long): String = {
    val elapsedSeconds = ns.toDouble / Giga
    f"$elapsedSeconds%.6fs"
  }

  private val Kilo: Double = 1000.0
  private val Mega: Double = 1000000.0
  private val Giga: Double = 1000000000.0
  private def freqString(ns: Long, steps: Int): String = {
    val elapsedSeconds = ns.toDouble / Giga
    val hz = steps.toDouble / elapsedSeconds
    if (hz > Giga) {
      f"${hz / Giga}%.6fGHz"
    } else if (hz > Mega) {
      f"${hz / Mega}%.6fMHz"
    } else if (hz > Kilo) {
      f"${hz / Kilo}%.6fkHz"
    } else {
      f"${hz}%.6fHz"
    }
  }

  def runBench(conf: Config, bench: Bench): Result = {
    val src = bench.getCircuit(conf.sim)
    conf.sim match {
      case "fsim"    => runFSimBench(conf, bench, src)
      case "treadle" => runTreadleBench(conf, bench, src)
      case other     => throw new RuntimeException(s"Unsupported simulator: $other")
    }
  }

  private def runFSimBench(conf: Config, bench: Bench, src: String): Result = {
    val compileStart = System.nanoTime()
    val sim = new Simulation(Compiler.run(FirrtlCompiler.toLow(src)))
    val compileEnd = System.nanoTime()
    if (conf.warmupRun) {
      bench.runTest(sim)
    }
    val testStart = System.nanoTime()
    bench.runTest(sim)
    val testEnd = System.nanoTime()
    val steps = sim.getStepCount
    // TODO: shut down sim
    Result(nsCompile = compileEnd - compileStart, nsRun = testEnd - testStart, steps = steps)
  }

  private def runTreadleBench(conf: Config, bench: Bench, src: String): Result = {
    val compileStart = System.nanoTime()
    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(src)))
    val compileEnd = System.nanoTime()
    if (conf.warmupRun) {
      bench.runTreadleTest(tester)
    }
    val testStart = System.nanoTime()
    bench.runTreadleTest(tester)
    val testEnd = System.nanoTime()
    val steps = tester.cycleCount.toInt
    tester.finish
    Result(nsCompile = compileEnd - compileStart, nsRun = testEnd - testStart, steps = steps)
  }
}

object FirrtlCompiler {
  private val loFirrtlCompiler =
    new firrtl2.stage.transforms.Compiler(Seq(Dependency[firrtl2.LowFirrtlEmitter]) ++ Forms.LowFormOptimized)
  def toLow(src: String): firrtl2.ir.Circuit = {
    val hi = firrtl2.Parser.parse(src)
    val lo = loFirrtlCompiler.execute(firrtl2.CircuitState(hi))
    lo.circuit
  }
}
