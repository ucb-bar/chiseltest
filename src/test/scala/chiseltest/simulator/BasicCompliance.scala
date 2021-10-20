// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import chiseltest.utils.CaptureStdout
import firrtl.options.TargetDirAnnotation
import org.scalatest.Tag

/** tests some basic functionality that should be supported by all simulators */
abstract class BasicCompliance(sim: Simulator, tag: Tag = DefaultTag, skipSimRefTest: Boolean = false) extends ComplianceTest(sim, tag) {
  it should "be available" taggedAs(tag) in {
    assert(sim.isAvailable)
  }

  private val standardCircuit = ComplianceTest.StandardInverter

  it should "be able to load and execute a simple combinatorial inverter generated from Chisel" taggedAs(tag)in {
    val dut = load(standardCircuit)
    dut.poke("io_in", 1)
    assert(dut.peek("io_out") == 0)
    dut.poke("io_in", 0)
    assert(dut.peek("io_out") == 1)
    dut.finish()
  }

  it should "provide a reference to the simulator from the dut context object" taggedAs(tag) in {
    assume(!skipSimRefTest)
    val dut = load(standardCircuit)
    assert(dut.sim == sim)
    dut.finish()
  }

  it should "be able to create two independent simulators for the same circuit" taggedAs(tag) in {
    val dut0 = load(standardCircuit)
    // note: while we need to be able to create two simulators,
    // creating them in the same target dir isn't necessarily supported
    val dut1 = load(standardCircuit, Seq(TargetDirAnnotation(targetDirAnno.directory + "_2")))
    val inputs = Seq(1, 0, 1, 1, 0, 0)
    inputs.foreach { i =>
      dut0.poke("io_in", i)
      val out0 = dut0.peek("io_out")
      dut1.poke("io_in", i)
      val out1 = dut1.peek("io_out")
      assert(out0 == out1)
    }
    dut0.finish()
    dut1.finish()
  }

  private def staticModule(num: Int) =
    s"""circuit Foo :
      |  module Foo :
      |    output num: UInt<30>
      |
      |    num <= UInt($num)
      |""".stripMargin

  it should "be able to simulate a circuit with no inputs" taggedAs(tag) in {
    val dut = load(staticModule(1234))
    assert(dut.peek("num") == 1234)
    dut.finish()
  }

  it should "be able to simulate different circuits at the same time" taggedAs(tag) in {
    // note: while we need to be able to create two simulators,
    // creating them in the same target dir isn't necessarily supported
    val nums = Seq(123, 432, 555)
    val duts = nums.map { n =>
      load(staticModule(n), Seq(TargetDirAnnotation(targetDirAnno.directory + "_" + n.toString)))
    }

    nums.zip(duts).foreach { case (num, dut) =>
      assert(dut.peek("num") == num)
    }

    duts.foreach(_.finish())
  }

  private val standardNoReset =
    """circuit Foo :
      |  module Foo :
      |    input clock : Clock
      |    output io : { flip in : UInt<1>, out : UInt<1>}
      |
      |    node _io_out_T = not(io.in) @[main.scala 12:13]
      |    io.out <= _io_out_T @[main.scala 12:10]
      |""".stripMargin

  it should "be able to load and execute a simple circuit without a reset input" taggedAs(tag) in {
    // while Chisel will always add a reset input to a Module, we want this firrtl simulator interface
    // to work with arbitrary firrtl circuits, even if they do not contain a `reset` input
    val dut = load(standardNoReset)
    dut.poke("io_in", 1)
    assert(dut.peek("io_out") == 0)
    dut.poke("io_in", 0)
    assert(dut.peek("io_out") == 1)
    dut.finish()
  }

  private val standardNoResetNoClock =
    """circuit Foo :
      |  module Foo :
      |    output io : { flip in : UInt<1>, out : UInt<1>}
      |
      |    node _io_out_T = not(io.in) @[main.scala 12:13]
      |    io.out <= _io_out_T @[main.scala 12:10]
      |""".stripMargin

  it should "be able to load and execute a simple circuit without a clock input or reset" taggedAs(tag) in {
    // while Chisel will always add a reset input to a Module, we want this firrtl simulator interface
    // to work with arbitrary firrtl circuits, even if they do not contain a `clock` input
    val dut = load(standardNoResetNoClock)
    dut.poke("io_in", 1)
    assert(dut.peek("io_out") == 0)
    dut.poke("io_in", 0)
    assert(dut.peek("io_out") == 1)
    dut.finish()
  }

  it should "not generate any files in the target dir directly" taggedAs(tag) in {
    // we expect simulators to create most of their files in a subdirectory
    // only the firrtl, (System)Verilog and C++ test bench files should be in the directory
    val allowedExts = Seq("sv", "fir", "cpp", "h", "tab", "key", "dat")

    val dut = load(standardCircuit)
    dut.poke("io_in", 1)
    assert(dut.peek("io_out") == 0)
    dut.poke("io_in", 0)
    assert(dut.peek("io_out") == 1)
    dut.finish()

    val unexpected = os.list(targetDir)
      .filter(os.isFile)
      .filterNot( f => allowedExts.contains(f.last.split('.').last))
      .filterNot( _.last == "Foo")
      // JNI library copies
      .filterNot(_.last.startsWith("TesterSharedLib"))
    assert(unexpected.isEmpty, s"${sim.name} generated unexpected file(s):\n" + unexpected.mkString("\n"))
  }

  it should "not print anything to stdout under normal conditions" taggedAs(tag) in {
    // TODO: icarus and vcs still print out some spam in non-debug mode
    val isIcarusOrVcs = sim == IcarusSimulator || sim == VcsSimulator
    assume(!isIcarusOrVcs)

    val (_, out) = CaptureStdout {
      val dut = load(standardCircuit)
      dut.poke("io_in", 1)
      assert(dut.peek("io_out") == 0)
      dut.poke("io_in", 0)
      assert(dut.peek("io_out") == 1)
      dut.finish()
    }
    assert(out.trim.isEmpty)
  }
}
