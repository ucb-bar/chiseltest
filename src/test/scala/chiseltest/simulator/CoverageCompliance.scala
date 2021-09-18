// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import org.scalatest.Tag

import scala.util.Random

/** Compliance tests for the `getCoverage` and `resetCoverage` functions of the [[SimulatorContext]] interface.  */
abstract class CoverageCompliance(sim: Simulator, tag: Tag = DefaultTag) extends ComplianceTest(sim, tag) {
  behavior of sim.name

  private val testSrc =
    """circuit Foo :
      |  module Child :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output io : { flip in : UInt<1>, out : UInt<1>}
      |
      |    reg r : UInt<1>, clock with :
      |      reset => (reset, UInt(0))
      |    r <= io.in
      |    cover(clock, r, UInt(1), "") : r_one
      |    cover(clock, not(r), UInt(1), "") : r_zero
      |    io.out <= r
      |
      |  module Foo :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output io : { flip in : UInt<1>, out : UInt<1>}
      |
      |    node _io_out_T = not(io.in) @[main.scala 12:13]
      |    inst c of Child
      |    c.clock <= clock
      |    c.reset <= reset
      |    c.io.in <= _io_out_T @[main.scala 12:10]
      |    io.out <= c.io.out
      |""".stripMargin


  if(!sim.supportsCoverage) {
    it should "throw a NotImplemented error when trying to call getCoverage" taggedAs(tag) in {
      val dut = load(testSrc)
      performDutTest(dut)
      dut.finish()
      assertThrows[NotImplementedError] {
        dut.getCoverage()
      }
    }
  }

  if(sim.supportsCoverage) {
    it should "return coverage info after a test was performed" taggedAs(tag) in {
      val dut = load(testSrc)
      performDutTest(dut)
      dut.finish()

      val cov = dut.getCoverage()
      assert(cov.size == 2)
      assert(cov == List(
        "c.r_one" -> 7, "c.r_zero" -> 5
      ))
    }
  }

  if(!sim.supportsLiveCoverage) {
    it should "throw a NotImplemented error when trying to call getCoverage before finish" taggedAs(tag) in {
      val dut = load(testSrc)
      performDutTest(dut)
      assertThrows[NotImplementedError] {
        dut.getCoverage()
      }
      dut.finish()
    }

    it should "throw a NotImplemented error when trying to call resetCoverage" taggedAs(tag) in {
      val dut = load(testSrc)
      performDutTest(dut)
      dut.finish()
      assertThrows[NotImplementedError] {
        dut.resetCoverage()
      }
    }

    it should "throw a NotImplemented error when trying to call resetCoverage before finish" taggedAs(tag) in {
      val dut = load(testSrc)
      performDutTest(dut)
      assertThrows[NotImplementedError] {
        dut.resetCoverage()
      }
      dut.finish()
    }
  }

  if(sim.supportsLiveCoverage) {
    it should "when live coverage is supported, regular coverage must also be supported" taggedAs(tag) in {
      assert(sim.supportsCoverage)
    }

    it should "return coverage info during a test" taggedAs(tag) in {
      val dut = load(testSrc)
      performDutTest(dut)

      val cov = dut.getCoverage()
      assert(cov.size == 2)
      assert(cov == List(
        "c.r_one" -> 7, "c.r_zero" -> 5
      ))

      // more testing
      performDutTest(dut)

      val cov2 = dut.getCoverage()
      assert(cov2.size == 2)
      assert(cov2 == List(
        "c.r_one" -> 14, "c.r_zero" -> 10
      ))

      dut.finish()
    }

    it should "support resetting the coverage counters to zero" taggedAs(tag) in {
      val dut = load(testSrc)
      performDutTest(dut)

      val cov = dut.getCoverage()
      assert(cov.size == 2)
      assert(cov == List(
        "c.r_one" -> 7, "c.r_zero" -> 5
      ))

      // more testing
      dut.resetCoverage()
      performDutTest(dut)

      val cov2 = dut.getCoverage()
      assert(cov2.size == 2)
      assert(cov2 == List(
        "c.r_one" -> 7, "c.r_zero" -> 5
      ))
    }

  }


  // perform some testing with the dut in order to generate interesting waveforms
  private def performDutTest(dut: SimulatorContext): Unit = {
    dut.poke("reset", 1)
    dut.step()
    dut.poke("reset", 0)
    assert(dut.peek("io_out") == 0)
    val rand = new scala.util.Random(0)
    (0 until 10).foreach { _ =>
      val in = BigInt(1, rand)
      dut.poke("io_in", in)
      dut.step()
      assert(dut.peek("io_out") == ((~in) & 1))
    }
    dut.step()
  }
}
