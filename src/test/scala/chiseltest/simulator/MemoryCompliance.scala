// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl.annotations.{CircuitTarget, MemoryArrayInitAnnotation, MemoryFileInlineAnnotation, MemoryScalarInitAnnotation}
import org.scalatest.Tag

import scala.util.Random

/** Compliance tests for the `peek` and `poke` functions of the [[SimulatorContext]] interface.  */
abstract class MemoryCompliance(sim: Simulator, tag: Tag = DefaultTag) extends ComplianceTest(sim, tag) {
  behavior of sim.name

  private val readMem =
    """circuit test:
      |  module test:
      |    input clock : Clock
      |    input reset : UInt<1>
      |    input addr : UInt<2>
      |    output value : UInt<8>
      |
      |    mem m:
      |      data-type => UInt<8>
      |      depth => 3
      |      reader => r
      |      read-latency => 1
      |      write-latency => 1
      |      read-under-write => new
      |
      |    m.r.clk <= clock
      |    m.r.en <= UInt(1)
      |    m.r.addr <= addr
      |    value <= m.r.data
      |""".stripMargin
  val mRef = CircuitTarget("test").module("test").ref("m")

  private def checkMem(dut: SimulatorContext, expected: Seq[BigInt]): Unit = {
    expected.zipWithIndex.foreach { case (expect, addr) =>
      dut.poke("addr", addr)
      dut.step()
      assert(dut.peek("value") == expect)
    }
    dut.step()
    dut.finish()
  }

  it should "support initializing a memory with a scalar constant" taggedAs tag in {
    val dut = load(readMem, Seq(MemoryScalarInitAnnotation(mRef, 123)))
    checkMem(dut, Seq(123, 123, 123))
  }

  it should "support initializing a memory with a list" taggedAs tag in {
    val dut = load(readMem, Seq(MemoryArrayInitAnnotation(mRef, Seq(123, 231, 234))))
    checkMem(dut, Seq(123, 231, 234))
  }

  it should "support initializing a memory from a file" taggedAs tag in {
    val dut = load(readMem, Seq(MemoryFileInlineAnnotation(mRef, "src/test/resources/init.mem")))
    checkMem(dut, Seq(0xab, 0xcd, 0xef))
  }
}