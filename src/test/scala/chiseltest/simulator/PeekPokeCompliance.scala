// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import org.scalatest.Tag

import scala.util.Random

/** Compliance tests for the `peek` and `poke` functions of the [[SimulatorContext]] interface.  */
abstract class PeekPokeCompliance(sim: Simulator, tag: Tag = DefaultTag) extends ComplianceTest(sim, tag) {
  behavior of sim.name

  private val bufferedInverter =
    """circuit test:
      |  module test:
      |    input clock: Clock
      |    input in: UInt<1>
      |    output out: UInt<1>
      |
      |    reg r : UInt<1>, clock with :
      |      reset => (UInt<1>(0), r)
      |    r <= not(in)
      |    out <= r
      |
      |""".stripMargin

  it should "poke inputs and peek outputs" taggedAs(tag) in {
    val dut = load(bufferedInverter)

    dut.poke("in", 1)
    dut.step()
    assert(dut.peek("out") == 0)

    dut.poke("in", 0)
    dut.step()
    assert(dut.peek("out") == 1)

    dut.finish()
  }

  it should "peek inputs as well as outputs" taggedAs(tag) in {
    val dut = load(bufferedInverter)

    dut.poke("in", 1)
    dut.step()
    assert(dut.peek("out") == 0)
    // peek an input
    assert(dut.peek("in") == 1)

    dut.finish()
  }

  private def comb(f: String, bits: Int): String =
    s"""circuit test:
       |  module test:
       |    input a : UInt<$bits>
       |    input b : UInt<$bits>
       |    output o : UInt<$bits>
       |    o <= $f
       |""".stripMargin

  it should "propagate values through the circuit" taggedAs(tag) in {
    val rand = new Random(0)
    val dut = load(comb("a", 43))

    val value = BigInt(43, rand)
    dut.poke("a", value)
    assert(dut.peek("o") == value)

    dut.finish()
  }

  it should "propagate values of variable widths through the circuit" taggedAs(tag) in {
    val Widths = Seq(1, 2, 3, 5, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129,
      300, 400, 500, 600, 1023, 1024, 1025)
    val lines = Seq("circuit test:", "  module test:", "    input clock: Clock") ++
      Widths.flatMap(w => Seq(s"    input in$w : UInt<$w>",   s"    output out$w : UInt<$w>")) ++ Seq("") ++
      Widths.map( w => s"    out$w <= in$w") ++ Seq("")
    val src = lines.mkString("\n")
    // println(src)
    val dut = load(src) //, Seq(WriteVcdAnnotation))

    val rand = new Random(0)
    Widths.foreach { w =>
      val values = Seq.fill(20)(BigInt(w, rand))
      val res = values.map { in =>
        dut.poke(s"in$w", in)
        dut.step()
        dut.peek(s"out$w")
      }
      if(res != values) { dut.finish() }
      assert(res == values, s"width = $w")
    }
  }

  it should "propagate SInt values of variable widths through the circuit" taggedAs(tag) in {
    val Widths = Seq(1, 2, 3, 5, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129,
      300, 400, 500, 600, 1023, 1024, 1025)
    val lines = Seq("circuit test:", "  module test:", "    input clock: Clock") ++
      Widths.flatMap(w => Seq(s"    input in$w : SInt<$w>",   s"    output out$w : SInt<$w>")) ++ Seq("") ++
      Widths.map( w => s"    out$w <= in$w") ++ Seq("")
    val src = lines.mkString("\n")
    // println(src)
    val dut = load(src, Seq(WriteVcdAnnotation))

    val rand = new Random(0)
    Widths.foreach { w =>
      val positiveValues = Seq.fill(20)(BigInt(w-1, rand))
      val values = positiveValues ++ positiveValues.map(v => -v)
      val res = values.map { in =>
        dut.poke(s"in$w", in)
        dut.step()
        dut.peek(s"out$w")
      }
      if(res != values) { dut.finish() }
      assert(res == values, s"width = $w")
    }
  }

}
