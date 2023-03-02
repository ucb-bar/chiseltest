// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable.StopException

// scalastyle:off magic.number
class ClockSpec extends AnyFreeSpec with Matchers with LazyLogging {
  "ClockSpec should pass a basic test" in {
    val input =
      """
        |circuit Stop0 :
        |  module DUT :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output out2 : UInt<8>
        |
        |    reg reg2 : UInt<8>, clock with : (reset => (reset, UInt<8>("h07")))
        |    reg2 <= add(reg2, UInt<8>("h01"))
        |    out2 <= reg2
        |
        |  module Stop0 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output out1 : UInt<8>
        |    output out2 : UInt<8>
        |
        |    reg reg1 : UInt<8>, clock with : (reset => (reset, UInt<8>("h03")))
        |
        |    inst dut of DUT
        |
        |    dut.clock <= clock
        |    dut.reset <= reset
        |
        |    out1  <= reg1
        |    out2  <= dut.out2
        |
        |    reg1 <= add(reg1, UInt<8>("h01"))
        |    when gt(reg1, UInt<8>("h08")) :
        |      stop(clock, UInt(1), 0) ; Done!
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      intercept[StopException] {
        tester.step(100)
      }
      tester.engine.lastStopResult should be(Some(0))
    }
  }

  "clocks can have names other than clock" in {
    val input =
      """
        |circuit OddClockName :
        |  module OddClockName :
        |    input tardigrade : Clock
        |    input reset : UInt<1>
        |    output out2 : UInt<8>
        |
        |    reg reg2 : UInt<8>, tardigrade with : (reset => (reset, UInt<8>("h0")))
        |    reg2 <= add(reg2, UInt<8>("h01"))
        |    out2 <= reg2
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.step(100)
      tester.expect("out2", 100)
    }
  }

  "clocks must behave properly behind validif" in {
    val input =
      """
        |circuit ClockedValidIf :
        |  module ClockedValidIf :
        |    input clock    : Clock
        |    input reset    : UInt<1>
        |    input in1      : UInt<16>
        |    input valid1   : UInt<1>
        |    input valid2   : UInt<1>
        |    input addr     : UInt<8>
        |    input write_en : UInt<1>
        |    output out1    : UInt<16>
        |
        |    reg reg1 : UInt<16>, clock with : (reset => (reset, UInt<8>("h07")))
        |    reg1 <= add(in1, UInt<16>(1))
        |
        |    node clock2 = validif(valid1, clock)
        |
        |    mem m :
        |      data-type => UInt<16>
        |      depth => 8
        |      read-latency => 0
        |      write-latency => 1
        |      reader => read
        |      writer => write
        |
        |    m.read.clk <= clock2
        |    m.read.en <= eq(write_en, UInt<1>(0))
        |    m.read.addr <= addr
        |
        |    m.write.clk <= clock2
        |    m.write.en <= eq(write_en, UInt<1>(1))
        |    m.write.mask <= UInt<8>("hff")
        |    m.write.addr <= addr
        |    m.write.data <= in1
        |
        |    out1 <= m.read.data
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), ValidIfIsRandomAnnotation)) { tester =>
      // load memory
      tester.poke("write_en", 1)
      for (i <- 0 until 8) {
        tester.poke("addr", i)
        tester.poke("in1", i * 10 + i)
        tester.step()
      }

      logger.debug {
        (0 until 8).map { i => s"memory($i) = ${tester.peekMemory("m", i)}" }.mkString("\n")
      }
      // read phase
      tester.poke("write_en", 0)
      for (i <- 0 until 8) {
        tester.poke("addr", i)
        tester.expect("out1", i * 10 + i)

        logger.debug(s"mem($i) ${tester.peekMemory("m", i)}")
        tester.step()
      }
    }
  }
}
