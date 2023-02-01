// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import logger.{LazyLogging, LogLevel, LogLevelAnnotation}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import treadle2.utils.NameBasedRandomNumberGenerator

import scala.util.Random

class RandomizeCircuitTest extends AnyFreeSpec with Matchers with LazyLogging {
  "name based random numbers should have distributions that are similar to regular random numbers" in {
    val samples = 10000

    val nameBasedRandomNumberGenerator = new NameBasedRandomNumberGenerator

    val a = Array.tabulate(samples) { i =>
      Random.nextInt(1 << 16).toLong
    }
    val b = Array.tabulate(samples) { i =>
      val registerName = s"reg$i"
      nameBasedRandomNumberGenerator.nextBigInt(registerName, deviationSeed = 0L, 16).toLong
    }

    a.zip(b).foreach { case (r1, r2) =>
      logger.debug(f"$r1%10d   $r2%10d")
    }

    val avg1 = a.sum / samples.toDouble
    val avg2 = b.sum / samples.toDouble
    def delta(a: Long, b: Double): Double = {
      (a - b) * (a - b)
    }
    val sigma1 = math.sqrt(a.map { a1 =>
      delta(a1, avg1)
    }.sum / samples.toDouble)
    val sigma2 = math.sqrt(b.map { b1 =>
      delta(b1, avg2)
    }.sum / samples.toDouble)

    (avg1 - avg2).abs / avg1 must be < 0.03
    (sigma1 - sigma2).abs / sigma1 must be < 0.03

    logger.info(f"$avg1%10.2f  $avg2%10.2f")
    logger.info(f"$sigma1%10.2f  $sigma2%10.2f")
  }

  "circuits can be randomized" in {
    val input =
      s"""
         |circuit ToBeRandomized :
         |  module ToBeRandomized :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    output io_r_data : UInt<8>
         |    input  io_r_addr : UInt<3>
         |    input  io_r_en   : UInt<1>
         |    input  io_w_data : UInt<8>
         |    input  io_w_addr : UInt<3>
         |    input  io_w_en   : UInt<1>
         |    input  io_w_mask : UInt<1>
         |    input  io_in_a   : UInt<16>
         |    output io_out_a  : UInt<16>
         |
         |    mem m :
         |      data-type => UInt<8>
         |      depth => 5
         |      read-latency => 1
         |      write-latency => 1
         |      reader => r
         |      writer => w
         |
         |    io_r_data   <= m.r.data
         |    m.r.addr    <= io_r_addr
         |    m.r.en      <= io_r_en
         |    m.r.clk     <= clock
         |
         |    m.w.data <= io_w_data
         |    m.w.addr <= io_w_addr
         |    m.w.en   <= io_w_en
         |    m.w.mask <= io_w_mask
         |    m.w.clk  <= clock
         |
         |    reg reg1 : UInt<16>, clock with : (reset => (reset, UInt(1)))
         |    reg reg2 : UInt<16>, clock with : (reset => (reset, UInt(2)))
         |    reg reg3 : UInt<16>, clock with : (reset => (reset, UInt(3)))
         |    reg reg4 : UInt<16>, clock with : (reset => (reset, UInt(4)))
         |
         |    reg reg11 : UInt<16>, clock with : (reset => (reset, UInt(11)))
         |    reg reg12 : UInt<16>, clock with : (reset => (reset, UInt(12)))
         |
         |    reg1 <= io_in_a
         |    reg2 <= add(io_in_a, io_in_a)
         |    reg3 <= add(io_in_a, UInt(1))
         |    reg4 <= add(io_in_a, UInt(17))
         |    node dog = and(reg1, reg2)
         |    node cat = and(reg3, reg4)
         |    reg11 <= dog
         |    reg12 <= cat
         |    node rat = xor(reg11, reg12)
         |    io_out_a <= rat
         |
         |
         |""".stripMargin

    TreadleTestHarness(
      Seq(
        FirrtlSourceAnnotation(input),
        RandomizeAtStartupAnnotation,
        LogLevelAnnotation(LogLevel.None)
      )
    ) { tester =>
      val regNames = Seq(1, 2, 3, 4, 11, 12).map { n =>
        f"reg$n"
      }
      val saveRegs = regNames.map { name =>
        name -> tester.peek(name)
      }.toMap

      logger.info(regNames.map { s =>
        f"$s%10s"
      }.mkString(""))
      logger.info(regNames.map { s =>
        f"${tester.peek(s)}%10d"
      }.mkString(""))

      tester.poke("io_in_a", 7)
      tester.step(10)

      logger.info(regNames.map { s =>
        f"${tester.peek(s)}%10d"
      }.mkString(""))
      tester.peek("reg1") must be(BigInt(7))
      saveRegs.exists { case (name, savedValue) => savedValue != tester.peek(name) } must be(true)

      tester.randomize()

      logger.info(regNames.map { s =>
        f"${tester.peek(s)}%10d"
      }.mkString(""))
      regNames.forall { name =>
        saveRegs(name) == tester.peek(name)
      } must be(true)
    }
  }
}
