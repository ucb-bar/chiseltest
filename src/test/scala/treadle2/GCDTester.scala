// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GCDTester extends AnyFlatSpec with Matchers with LazyLogging {
  def computeGcd(a: Int, b: Int): (Int, Int) = {
    var x = a
    var y = b
    var depth = 1
    while (y > 0) {
      if (x > y) {
        x -= y
      } else {
        y -= x
      }
      depth += 1
    }
    (x, depth)
  }

  behavior.of("GCD")

  def sizableTest(width: Int): Unit = {
    val gcdFirrtl: String =
      s"""
         |circuit GCD :
         |  module GCD :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_a : UInt<$width>
         |    input io_b : UInt<$width>
         |    input io_e : UInt<1>
         |    output io_z : UInt<$width>
         |    output io_v : UInt<1>
         |    reg x : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), x)
         |    reg y : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), y)
         |    node T_13 = gt(x, y)
         |    node T_14 = sub(x, y)
         |    node T_15 = tail(T_14, 1)
         |    node T_17 = eq(T_13, UInt<1>("h0"))
         |    node T_18 = sub(y, x)
         |    node T_19 = tail(T_18, 1)
         |    node T_21 = eq(y, UInt<1>("h0"))
         |    node GEN_0 = mux(T_13, T_15, x)
         |    x <= mux(io_e, io_a, GEN_0)
         |    node GEN_1 = mux(T_17, T_19, y)
         |    y <= mux(io_e, io_b, GEN_1)
         |    io_z <= x
         |    io_v <= T_21
    """.stripMargin

    val values =
      for {
        x <- 10 to 100
        y <- 10 to 100
      } yield (x, y, computeGcd(x, y)._1)

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(gcdFirrtl))) { tester =>
      val startTime = System.nanoTime()
      tester.poke("clock", 1)

      for ((x, y, z) <- values) {
        tester.step()
        tester.poke("io_a", x)
        tester.poke("io_b", y)
        tester.poke("io_e", 1)
        tester.step()

        tester.poke("io_e", 0)
        tester.step()

        var count = 0
        while (tester.peek("io_v") != Big1) {
          count += 1
          tester.step()
        }

        tester.expect("io_z", BigInt(z))
      }
      val endTime = System.nanoTime()
      val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

      val cycle = 11 // tester.engine.circuitState.stateCounter

      logger.info(
        f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
      )
    }
  }

  //scalastyle:off
  def manyValuesTest(width: Int): Unit ={
    val gcdFirrtl: String =
      s"""
         |circuit GCD :
         |  module GCD :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_a : UInt<$width>
         |    input io_b : UInt<$width>
         |    input io_e : UInt<1>
         |    output io_z : UInt<$width>
         |    output io_v : UInt<1>
         |    reg x : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), x)
         |    reg y : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), y)
         |    node T_13 = gt(x, y)
         |    node T_14 = sub(x, y)
         |    node T_15 = tail(T_14, 1)
         |    node T_17 = eq(T_13, UInt<1>("h0"))
         |    node T_18 = sub(y, x)
         |    node T_19 = tail(T_18, 1)
         |    node T_21 = eq(y, UInt<1>("h0"))
         |    node GEN_0 = mux(T_13, T_15, x)
         |    x <= mux(io_e, io_a, GEN_0)
         |    node GEN_1 = mux(T_17, T_19, y)
         |    y <= mux(io_e, io_b, GEN_1)
         |    io_z <= x
         |    io_v <= T_21
    """.stripMargin

    val values =
      for {
        x <- 1 to 100
        y <- 1 to 100
      } yield (x, y, computeGcd(x, y)._1)

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(gcdFirrtl))) { tester =>
      val startTime = System.nanoTime()
      tester.poke("clock", 1)

      for ((x, y, z) <- values) {
        tester.step()
        tester.poke("io_a", x)
        tester.poke("io_b", y)
        tester.poke("io_e", 1)
        tester.step()

        tester.poke("io_e", 0)
        tester.step()

        while (tester.peek("io_v") != Big1) {
          tester.step()
        }

        tester.expect("io_z", z)
      }
      val endTime = System.nanoTime()
      val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

      val cycle = tester.cycleCount

      logger.info(
        f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
      )
    }

  }

  it should "run with InterpretedTester at Int size 16" in {
    sizableTest(16)
  }

  it should "run with InterpretedTester at Int size 44" in {
    sizableTest(44)
  }

  it should "run with InterpretedTester at size 68" in {
    sizableTest(68)
  }

  it should "run a lot of values" in {
    manyValuesTest(24)
  }
}
