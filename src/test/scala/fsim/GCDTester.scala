// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

import org.scalatest.freespec.AnyFreeSpec

/** Test correctness by executing a known-to-be-correct GCD circuit. For a benchmarking version, have a look at the
  * `benchmark` project.
  */
class GCDTester extends AnyFreeSpec {
  def circuitSrc(width: Int): String =
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

  def sizableTest(width: Int, from: Long, upTo: Long): Unit = {
    val values =
      for {
        x <- from to upTo
        y <- from to upTo
      } yield (x, y, BigInt(x).gcd(y).toLong)
    val sim = new Simulation(Compiler.run(FirrtlCompiler.toLow(circuitSrc(width))))
    val (io_a, io_b, io_e) = (sim.getSymbolId("io_a"), sim.getSymbolId("io_b"), sim.getSymbolId("io_e"))
    val (io_v, io_z) = (sim.getSymbolId("io_v"), sim.getSymbolId("io_z"))

    for ((x, y, z) <- values) {
      sim.step()
      sim.pokeLong(io_a, x)
      sim.pokeLong(io_b, y)
      sim.pokeBool(io_e, true)
      sim.step()

      sim.pokeBool(io_e, false)
      sim.step()

      var count = 0
      while (!sim.peekBool(io_v)) {
        count += 1
        sim.step()
      }

      assert(sim.peekLong(io_z) == z)
    }
    sim.finish()
  }

  "run with Simulation at Int size 16" in {
    sizableTest(16, from = 2, upTo = 100)
  }

  "run with Simulation at Int size 44" in {
    sizableTest(44, from = 2, upTo = 100)
  }

  "run with Simulation at size 64" in {
    sizableTest(64, from = 2, upTo = 100)
  }
}
