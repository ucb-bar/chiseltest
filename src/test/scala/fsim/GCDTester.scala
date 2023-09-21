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

class DecoupledGCDTests extends AnyFreeSpec {
  private def circuitSrc(width: Int): String =
    s"""
       |circuit DecoupledGCD :
       |  module DecoupledGCD :
       |    input clock : Clock
       |    input reset : UInt<1>
       |    input input : { flip ready : UInt<1>, valid : UInt<1>, bits : { value1 : UInt<$width>, value2 : UInt<$width>}}
       |    output output : { flip ready : UInt<1>, valid : UInt<1>, bits : { value1 : UInt<$width>, value2 : UInt<$width>, gcd : UInt<$width>}}
       |
       |    reg xInitial : UInt<$width>, clock with :
       |      reset => (UInt<1>("h0"), xInitial) @[main.scala 27:24]
       |    reg yInitial : UInt<$width>, clock with :
       |      reset => (UInt<1>("h0"), yInitial) @[main.scala 28:24]
       |    reg x : UInt<$width>, clock with :
       |      reset => (UInt<1>("h0"), x) @[main.scala 29:24]
       |    reg y : UInt<$width>, clock with :
       |      reset => (UInt<1>("h0"), y) @[main.scala 30:24]
       |    reg busy : UInt<1>, clock with :
       |      reset => (reset, UInt<1>("h0")) @[main.scala 31:28]
       |    reg resultValid : UInt<1>, clock with :
       |      reset => (reset, UInt<1>("h0")) @[main.scala 60:28]
       |    node _input_ready_T = eq(busy, UInt<1>("h0")) @[main.scala 34:18]
       |    input.ready <= _input_ready_T @[main.scala 34:15]
       |    output.valid <= resultValid @[main.scala 35:16]
       |    output.bits.gcd is invalid @[main.scala 36:15]
       |    output.bits.value2 is invalid @[main.scala 36:15]
       |    output.bits.value1 is invalid @[main.scala 36:15]
       |    reg cycle : UInt<$width>, clock with :
       |      reset => (reset, UInt<$width>("h0")) @[main.scala 38:22]
       |    node _cycle_T = add(cycle, UInt<1>("h1")) @[main.scala 39:18]
       |    node _cycle_T_1 = tail(_cycle_T, 1) @[main.scala 39:18]
       |    cycle <= _cycle_T_1 @[main.scala 39:9]
       |    when busy : @[main.scala 44:15]
       |      node _T = geq(x, y) @[main.scala 45:12]
       |      when _T : @[main.scala 45:18]
       |        node _x_T = sub(x, y) @[main.scala 46:14]
       |        node _x_T_1 = tail(_x_T, 1) @[main.scala 46:14]
       |        x <= _x_T_1 @[main.scala 46:9]
       |      else :
       |        node _y_T = sub(y, x) @[main.scala 48:14]
       |        node _y_T_1 = tail(_y_T, 1) @[main.scala 48:14]
       |        y <= _y_T_1 @[main.scala 48:9]
       |      node _T_1 = eq(x, UInt<1>("h0")) @[main.scala 50:12]
       |      node _T_2 = eq(y, UInt<1>("h0")) @[main.scala 50:25]
       |      node _T_3 = or(_T_1, _T_2) @[main.scala 50:20]
       |      when _T_3 : @[main.scala 50:34]
       |        node _T_4 = eq(x, UInt<1>("h0")) @[main.scala 51:14]
       |        when _T_4 : @[main.scala 51:23]
       |          output.bits.gcd <= y @[main.scala 52:25]
       |        else :
       |          output.bits.gcd <= x @[main.scala 54:25]
       |        output.bits.value1 <= xInitial @[main.scala 57:26]
       |        output.bits.value2 <= yInitial @[main.scala 58:26]
       |        resultValid <= UInt<1>("h1") @[main.scala 59:19]
       |        node _T_5 = and(output.ready, resultValid) @[main.scala 61:25]
       |        when _T_5 : @[main.scala 61:41]
       |          busy <= UInt<1>("h0") @[main.scala 62:14]
       |          resultValid <= UInt<1>("h0") @[main.scala 63:21]
       |    else :
       |      when input.valid : @[main.scala 67:23]
       |        input.ready <= UInt<1>("h1") @[Decoupled.scala 81:20]
       |        x <= input.bits.value1 @[main.scala 69:9]
       |        y <= input.bits.value2 @[main.scala 70:9]
       |        xInitial <= input.bits.value1 @[main.scala 71:16]
       |        yInitial <= input.bits.value2 @[main.scala 72:16]
       |        busy <= UInt<1>("h1") @[main.scala 73:12]
       |""".stripMargin

  private def doTest(dut: Simulation, testValues: Iterable[(Long, Long, Long)]): Long = {
    val reset = dut.getSymbolId("reset")
    val (output_ready, output_valid) = (dut.getSymbolId("output_ready"), dut.getSymbolId("output_valid"))
    val (output_bits_gcd, input_valid) = (dut.getSymbolId("output_bits_gcd"), dut.getSymbolId("input_valid"))
    val (input_bits_value1, input_bits_value2) =
      (dut.getSymbolId("input_bits_value1"), dut.getSymbolId("input_bits_value2"))

    var cycles = 0L
    dut.pokeBool(reset, true)
    dut.step()
    dut.step()
    cycles += 2
    dut.pokeBool(reset, false)

    dut.pokeBool(output_ready, true)
    for ((i, j, expected) <- testValues) {
      dut.pokeLong(input_bits_value1, i)
      dut.pokeLong(input_bits_value2, j)
      dut.pokeBool(input_valid, true)
      dut.step()
      cycles += 1

      while (!dut.peekBool(output_valid)) {
        dut.step()
        cycles += 1
      }
      assert(dut.peekLong(output_bits_gcd) == expected)
      assert(dut.peekBool(output_valid))
    }

    cycles
  }

  def sizableTest(width: Int, from: Long, upTo: Long): Unit = {
    val values =
      for {
        x <- from to upTo
        y <- from to upTo
      } yield (x, y, BigInt(x).gcd(y).toLong)
    val sim = new Simulation(Compiler.run(FirrtlCompiler.toLow(circuitSrc(width))))
    doTest(sim, values)
    sim.finish()
  }

  "run decoupled GCD with width 16" in {
    sizableTest(16, from = 2, upTo = 100)
  }

  "run decoupled GCD with width 44" in {
    sizableTest(44, from = 2, upTo = 100)
  }

  "run decoupled GCD with width 64" in {
    sizableTest(64, from = 2, upTo = 100)
  }
}
