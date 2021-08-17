// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest.iotesters._
import chiseltest.simulator.RequiresVerilator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HasCycle extends Module {
  val io = IO( new Bundle {
    val a = Input(Bool())
    val o = Output(Bool())
  })

  val b = Wire(Bool())
  b := b&&io.a

  io.o := b
}

class HasCycleTester( c:HasCycle) extends PeekPokeTester(c) {
  poke( c.io.a, 0)
  step(1)
}

class HasCycleTest extends AnyFlatSpec with Matchers {
  behavior of "HasCycle"

  it should "work in the interpreter" in {
    Driver.execute(
      // interpreter has it's own loop detector that needs to be disabled as well with --fr-allow-cycles
      Array( "--no-check-comb-loops", "--backend-name", "firrtl", "--fr-allow-cycles"),
      () => new HasCycle) { c =>
      new HasCycleTester( c)
    } should be ( true)
  }
  it should "work in verilator" taggedAs RequiresVerilator in {
    Driver.execute(
              Array( "--no-check-comb-loops", "--backend-name", "verilator"),
      () => new HasCycle) { c =>
      new HasCycleTester( c)
    } should be ( true)
  }
}