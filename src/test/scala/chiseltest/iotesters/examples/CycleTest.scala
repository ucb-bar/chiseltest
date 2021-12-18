// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest._
import chiseltest.iotesters._
import chiseltest.simulator.RequiresVerilator
import firrtl.transforms.DontCheckCombLoopsAnnotation
import org.scalatest.flatspec.AnyFlatSpec

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

class HasCycleTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "HasCycle"

  it should "work in treadle" in {
    // for treadle we need to use a treadle specific annotation: AllowCyclesAnnotation
    test(new HasCycle)
      .withAnnotations(Seq(DontCheckCombLoopsAnnotation, TreadleBackendAnnotation, treadle.AllowCyclesAnnotation))
      .runPeekPoke(new HasCycleTester(_))
  }
  it should "work in verilator" taggedAs RequiresVerilator in {
    test(new HasCycle).withAnnotations(Seq(DontCheckCombLoopsAnnotation, VerilatorBackendAnnotation))
      .runPeekPoke(new HasCycleTester(_))
  }
}