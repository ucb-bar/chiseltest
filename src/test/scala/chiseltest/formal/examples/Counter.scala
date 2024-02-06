// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal.examples

import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec

class CounterVerify extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  "Counter" should "succeed BMC" taggedAs FormalTag in {
    verify(new Counter(65000, 60000), Seq(BoundedCheck(10), DefaultBackend))
  }

  "Counter" should "Fail induction" taggedAs FormalTag in {
    verify(new Counter(65000, 50), Seq(InductionCheck(10), DefaultBackend))
  }
}

class Counter(to: Int, assert_bound: Int) extends Module {
  val en = IO(Input(Bool()))
  val out = IO(Output(UInt(16.W)))

  val cnt = RegInit(0.U(16.W))
  out := cnt
  when(en) {
    when(cnt === to.U) {
      cnt := 0.U
    }.otherwise {
      cnt := cnt + 1.U;
    }
  }

  assert(cnt <= assert_bound.U(16.W))
}
