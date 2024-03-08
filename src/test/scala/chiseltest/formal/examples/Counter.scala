// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal.examples

import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec

class CounterVerify extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  "Counter" should "pass BMC" taggedAs FormalTag in {
    verify(new Counter(65000, 60000), Seq(BoundedCheck(3), DefaultBackend))
  }

  "Counter" should "fail induction" taggedAs FormalTag in {
    // btormc induction is unsupported
    DefaultBackend match {
      case BtormcEngineAnnotation => {}
      case anno => {
        val e = intercept[FailedInductionCheckException] {
          verify(new Counter(65000, 64999), Seq(InductionCheck(1), anno))
        }
        assert(e.failAt == 1)
      }
    }
  }

  "Counter" should "pass induction" taggedAs FormalTag in {
    // btormc induction is unsupported
    DefaultBackend match {
      case BtormcEngineAnnotation => {}
      case anno                   => verify(new Counter(65000, 65000), Seq(InductionCheck(1), anno))
    }
  }

  "Counter" should "Fail BMC step of induction" taggedAs FormalTag in {
    // btormc induction is unsupported
    DefaultBackend match {
      case BtormcEngineAnnotation => {}
      case anno => {
        val e = intercept[FailedBoundedCheckException] {
          verify(new Counter(65000, 4), Seq(InductionCheck(8), anno))
        }
        assert(e.failAt == 5)
      }
    }
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
