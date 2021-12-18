// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PastTestModule(sel: Int) extends Module {
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))

  out := RegNext(RegNext(in))

  // all of these should be equivalent!
  sel match {
    case 0 => assert(past(past(in)) === out)
    case 1 => assert(out === past(past(in)))
    case 2 => assert(past(in, 2) === out)
  }
}

class PastWhenTestModule extends Module {
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))

  out := RegNext(in, init = 0.U)
  when(past(in) === 11.U) {
    assert(out === 11.U)
  }
}

class PastMemTestModule extends Module {
  val addr = IO(Input(UInt(8.W)))
  val data = IO(Input(UInt(32.W)))
  val aEn = IO(Input(Bool()))
  val bEn = IO(Input(Bool()))

  val m = Mem(32, UInt(32.W))
  // we have two write ports that can collide
  when(aEn) { m.write(addr, data) }
  when(bEn) { m.write(addr, data) }
  // exactly one port is always active
  assume(aEn ^ bEn)

  // we delay the data manually to make sure the `Past(addr)` is correctly propagated through the combinatorial read port
  assert(m.read(past(addr)) === RegNext(data))
}

class SafePastSpec extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "Safe Past"

  (0 until 3).foreach { ii =>
    it should s"guard the assertions appropriately (style=$ii)" taggedAs FormalTag in {
      verify(new PastTestModule(ii), Seq(BoundedCheck(4), DefaultBackend))
    }
  }

  it should "guard the assertion appropriately even when past is used in a surrounding when statement" taggedAs FormalTag in {
    verify(new PastWhenTestModule, Seq(BoundedCheck(4), DefaultBackend))
  }

  it should "correctly propagate delay information through a combinatorial read port" taggedAs FormalTag in {
    verify(new PastMemTestModule, Seq(BoundedCheck(4), DefaultBackend))
  }
}
