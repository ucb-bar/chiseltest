// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3._


class BasicBoundedCheckTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "verify command"

  it should "support modules that do not have any asserts (trivial pass!)"  taggedAs FormalTag in {
    verify(new AdderWithoutAsserts, Seq(BoundedCheck(1), DefaultBackend))
  }

  it should "support simple bmc with a passing check" taggedAs FormalTag in {
    // this one should pass since we only check one step
    verify(new FailAfterModule(2), Seq(BoundedCheck(kMax = 1), DefaultBackend))
  }

  it should "support simple bmc with a failing check" taggedAs FormalTag in {
    // this one will fail
    assertThrows[FailedBoundedCheckException] {
      verify(new FailAfterModule(2), Seq(BoundedCheck(kMax = 2), DefaultBackend))
    }
  }

  it should "support assumption" taggedAs FormalTag in {
    verify(new AssumeAssertTestModule, Seq(BoundedCheck(kMax = 1), DefaultBackend))
  }

  it should "support assumption in nested modules" taggedAs FormalTag in {
    verify(new NestedAssertAssumeTestModule, Seq(BoundedCheck(kMax = 1), DefaultBackend))
  }

  it should "detect a failure in DanielModuleWithBadAssertion" taggedAs FormalTag in {
    assertThrows[FailedBoundedCheckException] {
      verify(new DanielModuleWithBadAssertion, Seq(BoundedCheck(kMax = 4), DefaultBackend))
    }
  }

  it should "verify DanielModuleWithGoodAssertion" taggedAs FormalTag in {
    verify(new DanielModuleWithGoodAssertion, Seq(BoundedCheck(kMax = 4), DefaultBackend))
  }

  it should "support simple bmc with the RawTester" taggedAs FormalTag in {
    RawTester.verify(new FailAfterModule(2), Seq(BoundedCheck(kMax = 1), DefaultBackend))
  }

  it should "support simple bmc with a failing check with the RawTester" taggedAs FormalTag in {
    assertThrows[FailedBoundedCheckException] {
      RawTester.verify(new FailAfterModule(2), Seq(BoundedCheck(kMax = 2), DefaultBackend))
    }
  }
}

class AdderWithoutAsserts extends Module {
  val a = IO(Input(UInt(8.W)))
  val b = IO(Input(UInt(8.W)))
  val r = IO(Output(UInt(8.W)))
  r := a + b
}

class AssumeAssertTestModule extends Module {
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))
  out := in + 1.U
  assume(in > 12.U && in < 255.U)
  assert(out > 13.U)
}

class NestedAssertAssumeTestModule extends Module {
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))
  val child = Module(new AssumeAssertTestModule)
  child.in := in
  out := child.out
}

// from Daniel Kasza's dank-formal library
class DanielModuleWithBadAssertion extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val a = Output(Bool())
    val b = Output(Bool())
  })

  val aReg = RegInit(true.B)
  val bReg = RegInit(false.B)
  io.a := aReg
  io.b := bReg

  when (io.in) {
    aReg := true.B
    bReg := false.B
  }.otherwise {
    aReg := false.B
    bReg := true.B
  }

  assert(aReg && bReg)
}

// from Daniel Kasza's dank-formal library
class DanielModuleWithGoodAssertion extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val a = Output(Bool())
    val b = Output(Bool())
  })

  val aReg = RegInit(true.B)
  val bReg = RegInit(false.B)
  io.a := aReg
  io.b := bReg

  when (io.in) {
    aReg := true.B
    bReg := false.B
  }.otherwise {
    aReg := false.B
    bReg := true.B
  }

  assert(aReg ^ bReg)
}
