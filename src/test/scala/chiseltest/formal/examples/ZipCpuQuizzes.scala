// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal.examples

import chisel3.SyncReadMem.{ReadFirst, ReadUnderWrite, Undefined, WriteFirst}
import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.util._

/** Chisel versions of the quizzes from ZipCPU: http://zipcpu.com/quiz/quizzes.html
  */
class ZipCpuQuizzes extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  "Quiz1" should "pass with assumption" taggedAs FormalTag in {
    verify(new Quiz1(true), Seq(BoundedCheck(11), DefaultBackend))
  }
  "Quiz1" should "fail without assumption" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new Quiz1(false), Seq(BoundedCheck(11), DefaultBackend))
    }
    assert(e.failAt == 11)
  }

  "Quiz2" should "fail without counter reset value" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new Quiz2(false), Seq(BoundedCheck(5), DefaultBackend))
    }
    assert(e.failAt == 0)
  }
  "Quiz2" should "pass with counter reset value" taggedAs FormalTag in {
    verify(new Quiz2(true), Seq(BoundedCheck(5), DefaultBackend))
  }

  "Quiz3" should "fail induction check" taggedAs FormalTag in {
    DefaultBackend match {
      case BtormcEngineAnnotation => {}
      case anno => {
        val e = intercept[FailedInductionCheckException] {
          verify(new Quiz3(), Seq(InductionCheck(1), anno))
        }
        assert(e.failAt == 1)
      }
    }
  }

  "Quiz4" should "fail when using a RegNext to delay the signal" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new Quiz4(0), Seq(BoundedCheck(5), DefaultBackend))
    }
    assert(e.failAt == 0)
  }
  "Quiz4" should "pass when using the Chisel past function" taggedAs FormalTag in {
    verify(new Quiz4(1), Seq(BoundedCheck(5), DefaultBackend))
  }
  "Quiz4" should "pass when using the guarded assumption + Chisel past function" taggedAs FormalTag in {
    verify(new Quiz4(2), Seq(BoundedCheck(5), DefaultBackend))
  }

  "Quiz7" should "fail when using a RegNext to delay the signal" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new Quiz7(false), Seq(BoundedCheck(5), DefaultBackend))
    }
    assert(e.failAt == 0)
  }
  "Quiz7" should "pass when using the Chisel past function" taggedAs FormalTag in {
    verify(new Quiz7(true), Seq(BoundedCheck(5), DefaultBackend))
  }
  "Quiz11" should "fail induction" taggedAs FormalTag in {
    DefaultBackend match {
      case BtormcEngineAnnotation => {}
      case anno => {
        val e = intercept[FailedInductionCheckException] {
          verify(new Quiz11(false), Seq(InductionCheck(3), anno))
        }
        assert(e.failAt == 3)
      }
    }
  }
  "Quiz11" should "pass induction" taggedAs FormalTag in {
    DefaultBackend match {
      case BtormcEngineAnnotation => {}
      case anno => {
        verify(new Quiz11(true), Seq(InductionCheck(4), anno))
      }
    }
  }
  "Quiz13" should "pass when x is 1 wide" taggedAs FormalTag in {
    verify(new Quiz13(1), Seq(BoundedCheck(4), DefaultBackend))
  }
  "Quiz13" should "pass when x is 8 wide" taggedAs FormalTag in {
    verify(new Quiz13(8), Seq(BoundedCheck(4), DefaultBackend))
  }

  "Quiz15" should "fail when using ReadFirst" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new Quiz15(ReadFirst), Seq(BoundedCheck(5), DefaultBackend))
    }
    assert(e.failAt == 1)
  }
  "Quiz15" should "fail when using Undefined" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new Quiz15(Undefined), Seq(BoundedCheck(5), DefaultBackend))
    }
    assert(e.failAt == 1)
  }
  "Quiz15" should "pass when using WriteFirst" taggedAs FormalTag in {
    verify(new Quiz15(WriteFirst), Seq(BoundedCheck(5), DefaultBackend))
  }
  "Quiz17" should "fail induction" taggedAs FormalTag in {
    DefaultBackend match {
      case BtormcEngineAnnotation => {}
      case anno => {
        val e = intercept[FailedInductionCheckException] {
          verify(new Quiz17(22, false), Seq(InductionCheck(4), anno))
        }
      }
    }
  }
  "Quiz17" should "pass induction" taggedAs FormalTag in {
    DefaultBackend match {
      case BtormcEngineAnnotation => {}
      case anno => {
        verify(new Quiz17(22, true), Seq(InductionCheck(4), anno))
      }
    }
  }
}

/** http://zipcpu.com/quiz/2019/08/03/quiz01.html */
class Quiz1(withAssume: Boolean) extends Module {
  val counter = RegInit(0.U(16.W))
  counter := counter + 1.U
  assert(counter <= 10.U) // reduced from 100 -> 10 to make it fail faster
  if (withAssume) {
    assume(counter <= 9.U)
  }
}

/** http://zipcpu.com/quiz/2019/08/08/quiz02.html */
class Quiz2(withInit: Boolean) extends Module {
  val iStartSignal = IO(Input(Bool()))
  val oBusy = IO(Output(Bool()))
  val MaxAmount = 4.U(16.W) // reduced from 22 -> 4 for faster checking
  val counter = if (withInit) { RegInit(0.U(16.W)) }
  else { Reg(UInt(16.W)) }

  when(iStartSignal && counter === 0.U) {
    counter := MaxAmount - 1.U
  }.elsewhen(counter =/= 0.U) {
    counter := counter - 1.U
  }
  oBusy := counter =/= 0.U
  assert(counter < MaxAmount)
}

/** http://zipcpu.com/quiz/2019/08/19/quiz03.html */
class Quiz3() extends Module {
  val counter = RegInit(0.U(16.W))

  when(counter === 22.U) {
    counter := 0.U
  }.otherwise {
    counter := counter + 1.U
  }
  assert(counter =/= 500.U)
}

/** http://zipcpu.com/quiz/2019/08/24/quiz04.html */
class Quiz4(style: Int) extends Module {
  val iStartSignal = IO(Input(Bool()))
  val counter = RegInit(0.U(16.W))
  when(iStartSignal && counter === 0.U) {
    counter := 3.U // reduced from 23 -> 3 for faster checking
  }.elsewhen(counter =/= 0.U) {
    counter := counter - 1.U
  }

  assert(counter < 4.U) // reduced from 24 -> 4

  style match {
    case 0 => // similar to the original ZipCPU example
      assume(!iStartSignal)
      assert(RegNext(counter === 0.U))
    case 1 => // using chisel past
      assume(!iStartSignal)
      assert(past(counter === 0.U))
    case 2 => // the solution suggested by ZipCPU (implemented with Chisel past)
      when(past(counter === 0.U && !iStartSignal)) {
        assert(counter === 0.U)
      }
  }
}

/** https://zipcpu.com/quiz/2019/08/31/quiz05.html */
// since we do not have SVA style sequence support, this one is hard to make work

/** https://zipcpu.com/quiz/2019/09/06/quiz06.html */
// chisel is focused on synchronous circuits so we do not support clock events for now

/** https://zipcpu.com/quiz/2019/11/16/quiz07.html */
class Quiz7(fixed: Boolean) extends Module {
  val iStartSignal = IO(Input(Bool()))
  val oBusy = IO(Output(Bool()))
  val counter = RegInit(0.U(16.W))
  when(iStartSignal && counter === 0.U) {
    counter := 3.U // reduced from 21 -> 3 for faster checking
  }.elsewhen(counter =/= 0.U) {
    counter := counter - 1.U
  }
  oBusy := counter =/= 0.U

  if (fixed) {
    when(past(iStartSignal) && past(counter === 0.U)) {
      assert(counter === 3.U) // reduced from 21 -> 3 for faster checking
    }
  } else {
    when(RegNext(iStartSignal)) {
      assert(counter === 3.U) // reduced from 21 -> 3 for faster checking
    }
  }
}

/** http://zipcpu.com/quiz/2019/11/29/quiz08.html */
// A very similar example is in formal/examples/Counter.scala

/** http://zipcpu.com/quiz/2019/12/12/quiz09.html */
// this one is hard to translate to Chisel since we do not have blocking assignment

/** http://zipcpu.com/quiz/2020/01/17/quiz10.html */
class Quiz10 extends Module {
  val iStall = IO(Input(Bool()))
  val oRequest = IO(Output(Bool()))
  val oRequestDetails = IO(Output(UInt(16.W)))

  // Using the Chisel past function, this example is safe and will automatically be disabled
  // in the first cycle after reset.
  when(past(oRequest) && past(iStall)) {
    assert(oRequest)
    assert(stable(oRequestDetails))
  }
}

/** http://zipcpu.com/quiz/2020/01/23/quiz11.html */
class Quiz11(shouldPass: Boolean) extends Module {
  val i_ce = IO(Input(Bool()))
  val i_bit = IO(Input(Bool()))

  val sa = RegInit(0.U(4.W))
  val sb = RegInit(0.U(4.W))

  when(i_ce) {
    sa := Cat(sa(2, 0), i_bit)
    sb := Cat(i_bit, sb(3, 1))
  }

  if (shouldPass) {
    assert(sa === Reverse(sb))
  } else {
    assert(sa(3) === sb(0))
  }
}

/** http://zipcpu.com/quiz/2020/09/14/quiz13.html */
class Quiz13(xWidth: Int) extends Module {
  require(xWidth > 0)
  val x = IO(Input(UInt(xWidth.W)))

  assert(stable(x) === (x === past(x)))
  assert(changed(x) === (x =/= past(x)))
  // Chisel's rose function only operates on Bool, not on UInt of arbitrary sizes!
  // => thus we cannot (easily) make the same mistake as in the original question
  assert(rose(x(0)) === (x(0) && !past(x(0))))
  assert(fell(x(0)) === (!x(0) && past(x(0))))
}

/** http://zipcpu.com/quiz/2020/12/24/quiz14.html */
// TODO: consider implementing once we have good multi-clock support

/** http://zipcpu.com/quiz/2021/06/12/quiz15.html */
class Quiz15(readUnderWrite: ReadUnderWrite) extends Module {
  val iWrite = IO(Input(Bool()))
  val iData = IO(Input(UInt(32.W)))
  val iWAddr = IO(Input(UInt(8.W)))
  val iRead = IO(Input(Bool()))
  val iRAddr = IO(Input(UInt(8.W)))
  val oData = IO(Output(UInt(32.W)))

  val mem = SyncReadMem(256, chiselTypeOf(iData), readUnderWrite)
  when(iWrite) { mem.write(iWAddr, iData) }
  oData := mem.read(iRAddr, iRead)

  // This will only pass if we chose the write first behavior.
  // Read first will return an old value and undefined will result in an
  // arbitrary value to be read (the write is still performed and available
  // a cycle later).
  when(past(iWrite && iRead && iWAddr === iRAddr)) {
    assert(oData === past(iData))
  }
}

/** https://zipcpu.com/quiz/2021/07/10/quiz16.html */
// TODO: consider implementing once we have good async reset support

/** https://zipcpu.com/quiz/2021/08/05/quiz17.html */
class Quiz17(maxVal: Int, makePass: Boolean) extends Module {
  val i_start = IO(Input(Bool()))

  val counter = RegInit(0.U(16.W))
  val zero_counter = RegInit(true.B)

  when(counter > 0.U) {
    counter := counter - 1.U
    when(counter === 1.U) {
      zero_counter := true.B
    }
  }.elsewhen(i_start) {
    counter := maxVal.U
    zero_counter := false.B
  }

  when(past(counter === 1.U)) {
    assert(rose(zero_counter))
  }

  if (makePass) {
    assert(zero_counter === (counter === 0.U))
  }
}
