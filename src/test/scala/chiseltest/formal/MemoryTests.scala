// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import chisel3.SyncReadMem.ReadUnderWrite
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3._
import chisel3.util._
import chisel3.experimental.verification
import firrtl.ir.ReadUnderWrite

// most of the tests are inspired by the MemorySpec in firrtl.backends.experimental.smt.end2end
class MemoryTests extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "Registered read-first memory" should "return written data after two cycles" taggedAs FormalTag in {
    verify(new ReadFirstMemoryReturnsDataAfterTwoCycles, Seq(BoundedCheck(3)))
  }
  "Registered read-first memory" should "not return written data after one cycle" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new ReadFirstMemoryReturnsDataAfterOneCycle, Seq(BoundedCheck(3)))
    }
    assert(e.failAt == 1)
  }
}

class SyncMemTestModule(readUnderWrite: ReadUnderWrite) extends Module {
  val writeAddr = IO(Input(UInt(5.W)))
  val readAddr = IO(Input(UInt(5.W)))
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))

  val m = SyncReadMem(32, UInt(8.W), readUnderWrite)
  m.write(writeAddr, in)
  val readValue = m.read(readAddr, true.B)
  out := readValue

  val cycle = RegInit(0.U(8.W))
  cycle := Mux(cycle === 100.U, 100.U, cycle + 1.U)
}

class ReadFirstMemoryReturnsDataAfterTwoCycles extends SyncMemTestModule(ReadUnderWrite.Old) {
  val pastWriteAddr = RegNext(writeAddr)
  when(cycle >= 1.U) {
    // we assume the we read from the address that we last wrote to
    verification.assume(readAddr === pastWriteAddr)
  }
  val pastPastIn = RegNext(RegNext(in))
  when(cycle >= 2.U) {
    verification.assert(out === pastPastIn)
  }
}

class ReadFirstMemoryReturnsDataAfterOneCycle extends SyncMemTestModule(ReadUnderWrite.Old) {
  verification.assume(readAddr === writeAddr)
  val pastIn = RegNext(in)
  when(cycle >= 1.U) {
    verification.assert(out === pastIn)
  }
}
