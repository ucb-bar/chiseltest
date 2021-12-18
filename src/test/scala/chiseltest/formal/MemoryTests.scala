// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import chisel3.SyncReadMem.ReadUnderWrite
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest._
import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import firrtl.annotations.{Annotation, MemoryArrayInitAnnotation, MemoryScalarInitAnnotation, ReferenceTarget}
import firrtl.ir.ReadUnderWrite

// most of the tests are inspired by the MemorySpec in firrtl.backends.experimental.smt.end2end
class MemoryTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  "Registered read-first memory" should "return written data after two cycles" taggedAs FormalTag in {
    verify(new ReadFirstMemoryReturnsDataAfterTwoCycles, Seq(BoundedCheck(2), DefaultBackend))
  }
  "Registered read-first memory" should "not return written data after one cycle" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new ReadFirstMemoryReturnsDataAfterOneCycle, Seq(BoundedCheck(2), DefaultBackend))
    }
    assert(e.failAt == 1)
  }
  "Registered write-first memory" should "return written data after one cycle" taggedAs FormalTag in {
    verify(new ReadFirstMemoryReturnsDataAfterTwoCycles, Seq(BoundedCheck(2), DefaultBackend))
  }
  "read-only memory" should "always return 0" taggedAs FormalTag in {
    verify(new ReadOnlyMemoryAlwaysReturnZero, Seq(BoundedCheck(1), DefaultBackend))
  }
  "read-only memory" should "not always return 1" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new ReadOnlyMemoryAlwaysReturnOneFail, Seq(BoundedCheck(1), DefaultBackend))
    }
    assert(e.failAt == 0)
  }
  "read-only memory" should "always return 1 or 2" taggedAs FormalTag in {
    verify(new ReadOnlyMemoryAlwaysReturnOneOrTwo, Seq(BoundedCheck(1), DefaultBackend))
  }
  "read-only memory" should "not always return 1 or 2 when initialized with a 3" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new ReadOnlyMemoryAlwaysReturnOneOrTwoFail, Seq(BoundedCheck(1), DefaultBackend))
    }
    assert(e.failAt == 0)
  }
  "memory with two write ports" should "not have collisions when enables are mutually exclusive" taggedAs FormalTag in {
    verify(new MutuallyExclusiveWritesShouldNotCollide, Seq(BoundedCheck(4), DefaultBackend))
  }
  "memory with two write ports" should "can have collisions when enables are unconstrained" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new MemoryCollisionModule, Seq(BoundedCheck(2), DefaultBackend))
    }
    assert(e.failAt == 1)
  }
  "a memory with read enable" should "supply valid data one cycle after en=1" taggedAs FormalTag in {
    verify(new ReadEnableMemValidDataAfterEnTrue, Seq(BoundedCheck(4), DefaultBackend))
  }
  "a memory with read enable" should "supply invalid data one cycle after en=0" taggedAs FormalTag in {
    val e = intercept[FailedBoundedCheckException] {
      verify(new ReadEnableMemInvalidDataAfterEnFalse, Seq(BoundedCheck(2), DefaultBackend))
    }
    assert(e.failAt == 1)
  }
  "a memory with read enable" should "just ignore the read enable when not modelling undef values" taggedAs FormalTag in {
    // WARN: it is not recommended to turn of undef modelling and it is not guaranteed that this test won't break
    verify(new ReadEnableMemInvalidDataAfterEnFalse, Seq(BoundedCheck(3), DoNotModelUndef, DefaultBackend))
  }
  "memory with two write ports" should "always have one write win in a collision when not modelling undef values" taggedAs FormalTag in {
    // WARN: it is not recommended to turn of undef modelling and it is not guaranteed that this test won't break
    verify(new MemoryCollisionModule, Seq(BoundedCheck(3), DoNotModelUndef, DefaultBackend))
  }
  "read-only memory" should "not always return 1 even when not modelling undef values" taggedAs FormalTag in {
    // this test does not rely on any undefined values and thus it should always fail
    val e = intercept[FailedBoundedCheckException] {
      verify(new ReadOnlyMemoryAlwaysReturnOneFail, Seq(BoundedCheck(1), DoNotModelUndef, DefaultBackend))
    }
    assert(e.failAt == 0)
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
}

class ReadFirstMemoryReturnsDataAfterTwoCycles extends SyncMemTestModule(ReadUnderWrite.Old) {
  // we assume the we read from the address that we last wrote to
  assume(readAddr === past(writeAddr))
  assert(out === past(in, 2))
}

class ReadFirstMemoryReturnsDataAfterOneCycle extends SyncMemTestModule(ReadUnderWrite.Old) {
  assume(readAddr === writeAddr)
  assert(out === past(in))
}

class WriteFirstMemoryReturnsDataAfterOneCycle extends SyncMemTestModule(ReadUnderWrite.New) {
  assume(readAddr === writeAddr)
  assert(out === past(in))
}

class ReadOnlyMemModule extends Module {
  val readAddr = IO(Input(UInt(2.W)))
  val out = IO(Output(UInt(8.W)))
  val m = Mem(4, UInt(8.W))
  out := m.read(readAddr)
  def annoMem(a: ReferenceTarget => Annotation): Unit = {
    annotate(new ChiselAnnotation {
      override def toFirrtl = a(m.toTarget)
    })
  }
}

class ReadOnlyMemoryAlwaysReturnZero extends ReadOnlyMemModule {
  annoMem(MemoryScalarInitAnnotation(_, 0))
  assert(out === 0.U)
}

class ReadOnlyMemoryAlwaysReturnOneFail extends ReadOnlyMemModule {
  annoMem(MemoryScalarInitAnnotation(_, 0))
  assert(out === 1.U)
}

class ReadOnlyMemoryAlwaysReturnOneOrTwo extends ReadOnlyMemModule {
  annoMem(MemoryArrayInitAnnotation(_, Seq(1, 2, 2, 1)))
  assert(out === 1.U || out === 2.U)
}

class ReadOnlyMemoryAlwaysReturnOneOrTwoFail extends ReadOnlyMemModule {
  // we add a three to the initialization to make the assertion fail
  annoMem(MemoryArrayInitAnnotation(_, Seq(1, 2, 2, 3)))
  assert(out === 1.U || out === 2.U)
}

class MemoryCollisionModule extends Module {
  val addr = IO(Input(UInt(8.W)))
  val data = IO(Input(UInt(32.W)))
  val aEn = IO(Input(Bool()))
  val bEn = IO(Input(Bool()))

  val m = Mem(32, UInt(32.W))
  // we have two write ports that can collide
  when(aEn) { m.write(addr, data) }
  when(bEn) { m.write(addr, data) }

  // the read port is used to verify the written value
  // we use it to check that we always read the last written value
  when(past(aEn || bEn)) {
    assert(m.read(past(addr)) === past(data))
  }
}

class MutuallyExclusiveWritesShouldNotCollide extends MemoryCollisionModule {
  assume(!(aEn && bEn)) // only one port writes at a time
}

class ReadEnableSyncMemModule extends Module {
  val en = RegInit(false.B)
  en := !en

  val m = SyncReadMem(4, UInt(8.W))
  // init with all zeros
  annotate(new ChiselAnnotation {
    override def toFirrtl = MemoryScalarInitAnnotation(m.toTarget, 0)
  })
  // the read port is enabled in even cycles
  val data = m.read(0.U, en)
}

class ReadEnableMemValidDataAfterEnTrue extends ReadEnableSyncMemModule {
  when(past(en)) {
    assert(data === 0.U)
  }
}

class ReadEnableMemInvalidDataAfterEnFalse extends ReadEnableSyncMemModule {
  when(past(!en)) {
    assert(data === 0.U)
  }
}
