package chiseltest.formal

import chisel3._
import chisel3.util._
import chiseltest._
import firrtl.AnnotationSeq
import org.scalatest.flatspec.AnyFlatSpec

class MagicPacketTrackerTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "MagicPacketTracker"

  private def defaultOptions: AnnotationSeq = Seq(BoundedCheck(7), DefaultBackend)
  private val DefaultDepth = 3
  private val DefaultDepthPow2 = 4 // some queues only work with power of two

  it should "notice if the counter overflows" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    // counter overflows can happen if the user specified a depth that is too small
    val e = intercept[FailedBoundedCheckException] {
      verify(new PacketTrackerCounterOverflowTest, Seq(BoundedCheck(16), DefaultBackend))
    }
    assert(e.failAt == 3) // without the overflow check, this fails at 4
  }

  it should "verify the BubbleFifo" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new FifoTestWrapper(new BubbleFifo(UInt(16.W), 4)), defaultOptions)
  }

  it should "verify QueueV0" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new QueueFormalTest(new MyQueueV0(32)), defaultOptions)
  }

  it should "verify QueueV1" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new QueueFormalTest(new MyQueueV1(DefaultDepth,32)), defaultOptions)
  }

  it should "find bug in QueueV2" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    val e = intercept[FailedBoundedCheckException] {
      verify(new QueueFormalTest(new MyQueueV2(DefaultDepth, 32)), defaultOptions)
    }
    assert(e.failAt == 3)
  }

  it should "verify QueueV3" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new QueueFormalTest(new MyQueueV3(DefaultDepthPow2,32)), defaultOptions)
  }

  it should "verify QueueV4" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new QueueFormalTest(new MyQueueV4(DefaultDepthPow2,32)), defaultOptions)
  }

  it should "verify QueueV5" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new QueueFormalTest(new MyQueueV5(DefaultDepthPow2,32)), defaultOptions)
  }

  it should "find bug in QueueV6 w/ pipe = false" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    val e = intercept[FailedBoundedCheckException] {
      verify(new QueueFormalTest(new MyQueueV6(DefaultDepth, 32, pipe = false, fixed = false)), defaultOptions)
    }
    assert(e.failAt == 3)
  }

  it should "verify fix for QueueV6 w/ pipe = false" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new QueueFormalTest(new MyQueueV6(DefaultDepth, 32, pipe = false, fixed = true)), defaultOptions)
  }

  it should "find bug in QueueV6 w/ pipe = true" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    val e = intercept[FailedBoundedCheckException] {
      verify(new QueueFormalTest(new MyQueueV6(DefaultDepth, 32, pipe = true, fixed = false)), defaultOptions)
    }
    assert(e.failAt == 2)
  }

  it should "verify fix for QueueV6 w/ pipe = true" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new QueueFormalTest(new MyQueueV6(DefaultDepth, 32, pipe = true, fixed = true)), defaultOptions)
  }

  it should "verify some versions of the Queue from the Chisel standard library" taggedAs FormalTag in {
    assume(DefaultBackend != CVC4EngineAnnotation, "CVC4 is too slow with some of these tests (5h+)")
    verify(new ChiselQueueTest(DefaultDepth, 32, false, false), defaultOptions)
    verify(new ChiselQueueTest(DefaultDepth, 32, false, true), defaultOptions)
    verify(new ChiselQueueTest(DefaultDepth, 32, true, false), defaultOptions)
    verify(new ChiselQueueTest(DefaultDepth, 32, true, true), defaultOptions)
  }
}

private class PacketTrackerCounterOverflowTest extends Module {
  // our queue can take up to 8 elements
  val dut = Module(new Queue(UInt(32.W), 8))
  val io = IO(chiselTypeOf(dut.io)) ; io <> dut.io
  // however for some reason we misconfigure the depth of the tracker
  MagicPacketTracker(enq = dut.io.enq, deq = dut.io.deq, depth = 3)
}


// very similar to the QueueSpec in the chisel3 tests, but a formal test
private class ChiselQueueTest(queueDepth: Int, bitWidth: Int, useSyncReadMem: Boolean, hasFlush: Boolean) extends Module {
  val dut = Module(new Queue(UInt(bitWidth.W), queueDepth, useSyncReadMem = useSyncReadMem, hasFlush = hasFlush))
  val io = IO(chiselTypeOf(dut.io)) ; io <> dut.io
  // we need to tie the flush pin to false, because the MagicPacketTracker cannot (currently)
  // handle flushes, it will complain about a missing packet if a flush happens
  dut.io.flush.foreach(_ := false.B)
  MagicPacketTracker(enq = dut.io.enq, deq = dut.io.deq, depth = queueDepth)
}


// FIFOs from https://github.com/freechipsproject/ip-contributions

private class FifoTestWrapper(fifo: => Fifo[UInt]) extends Module {
  val dut = Module(fifo)
  val io = IO(chiselTypeOf(dut.io)) ; io <> dut.io
  MagicPacketTracker(enq = dut.io.enq, deq = dut.io.deq, depth = dut.depth)
}

/**
  * FIFO IO with enqueue and dequeue ports using the ready/valid interface.
  */
private class FifoIO[T <: Data](private val gen: T) extends Bundle {
  val enq = Flipped(new DecoupledIO(gen))
  val deq = new DecoupledIO(gen)
}

/**
  * Base class for all FIFOs.
  */
private abstract class Fifo[T <: Data](gen: T, val depth: Int) extends Module {
  val io = IO(new FifoIO(gen))

  assert(depth > 0, "Number of buffer elements needs to be larger than 0")
}

/**
  * A simple bubble FIFO.
  * Maximum throughput is one word every two clock cycles.
  */
private class BubbleFifo[T <: Data](gen: T, depth: Int) extends Fifo(gen: T, depth: Int) {

  private class Buffer() extends Module {
    val io = IO(new FifoIO(gen))

    val fullReg = RegInit(false.B)
    val dataReg = Reg(gen)

    when(fullReg) {
      when(io.deq.ready) {
        fullReg := false.B
      }
    }.otherwise {
      when(io.enq.valid) {
        fullReg := true.B
        dataReg := io.enq.bits
      }
    }

    io.enq.ready := !fullReg
    io.deq.valid := fullReg
    io.deq.bits := dataReg
  }

  private val buffers = Array.fill(depth) { Module(new Buffer()) }
  for (i <- 0 until depth - 1) {
    buffers(i + 1).io.enq <> buffers(i).io.deq
  }

  io.enq <> buffers(0).io.enq
  io.deq <> buffers(depth - 1).io.deq
}




///////////////////////////////////////////////////////
// Queues from Scott Beamer's agile hardware lectures
///////////////////////////////////////////////////////

private class QueueFormalTest(makeQueue: => IsQueue) extends Module {
  val dut = Module(makeQueue)
  val io = IO(chiselTypeOf(dut.io)) ; io <> dut.io
  MagicPacketTracker(enq = dut.io.enq, deq = dut.io.deq, depth = dut.numEntries, debugPrint = false)
}

private trait IsQueue extends Module {
  def io: QueueIO
  def numEntries: Int
}

private class QueueIO(bitWidth: Int) extends Bundle {
  val enq = Flipped(Decoupled(UInt(bitWidth.W)))
  val deq = Decoupled(UInt(bitWidth.W))
}

private class MyQueueV0(bitWidth: Int) extends Module with IsQueue {
  override val numEntries = 1
  val io = IO(new QueueIO(bitWidth))
  val entry = Reg(UInt(bitWidth.W))
  val full = RegInit(false.B)
  io.enq.ready := !full || io.deq.fire
  io.deq.valid := full
  io.deq.bits := entry
  when (io.deq.fire) {
    full := false.B
  }
  when (io.enq.fire) {
    entry := io.enq.bits
    full := true.B
  }
}

private class MyQueueV1(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 0)
  // enqueue into index numEntries-1 (last) and dequeue from index 0 (head)
  val entries = Seq.fill(numEntries)(Reg(UInt(bitWidth.W)))
  val fullBits = Seq.fill(numEntries)(RegInit(false.B))
  val shiftDown = io.deq.fire || !fullBits.head
  io.enq.ready := !fullBits.last || shiftDown
  io.deq.valid := fullBits.head
  io.deq.bits := entries.head
  when(shiftDown) { // dequeue / shift
    for (i <- 0 until numEntries - 1) {
      entries(i) := entries(i + 1)
      fullBits(i) := fullBits(i + 1)
    }
    fullBits.last := false.B
  }
  when(io.enq.fire) { // enqueue
    entries.last := io.enq.bits
    fullBits.last := true.B
  }
  //     when (shiftDown || io.enq.fire) {
  //         entries.foldRight(io.enq.bits){(thisEntry, lastEntry) => thisEntry := lastEntry; thisEntry}
  //         fullBits.foldRight(io.enq.fire){(thisEntry, lastEntry) => thisEntry := lastEntry; thisEntry}
  //
}


private class MyQueueV2(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 0)
  // enqueue into lowest empty and dequeue from index 0 (head)
  val entries = Reg(Vec(numEntries, UInt(bitWidth.W)))
  val fullBits = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val emptyBits = fullBits map { !_ }
  io.enq.ready := emptyBits reduce { _ || _ } // any empties?
  io.deq.valid := fullBits.head
  io.deq.bits := entries.head
  when (io.deq.fire) { // dequeue & shift up
    for (i <- 0 until numEntries - 1) {
      entries(i) := entries(i+1)
      fullBits(i) := fullBits(i+1)
    }
    fullBits.last := false.B
  }
  when (io.enq.fire) { // priority enqueue
    val writeIndex = PriorityEncoder(emptyBits)
    entries(writeIndex) := io.enq.bits
    fullBits(writeIndex) := true.B
  }
}

private class MyQueueV3(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 1)
  require(isPow2(numEntries))
  val entries = Reg(Vec(numEntries, UInt(bitWidth.W))) // Mem?
  val enqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val deqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val empty = enqIndex === deqIndex
  val full = (enqIndex +% 1.U) === deqIndex
  io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := entries(deqIndex)
  when (io.deq.fire) {
    deqIndex := deqIndex +% 1.U
  }
  when (io.enq.fire) {
    entries(enqIndex) := io.enq.bits
    enqIndex := enqIndex +% 1.U
  }
}

private class MyQueueV4(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 1)
  require(isPow2(numEntries))
  val entries = Reg(Vec(numEntries, UInt(bitWidth.W)))
  val enqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val deqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val maybeFull = RegInit(false.B)
  val empty = enqIndex === deqIndex && !maybeFull
  val full = enqIndex === deqIndex && maybeFull
  io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := entries(deqIndex)
  when (io.deq.fire) {
    deqIndex := deqIndex +% 1.U
    when (enqIndex =/= deqIndex) {
      maybeFull := false.B
    }
  }
  when (io.enq.fire) {
    entries(enqIndex) := io.enq.bits
    enqIndex := enqIndex +% 1.U
    when ((enqIndex +% 1.U) === deqIndex) {
      maybeFull := true.B
    }
  }
}

private class MyQueueV5(val numEntries: Int, bitWidth: Int) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 1)
  require(isPow2(numEntries))
  val entries = Reg(Vec(numEntries, UInt(bitWidth.W)))
  val enqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val deqIndex = RegInit(0.U(log2Ceil(numEntries).W))
  val maybeFull = RegInit(false.B)
  val empty = enqIndex === deqIndex && !maybeFull
  val full = enqIndex === deqIndex && maybeFull
  io.enq.ready := !full || io.deq.ready  // NOTE: io.enq.ready now attached to io.deq.ready
  io.deq.valid := !empty
  io.deq.bits := entries(deqIndex)
  when (io.deq.fire) {
    deqIndex := deqIndex +% 1.U
    when (enqIndex =/= deqIndex) {
      maybeFull := false.B
    }
  }
  when (io.enq.fire) {
    entries(enqIndex) := io.enq.bits
    enqIndex := enqIndex +% 1.U
    when ((enqIndex +% 1.U) === deqIndex) {
      maybeFull := true.B
    }
  }
}

private class MyQueueV6(val numEntries: Int, bitWidth: Int, pipe: Boolean, fixed: Boolean) extends Module with IsQueue {
  val io = IO(new QueueIO(bitWidth))
  require(numEntries > 1)
  //     require(isPow2(numEntries))    // no longer needed
  val entries = Mem(numEntries, UInt(bitWidth.W))
  val enqIndex = Counter(numEntries)
  val deqIndex = Counter(numEntries)
  val maybeFull = RegInit(false.B)
  val indicesEqual = enqIndex.value === deqIndex.value
  val empty = indicesEqual && !maybeFull
  val full = indicesEqual && maybeFull
  if (pipe)
    io.enq.ready := !full || io.deq.ready
  else
    io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := entries(deqIndex.value)
  if(fixed) {
    when (io.deq.fire =/= io.enq.fire) {
      maybeFull := io.enq.fire
    }
  } else {
    when (indicesEqual && io.deq.fire =/= io.enq.fire) {
      maybeFull := !maybeFull
    }
  }
  when (io.deq.fire) {
    deqIndex.inc()
  }
  when (io.enq.fire) {
    entries(enqIndex.value) := io.enq.bits
    enqIndex.inc()
  }
}

