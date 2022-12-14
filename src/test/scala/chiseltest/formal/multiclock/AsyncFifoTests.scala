// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.multiclock

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.observe
import chiseltest.formal._
import logger.{LogLevel, LogLevelAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

class AsyncFifoTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "AsyncFifo"

  it should "pass a formal integrity test" taggedAs FormalTag in {
    verify(new FifoTestWrapper(new AsyncFifo(UInt(8.W), 4)), Seq(
      BoundedCheck(4), DefaultBackend, EnableMultiClock,
      // TODO: we currently do not model undef since the DefRandToRegisterPass doesn't deal well with multi-clock
      DoNotModelUndef,
      //      LogLevelAnnotation(LogLevel.Info)
    ))
  }

  "the DCAsyncFifo" should "pass a formal integrity test" taggedAs FormalTag in {
    verify(new DCAsyncFifoTestWrapper(new DCAsyncFifo(UInt(8.W), 4)), Seq(
      BoundedCheck(8), DefaultBackend, EnableMultiClock,
      // TODO: we currently do not model undef since the DefRandToRegisterPass doesn't deal well with multi-clock
      DoNotModelUndef,
      //      LogLevelAnnotation(LogLevel.Info)
    ))
  }
}


class FifoTestWrapper[D <: Data](fifo: => AsyncFifo[D]) extends Module {
  val dut = Module(fifo)
  val write = IO(chiselTypeOf(dut.write))
  val read = IO(chiselTypeOf(dut.read))
  val readClock = IO(Input(Clock()))
  write <> dut.write
  read <> dut.read
  dut.read.clock := readClock

  // define how to sample incoming and outcoming packets
  val enqReset = reset.asBool
  val enq = Wire(ValidIO(chiselTypeOf(dut.write.enq.bits)))
  enq.bits := dut.write.enq.bits
  enq.valid := dut.write.enq.fire && clockIsEnabled(clock) && !enqReset
  val deqReset = observe(dut.readReset)
  val deq = Wire(ValidIO(chiselTypeOf(dut.read.deq.bits)))
  deq.bits := dut.read.deq.bits
  deq.valid := dut.read.deq.fire && clockIsEnabled(dut.read.clock) && !deqReset

  // instantiate the normal tracker with global clock and read or write reset
  withGlobalClock {
    withReset(deqReset || enqReset) {
      MagicPacketTracker(enq, deq, dut.depth, debugPrint = true)
    }
  }
  // the reset is asserted in the first cycle
  duringInit {
    assume(enqReset)
  }
}


class DCAsyncFifoTestWrapper[D <: Data](fifo: => DCAsyncFifo[D]) extends Module {
  val dut = Module(fifo)
  // DUT I/O
  val enqClock = clock
  val enqReset = reset.asBool
  val deqClock = IO(Input(Clock()))
  val deqReset = IO(Input(Bool()))
  val enq = IO(Flipped(new DecoupledIO(chiselTypeOf(dut.io.enq.bits))))
  val deq = IO(new DecoupledIO(chiselTypeOf(dut.io.deq.bits)))
  dut.io.enqClock := enqClock ; dut.io.enqReset := enqReset
  dut.io.deqClock := deqClock ; dut.io.deqReset := deqReset
  dut.io.enq <> enq ; dut.io.deq <> deq

  // define how to sample incoming and outcoming packets
  val enqObserve = Wire(ValidIO(chiselTypeOf(dut.io.enq.bits)))
  enqObserve.bits := dut.io.enq.bits
  enqObserve.valid := dut.io.enq.fire && clockIsEnabled(dut.io.enqClock) && !enqReset
  val deqObserve = Wire(ValidIO(chiselTypeOf(dut.io.deq.bits)))
  deqObserve.bits := dut.io.deq.bits
  deqObserve.valid := dut.io.deq.fire && clockIsEnabled(dut.io.deqClock) && !deqReset

  // instantiate the normal tracker with global clock and read or write reset
  withGlobalClock {
    withReset(deqReset || enqReset) {
      MagicPacketTracker(enqObserve, deqObserve, dut.depth, debugPrint = true)
    }
  }
  // both resets are asserted in the first cycle and both clocks tick to make reset actually have an effect
  duringInit(cycles = 3) { // need to reset for at least this many cycles in order to clear the synchronizer regs
    assume(deqReset && clockIsEnabled(deqClock))
    assume(enqReset && clockIsEnabled(enqClock))
  }
}