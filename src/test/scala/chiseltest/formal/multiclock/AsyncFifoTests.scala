// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.multiclock

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.observe
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec

class AsyncFifoTests extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  behavior of "AsyncFifo"

  // TODO: find out why wire persist when running stuttering clock transform
  it should "pass a formal integrity test" taggedAs FormalTag ignore {
    verify(new FifoTestWrapper(new AsyncFifo(UInt(8.W), 4)), Seq(BoundedCheck(4), DefaultBackend, EnableMultiClock))
  }
}


// this only tests the fifo for the case where we have the same clock
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
      MagicPacketTracker(enq, deq, dut.depth)
    }
  }

}
