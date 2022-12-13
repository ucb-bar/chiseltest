// released under BSD 3-Clause License
// based on the fifo and cdc library from: https://github.com/amaranth-lang/amaranth/blob/main/amaranth/lib/fifo.py
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal.multiclock
import chisel3._
import chisel3.util._
import firrtl.ir.ReadUnderWrite

class AsyncFifoWriteIO[D <: Data](dataTpe: D, counterWidth: Int) extends Bundle {
  val enq = Flipped(DecoupledIO(dataTpe))
  val count = Output(UInt(counterWidth.W))
}

class AsyncFifoReadIO[D <: Data](dataTpe: D, counterWidth: Int) extends Bundle {
  val clock = Input(Clock())
  val deq = DecoupledIO(dataTpe)
  val count = Output(UInt(counterWidth.W))
}

class AsyncFifo[D <: Data](val dataTpe: D, val depth: Int) extends Module {
  require(depth > 0)
  val depthBits = log2Ceil(depth)
  require(depth == 1 << depthBits, s"depth needs to be a power of two, not $depth")
  val counterWidth = depthBits + 1

  // I/O
  val write = IO(new AsyncFifoWriteIO(dataTpe, depthBits + 1))
  val read = IO(new AsyncFifoReadIO(dataTpe, depthBits + 1))

  // The design of this queue is the "style #2" from Clifford E. Cummings' paper "Simulation
  // and Synthesis Techniques for Asynchronous FIFO Design":
  // http://www.sunburst-design.com/papers/CummingsSNUG2002SJ_FIFO1.pdf

  val mem = SyncReadMem(depth, dataTpe, ReadUnderWrite.Old)

  // keep binary and gray counts registered in the source domain
  val (writeCountBin, writeCountGray) = withClock(clock) {
    val writeCount = RegInit(0.U(counterWidth.W))
    val writeNext = writeCount + write.enq.fire
    writeCount := writeNext

    when(write.enq.fire) {
      mem.write(writeCount, write.enq.bits, clock)
    }

    (writeCount, RegNext(BinaryToGray(writeNext), 0.U))
  }

  val (readCountBin, readCountGray) = withClock(read.clock) {
    val readCount = Reg(UInt(counterWidth.W)) // intentionally without reset!
    val readNext = readCount + read.deq.fire
    readCount := readNext

    read.deq.bits := mem.read(readNext, read.clock)

    (readCount, RegNext(BinaryToGray(readNext))) // intentionally without reset!
  }

  // receive gray count after 2FF synchronization
  withClock(clock) {
    val synchronizedReadCountGray = RegNext(RegNext(readCountGray, 0.U), 0.U)
    val synchronizedReadCountBin = RegNext(GrayToBinary(synchronizedReadCountGray, counterWidth), 0.U)

    // calculate full as described in section 5.2
    val full = writeCountGray.head(1) =/= synchronizedReadCountGray.head(1) &&
      writeCountGray.head(2)(0) =/= synchronizedReadCountGray.head(2)(0) &&
      writeCountGray.tail(2) === synchronizedReadCountGray.tail(2)
    write.enq.ready := !full
    write.count := RegNext(writeCountBin - synchronizedReadCountBin, 0.U)
  }

  withClock(read.clock) {
    val synchronizedWriteCountGray = RegNext(RegNext(writeCountGray))
    val synchronizedWriteCountBin = RegNext(GrayToBinary(synchronizedWriteCountGray, counterWidth))

    // calculate empty as described in section 5.1
    val empty = readCountGray === synchronizedWriteCountGray
    read.deq.valid := !empty
    read.count := synchronizedWriteCountBin - readCountBin

  }
  // The write domain's reset signal is used to asynchronously reset the read domain's
  // counters and force the FIFO to be empty when the write domain's reset is asserted.

  val readReset = withClock(read.clock) {
    AsyncFFSynchronizer(reset)
  }
  when(readReset) {
    // overwrite in case of reset!
    read.deq.valid := false.B
    readCountBin := 0.U
    readCountGray := 0.U
  }
}

// see https://github.com/amaranth-lang/amaranth/blob/7d611b8fc1d9e58853ff268ec38ff8f4131a9774/amaranth/lib/cdc.py
object AsyncFFSynchronizer {
  def apply(in: Reset): Bool = apply(in.asAsyncReset)
  def apply(in: AsyncReset): Bool = {
    withReset(in) {
      RegNext(RegNext(false.B, true.B), true.B)
    }
  }
}

object BinaryToGray {
  def apply(in: UInt): UInt = in ^ (in >> 1.U)
}

object GrayToBinary {
  def apply(in: UInt, width: Int): UInt = apply(in(width - 1, 0))
  def apply(in: UInt): UInt = if (in.getWidth < 2) { in }
  else {
    var prev = in.head(1)
    val bits = in.getWidth - 2 to 0 by -1
    prev ## Cat(bits.map { ii => prev = prev ^ in(ii); prev })
  }
}
