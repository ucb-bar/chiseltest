package chiseltest.formal.vips

import chisel3._
import chisel3.experimental.IO
import chisel3.util._

/** Tracks random packets for formally verifying FIFOs
  *
  *  This ensures that when some data enters the FIFO, it will always be dequeued after the correct number of elements.
  *  So essentially we are verifying data integrity. Note that this does not imply that the FIFO has no bugs
  *  since e.g., a FIFO that never allows elements to be enqueued would easily pass our assertions.
  *
  *  This module was inspired by the MagicPacketTracker used in the evaluation of the following paper:
  *  Mann, Makai, and Clark Barrett. "Partial order reduction for deep bug finding in synchronous hardware.", CAV'20.
  */
object MagicPacketTracker {
  def apply[D <: Data](enq: ValidIO[D], deq: ValidIO[D], depth: Int, debugPrint: Boolean): Unit = {
    val tracker = Module(new MagicPacketTracker(chiselTypeOf(enq.bits), depth, debugPrint))
    tracker.enq := enq
    tracker.deq := deq
    val startTracking = IO(Input(Bool()))
    tracker.startTracking := startTracking
  }
  def apply[D <: Data](enq: ValidIO[D], deq: ValidIO[D], depth: Int): Unit =
    apply(enq, deq, depth, debugPrint = false)
  def apply[D <: Data](enq: DecoupledIO[D], deq: DecoupledIO[D], depth: Int, debugPrint: Boolean): Unit =
    apply(asValid(enq), asValid(deq), depth, debugPrint)
  def apply[D <: Data](enq: DecoupledIO[D], deq: DecoupledIO[D], depth: Int): Unit =
    apply(asValid(enq), asValid(deq), depth)

  private def asValid[D <: Data](port: DecoupledIO[D]): ValidIO[D] = {
    val validIO = Wire(ValidIO(chiselTypeOf(port.bits)))
    validIO.valid := port.fire
    validIO.bits := port.bits
    validIO
  }
}

class MagicPacketTracker[D <: Data] private (dataTpe: D, fifoDepth: Int, debugPrint: Boolean) extends Module {
  require(fifoDepth > 0, "Fifo depth needs to be positive!")
  val enq = IO(Input(ValidIO(dataTpe)))
  val deq = IO(Input(ValidIO(dataTpe)))

  // count the number of elements in the fifo
  val elementCount = RegInit(0.U(log2Ceil(fifoDepth + 1).W))
  val nextElementCount = Mux(
    enq.fire && !deq.fire,
    elementCount + 1.U,
    Mux(!enq.fire && deq.fire, elementCount - 1.U, elementCount)
  )
  elementCount := nextElementCount

  // track a random "magic" packet through the fifo
  val startTracking = IO(Input(Bool()))
  val isActive = RegInit(false.B)
  val packetValue = Reg(chiselTypeOf(enq.bits))
  val packetCount = Reg(chiselTypeOf(elementCount))

  when(!isActive && enq.fire && startTracking) {
    when(deq.fire && elementCount === 0.U) {
      assert(
        enq.bits.asUInt === deq.bits.asUInt,
        "element should pass through the fifo, but %x != %x",
        enq.bits.asUInt,
        deq.bits.asUInt
      )
    }.otherwise {
      isActive := true.B
      packetValue := enq.bits
      packetCount := nextElementCount
    }
  }

  when(isActive && deq.fire) {
    packetCount := packetCount - 1.U
    when(packetCount === 1.U) {
      assert(
        packetValue.asUInt === deq.bits.asUInt,
        "element should be dequeued in this cycle, but %x != %x",
        packetValue.asUInt,
        deq.bits.asUInt
      )
      isActive := false.B
    }
  }

  // detect element count overflow
  when(elementCount === fifoDepth.U) {
    val shouldIncrement = enq.fire && !deq.fire
    assert(
      !shouldIncrement,
      "MagicPacketTracker: element counter is overflowing %d -> %d\n" +
        "This could indicate either a bug in your FIFO design, or an insufficient depth provided to the MagicPacketTracker constructor.",
      elementCount,
      nextElementCount
    )
  }

  // printout to help debug counter examples
  if (debugPrint) {
    val cycle = RegInit(0.U(8.W)); cycle := cycle + 1.U
    printf(p"step: ${cycle} ----------------------------------\n")
    printf(p"element count: ${elementCount} -> ${nextElementCount}\n")
    when(enq.fire) { printf(p"[${enq.bits}]") }
    when(enq.fire || deq.fire) { printf(" --> ") }
    when(deq.fire) { printf(p"[${deq.bits}]") }
    when(enq.fire || deq.fire) { printf("\n") }
  }
}
