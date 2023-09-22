/*
 * Copyright: 2014, Technical University of Denmark, DTU Compute
 * Author: Martin Schoeberl (martin@jopdesign.com)
 * License: Simplified BSD License
 *
 * Play with FIFO buffers.
 *
 * This code is a copy from the chisel-examples repo for easier
 * inclusion in the Chisel book.
 *
 * Modified by Kevin Laeufer.
 *
 */

package chiseltest.coverage.circuits

import chisel3._
import chisel3.util._

object FifoState extends ChiselEnum {
  val Empty, Full = Value
}

/** A single register (=stage) to build the FIFO.
  */
//- start bubble_fifo_register
class FifoRegister(size: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(size.W)))
    val deq = Decoupled(UInt(size.W))
  })

  val stateReg = RegInit(FifoState.Empty)
  val dataReg = RegInit(0.U(size.W))

  when(stateReg === FifoState.Empty) {
    when(io.enq.fire) {
      stateReg := FifoState.Full
      dataReg := io.enq.bits
    }
  }.elsewhen(stateReg === FifoState.Full) {
    when(io.deq.fire) {
      stateReg := FifoState.Empty
      dataReg := 0.U // just to better see empty slots in the waveform
    }
  }.otherwise {
    // There should not be an otherwise state
  }

  io.enq.ready := (stateReg === FifoState.Empty)
  io.deq.valid := (stateReg === FifoState.Full)
  io.deq.bits := dataReg
}
