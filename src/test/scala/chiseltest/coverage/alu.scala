// See README.md for license details.

package chiseltest.coverage

import chisel3._
import chisel3.util._

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */
class ALU extends Module {
  val io = IO(new Bundle {
    val value1        = Input(UInt(16.W))
    val value2        = Input(UInt(16.W))
    val select        = Input(Bool())
    val output        = Output(UInt(16.W))
  })

  val reg = Reg(UInt())

  when(io.select) {
    reg := io.value1 + io.value2
  }.otherwise {
    reg := io.value1 - io.value2
  }

  io.output := reg

}
