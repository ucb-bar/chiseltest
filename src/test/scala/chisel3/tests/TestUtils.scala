package chisel3.tests

import chisel3._
import chisel3.experimental.MultiIOModule

class StaticModule[T <: Data](ioLit: T) extends MultiIOModule {
  val io = IO(Output(chiselTypeOf(ioLit)))
  io := ioLit
}

class PassthroughModule[T <: Data](ioType: T) extends Module {
  val io = IO(new Bundle {
    val in = Input(ioType)
    val out = Output(ioType)
  })
  io.out := io.in
}
