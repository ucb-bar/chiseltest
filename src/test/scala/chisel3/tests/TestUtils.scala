package chisel3.tests

import chisel3._
import chisel3.experimental.MultiIOModule

class StaticModule[T <: Data](ioLit: T) extends MultiIOModule {
  val out = IO(Output(chiselTypeOf(ioLit)))
  out := ioLit
}

class PassthroughModule[T <: Data](ioType: T) extends Module {
  val io = IO(new Bundle {
    val in = Input(ioType)
    val out = Output(ioType)
  })
  io.out := io.in
}
