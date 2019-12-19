package chiseltest.tests

import chisel3._
import chisel3.util._

class StaticModule[T <: Data](ioLit: T) extends MultiIOModule {
  val out = IO(Output(chiselTypeOf(ioLit)))
  out := ioLit
}

class InputOnlyModule[T <: Data](ioType: T) extends MultiIOModule {
  val in = IO(Input(ioType))
}

class PassthroughModule[T <: Data](ioType: T) extends MultiIOModule {
  val in = IO(Input(ioType))
  val out = IO(Output(ioType))
  out := in
}

class PassthroughQueue[T <: Data](ioType: T) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(ioType)))
  val out = IO(Decoupled(ioType))
  out <> in
}

class ShifterModule[T <: Data](ioType: T, cycles: Int = 1) extends MultiIOModule {
  val in = IO(Input(ioType))
  val out = IO(Output(ioType))
  out := ShiftRegister(in, cycles)
}

class QueueModule[T <: Data](ioType: T, entries: Int) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(ioType)))
  val out = IO(Decoupled(ioType))
  out <> Queue(in, entries)
}
