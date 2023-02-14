// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chisel3.util._

import scala.collection.immutable.ListMap

class StaticModule[T <: Data](ioLit: T) extends Module {
  val out = IO(Output(chiselTypeOf(ioLit)))
  out := ioLit
}

class InputOnlyModule[T <: Data](ioType: T) extends Module {
  val in = IO(Input(ioType))
}

class PassthroughModule[T <: Data](ioType: T) extends Module {
  val in = IO(Input(ioType))
  val out = IO(Output(ioType))
  out := in
}

class PassthroughQueue[T <: Data](ioType: T) extends Module {
  val in = IO(Flipped(Decoupled(ioType)))
  val out = IO(Decoupled(ioType))
  out <> in
}

class ShifterModule[T <: Data](ioType: T, cycles: Int = 1) extends Module {
  val in = IO(Input(ioType))
  val out = IO(Output(ioType))
  out := ShiftRegister(in, cycles)
}

class QueueModule[T <: Data](ioType: T, entries: Int) extends Module {
  val in = IO(Flipped(Decoupled(ioType)))
  val out = IO(Decoupled(ioType))
  out <> Queue(in, entries)
}

/** borrowed from chiselTests/RecordSpec.scala */
final class CustomBundle(elts: (String, Data)*) extends Record {
  val elements = ListMap(elts.map { case (field, elt) => field -> elt.cloneType }: _*)
  def apply(elt: String): Data = elements(elt)
}
/** taken from https://github.com/schoeberl/chisel-examples/blob/master/src/main/scala/simple/Alu.scala */
class Alu(size: Int) extends Module {
  val io = IO(new Bundle {
    val fn = Input(UInt(2.W))
    val a = Input(UInt(size.W))
    val b = Input(UInt(size.W))
    val result = Output(UInt(size.W))
  })

  val result = Wire(UInt(size.W))
  result := 0.U

  switch(io.fn) {
    is(0.U) { result := io.a + io.b }
    is(1.U) { result := io.a - io.b }
    is(2.U) { result := io.a | io.b }
    is(3.U) { result := io.a & io.b }
  }
  io.result := result
}
