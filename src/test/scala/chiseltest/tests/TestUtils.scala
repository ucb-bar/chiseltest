package chiseltest.tests

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.internal.requireIsChiselType
import chisel3.util._

import scala.collection.immutable.ListMap

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

class DummyModule extends Module {
  val io = IO(new Bundle {
        val condition = Input(Bool())
        val out = Output(UInt(2.W))
      })
  val counter = RegInit(0.U(2.W))
  when (io.condition) {
    counter := counter + 1.U
  }
  io.out := counter
}

class QueueModule[T <: Data](ioType: T, entries: Int) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(ioType)))
  val out = IO(Decoupled(ioType))
  out <> Queue(in, entries)
}

/** borrowed from chiselTests/RecordSpec.scala */
final class CustomBundle(elts: (String, Data)*) extends Record {
  val elements = ListMap(elts map { case (field, elt) =>
    requireIsChiselType(elt)
    field -> elt
  }: _*)
  def apply(elt: String): Data = elements(elt)
  override def cloneType: this.type = {
    val cloned = elts.map { case (n, d) => n -> DataMirror.internal.chiselTypeClone(d) }
    (new CustomBundle(cloned: _*)).asInstanceOf[this.type]
  }
}
