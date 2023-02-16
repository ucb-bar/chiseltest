// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.RequiresVerilator
import firrtl.AnnotationSeq
import org.scalatest.flatspec.AnyFlatSpec

object MyEnum extends ChiselEnum {
  val e0, e1, e3, e4 = Value
}

// Passes an enum with one cycle delay
class EnumPassThrough extends Module {
  val io = IO(new Bundle {
    val in = Input(MyEnum())
    val out = Output(MyEnum())
  })

  io.out := RegNext(io.in)
}

// Passes a Vec of enums with one cycle delay
class EnumVecPassThrough(size: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(size, MyEnum()))
    val out = Output(Vec(size, MyEnum()))
  })

  io.out <> RegNext(io.in)
}

class EnumMem(val size: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(log2Ceil(size).W))
    val read = Output(MyEnum())

    val read_u = Output(UInt(32.W))
    val read_s = Output(SInt(32.W))
  })

  val mem = Mem(size, MyEnum())
  io.read := mem(io.addr)

  val mem_u = Mem(size, UInt(32.W))
  val mem_s = Mem(size, SInt(32.W))

  io.read_u := mem_u(io.addr)
  io.read_s := mem_s(io.addr)
}

class EnumPeekPokeTester(c: EnumPassThrough) extends PeekPokeTester(c) {
  for (e <- MyEnum.all) {
    poke(c.io.in, e)
    step(1)
    expect(c.io.out, e)
  }
}

class IncorrectEnumPeekPokeTester(c: EnumPassThrough) extends PeekPokeTester(c) {
  for (e <- MyEnum.all) {
    poke(c.io.in, e)
    step(1)
    expect(c.io.out, MyEnum.all.head)
  }
}

class EnumVecPeekPokeTester(c: EnumVecPassThrough) extends PeekPokeTester(c) {
  // When poking Vecs directly, enums must be converted to their literal values. This is because there is currently no
  // implicit conversion between IndexedSeq[EnumType] and IndexedSeq[BigInt].

  poke(c.io.in, MyEnum.all.toIndexedSeq.map(_.litValue))
  step(1)
  expect(c.io.out, MyEnum.all.toIndexedSeq.map(_.litValue))
}

class EnumMemPeekPokeTester(c: EnumMem) extends PeekPokeTester(c) {
  for (i <- 0 until c.size) {
    val e = MyEnum.all(i % MyEnum.all.size)
    pokeAt(c.mem, e, i)
    expect(peekAt(c.mem, i) == e.litValue, "Enum memory is not correct")
  }

  for (i <- 0 until c.size) {
    val e = MyEnum.all(i % MyEnum.all.size)
    poke(c.io.addr, i)
    step(1)
    expect(c.io.read, e, "Enum memory is incorrect")
  }
}

class ReadyValidEnumShifter(val delay: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(MyEnum()))
    val out = ValidIO(MyEnum())
  })

  val cnt = RegInit(0.U(log2Ceil(delay+1).W))
  val req_fire = io.in.ready && io.in.valid

  cnt := 0.U
  when (req_fire || (cnt > 0.U && cnt < delay.U)) {
    cnt := cnt + 1.U
  }

  io.out.bits := ShiftRegister(io.in.bits, delay)
  io.out.valid := cnt >= delay.U
  io.in.ready := cnt === 0.U
}

class EnumSpec extends AnyFlatSpec with ChiselScalatestTester {
  def testPeekPoke(options: AnnotationSeq, skipMem: Boolean) = {
    test(new EnumPassThrough).withAnnotations(options).runPeekPoke(new EnumPeekPokeTester(_))

    assertThrows[PeekPokeFailure] {
      test(new EnumPassThrough).withAnnotations(options).runPeekPoke(new IncorrectEnumPeekPokeTester(_))
    }

    test(new EnumVecPassThrough(256)).withAnnotations(options).runPeekPoke(new EnumVecPeekPokeTester(_))

    if(!skipMem) {
      test(new EnumMem(256)).withAnnotations(options).runPeekPoke(new EnumMemPeekPokeTester(_))
    }
  }

  behavior of "Enum PeekPokeTesters"

  it should "work with a treadle backend" in {
    testPeekPoke(Seq(TreadleBackendAnnotation), skipMem = false)
  }

  // pokeAt and peekAt seem to be broken when using Verilator, so we skip the memory tests
  it should "work with a verilator backend" taggedAs RequiresVerilator in {
    testPeekPoke(Seq(VerilatorBackendAnnotation), skipMem = true)
  }
}
