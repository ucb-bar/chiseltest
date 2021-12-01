// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import chisel3._
import chisel3.util.log2Ceil
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec

/**
  * This whole test is about checking whether memory can be directly poked.
  * To this end the circuit design is a bit of a mess.
  * Trying to make sure memory does not get DCE'd or flagged as uninitialized.
  * Don't look here for anything other than how to poke a memory using the test harness
  * with a scala based simulator
  */
class InnerMemModule extends Module {
  //noinspection TypeAnnotation
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val addr = Input(UInt(log2Ceil(1024).W))
    val data = Input(UInt(32.W))
    val out  = Output(UInt(32.W))
  })
  val nelly = Mem(1024, UInt(32.W))

  when(io.enable) {
    nelly(io.addr) := io.data
  }
  io.out := nelly(io.addr)
}

class OuterMemModule extends Module {
  //noinspection TypeAnnotation
  val io = IO(new Bundle {
    val readAddress = Input(UInt(log2Ceil(1024).W))
    val readData    = Output(UInt(32.W))
    val readData2   = Output(UInt(32.W))
  })
  val billy = Mem(1024, UInt(32.W))
  val inner = Module(new InnerMemModule)
  inner.io.enable := false.B

  inner.io.addr := 1.U
  inner.io.data := 7.U
  io.readData2 := inner.io.out

  io.readData := billy(io.readAddress)
}

class MemPokeTester(m: OuterMemModule) extends PeekPokeTester(m) {
  reset(10)
  0 until 1024 foreach { i =>
    pokeAt(m.billy, i, i)
    pokeAt(m.inner.nelly, value = i + 1, off = i)
  }

  step(10)

  // This uses direct access reading
  0 until 1024 foreach { i =>
    expect(peekAt(m.billy, i) == BigInt(i), s"expected $i at $i, but found ${peekAt(m.billy, i)}")
    expect(peekAt(m.inner.nelly, i) == BigInt(i + 1), s"expected $i at $i, but found ${peekAt(m.billy, i)}")
  }

  // This shows that the ordinary memory systems sees the values written with pokeAt
  0 until 1024 foreach { i =>
    poke(m.io.readAddress, i)
    step(1)

    expect(peek(m.io.readData) == BigInt(i), s"expected $i at $i, but found ${peek(m.io.readData)}")
  }


}

class MemPokeSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Peeking and Poking straight into underlying memory, in interpreter"

  it should "return peek values exactly as poked" in {
    test(new OuterMemModule).runPeekPoke(new MemPokeTester(_))
  }
}
