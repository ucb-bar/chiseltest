// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest.ChiselScalatestTester
import chiseltest.iotesters._
import org.scalatest.freespec.AnyFreeSpec


class HasSyncReadMem extends Module {
  val readAddr  = IO(Input(UInt(16.W)))
  val readData  = IO(Output(UInt(16.W)))
  val writeAddr = IO(Input(UInt(16.W)))
  val writeData = IO(Input(UInt(16.W)))
  val writeEnable = IO(Input(Bool()))

  val mem = SyncReadMem(10, UInt(16.W))

  readData := mem(readAddr)
  when(writeEnable) {
    mem(writeAddr) := writeData
  }
}

class SyncReadMemTest extends AnyFreeSpec with ChiselScalatestTester {
  "peekAt and pokeAt should work with treadle" in {
    test(new HasSyncReadMem).runPeekPoke(new PeekPokeTester(_) {
      poke(dut.writeEnable, 1)
      for (i <- 0 until 8) {
        poke(dut.writeAddr, i)
        poke(dut.writeData, i + 30)
        step(1)
      }
      poke(dut.writeEnable, 0)
      for (i <- 0 until 8) {
        poke(dut.readAddr, i)
        step(1)
        val memValue = peek(dut.readData)
        assert(memValue  == i + 30)
        logger.info(s"$i -> $memValue")
      }
      for (i <- 0 until 8) {
        pokeAt(dut.mem, i + 20, i)
      }
      step(1)
      for (i <- 0 until 8) {
        val memValue = peekAt(dut.mem, i)
        logger.info(s"$i -> $memValue")
        assert(memValue == i + 20)
      }
    })
  }
}
