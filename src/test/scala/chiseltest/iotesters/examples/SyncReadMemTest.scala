// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest.iotesters._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


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

class SyncReadMemTest extends AnyFreeSpec with Matchers {
  "peekAt and pokeAt should work with treadle" in {
    Driver.execute(
      Array(
        "--backend-name",
        "treadle",
        "--target-dir",
        "test_run_dir/sync_read_mem_test_treadle",
        "--top-name",
        "SyncReadMem"
      ),
      () => new HasSyncReadMem
    ) { c =>
      new PeekPokeTester(c) {
        poke(c.writeEnable, 1)
        for (i <- 0 until 8) {
          poke(c.writeAddr, i)
          poke(c.writeData, i + 30)
          step(1)
        }
        poke(c.writeEnable, 0)
        for (i <- 0 until 8) {
          poke(c.readAddr, i)
          step(1)
          val memValue = peek(c.readData)
          memValue should be(i + 30)
          logger.info(s"$i -> $memValue")
        }
        for (i <- 0 until 8) {
          pokeAt(c.mem, i + 20, i)
        }
        step(1)
        for (i <- 0 until 8) {
          val memValue = peekAt(c.mem, i)
          logger.info(s"$i -> $memValue")
          memValue should be(i + 20)

        }
      }
    } should be(true)
  }
}
