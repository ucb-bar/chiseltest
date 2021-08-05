// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import java.nio.file.{Files, Paths}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import chisel3._
import chiseltest.iotesters._
import chisel3.util.experimental.{loadMemoryFromFile, loadMemoryFromFileInline}
import firrtl.FileUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class UsesMem(memoryDepth: Int, memoryType: Bits) extends Module {
  val io = IO(new Bundle {
    val address = Input(UInt(memoryType.getWidth.W))
    val value   = Output(memoryType)
    val value2  = Output(memoryType)
  })

  val memory = Mem(memoryDepth, memoryType)

  loadMemoryFromFileInline(memory, "mem1.txt")

  io.value := memory(io.address)

  val low = Module(new UsesMemLow(memoryDepth, memoryType))

  low.io.address := io.address
  io.value2 := low.io.value
}

class UsesMemLow(memoryDepth: Int, memoryType: Data) extends Module {
  val io = IO(new Bundle {
    val address = Input(UInt(memoryType.getWidth.W))
    val value   = Output(memoryType)
  })

  val memory = Mem(memoryDepth, memoryType)

  loadMemoryFromFileInline(memory, "mem2.txt")

  io.value := memory(io.address)
}

class LoadMemoryFromFileTester(c: UsesMem) extends PeekPokeTester(c) {
  for(addr <- 0 until 8) {
    poke(c.io.address, addr)
    step(1)
    println(f"peek from $addr ${peek(c.io.value)}%x ${peek(c.io.value2)}%x")
    expect(c.io.value, addr)
    expect(c.io.value2, 7 - addr)
  }
}

class LoadMemoryFromFileSpec extends AnyFreeSpec with Matchers {
  "Users can specify a source file to load memory from" in {

    val targetDirName = "test_run_dir/load_mem_test"
    FileUtils.makeDirectory(targetDirName)

    val path1 = Paths.get(targetDirName + "/mem1.txt")
    val path2 = Paths.get(targetDirName + "/mem2.txt")
    Files.copy(getClass.getResourceAsStream("/iotesters/mem1.txt"), path1, REPLACE_EXISTING)
    Files.copy(getClass.getResourceAsStream("/iotesters/mem2.txt"), path2, REPLACE_EXISTING)

    Driver.execute(
      args = Array("--backend-name", "verilator", "--target-dir", targetDirName, "--top-name", "load_mem_test"),
      dut = () => new UsesMem(memoryDepth = 8, memoryType = UInt(16.W))
    ) { c =>
      new LoadMemoryFromFileTester(c)
    } should be (true)
  }
}
