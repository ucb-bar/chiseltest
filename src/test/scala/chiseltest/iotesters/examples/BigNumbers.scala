// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chiseltest._
import chiseltest.iotesters._
import org.scalatest.freespec.AnyFreeSpec

class PassOn extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(64.W))
    val out = Output(UInt(64.W))
  })
  io.out := io.in
}

class BigNumbersTester(c: PassOn) extends PeekPokeTester(c) {
  poke (c.io.in, 0x0000000070000000L)
  expect(c.io.out, 0x0000000070000000L)

  // Test 2:(Test Fails)
  poke (c.io.in, 0x0000000770000000L)
  expect (c.io.out, 0x0000000770000000L)

  // Output only takes value of last 32 bits (70000000) and test fails.

  // Test 3:(FIRRTL generates an error)
  poke (c.io.in, 0x0000000080000000L)
  expect (c.io.out, 0x0000000080000000L)
}

class BigNumbersSpec extends AnyFreeSpec with ChiselScalatestTester {
  "big numbers should work with treadle backend" in {
    test(new PassOn).runPeekPoke(new BigNumbersTester(_))
  }
}
