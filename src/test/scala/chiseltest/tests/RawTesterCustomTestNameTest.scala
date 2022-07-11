// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CustomTestNameModule extends Module{
  val in  = IO(Input (Bool()))
  val out = IO(Output(Bool()))
  out := ~in
}

class RawTesterCustomTestNameTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "RawTester"

  val annos = Seq()

  it should "create a test directory based on the testName" in {
    val testName = "RawTesterCustomTestNameTest"
    RawTester.test(new CustomTestNameModule, annos, testName) { dut =>
      dut.in.poke(0)
      dut.clock.step()
    }

    val testDir = os.pwd / "test_run_dir" / testName
    assert(os.exists(testDir))
  }

}
