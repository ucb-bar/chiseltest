// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.icarus

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.{PlusArgsAnnotation, RequiresIcarus}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

import java.nio.file.Files
import java.nio.file.Paths
import org.scalatest.matchers.should.Matchers

class CustomTestNameModule extends Module{
  val in  = IO(Input (Bool()))
  val out = IO(Output(Bool()))
  
  out := ~in
}

class IcarusRawTesterCustomTestNameTest extends AnyFlatSpec with Matchers with ChiselScalatestTester {
  behavior of "Icarus Backend"

  val annos = Seq(IcarusBackendAnnotation)

  it should "create a test directory based on the testName" taggedAs RequiresIcarus in {
    RawTester.test(new CustomTestNameModule, annos, "IcarusRawTesterCustomTestNameTest") { dut =>
      dut.in.poke(0)
      dut.clock.step()
    }
    
    Files.exists(Paths.get("test_run_dir/IcarusRawTesterCustomTestNameTest")) should equal (true)
  }

}
