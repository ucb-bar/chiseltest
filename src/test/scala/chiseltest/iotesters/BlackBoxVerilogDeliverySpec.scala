// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import chisel3._
import chisel3.util.{HasBlackBoxInline, HasBlackBoxResource}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BBAddOne extends HasBlackBoxInline {
  val io = IO(new Bundle {
    val in = Input(UInt(16.W))
    val out = Output(UInt(16.W))
  }).suggestName("io")
  setInline("BBAddOne.v",
  """
    |module BBAddOne(
    |    input  [15:0] in,
    |    output reg [15:0] out
    |);
    |  always @* begin
    |  out <= in + 1;
    |  end
    |endmodule
  """.stripMargin)
}

class BBAddTwo extends HasBlackBoxResource {
  val io = IO(new Bundle {
    val in = Input(UInt(16.W))
    val out = Output(UInt(16.W))
  }).suggestName("io")
  addResource("/iotesters/AddTwoAddThree.v")
}

class BBAddThree extends HasBlackBoxResource {
  val io = IO(new Bundle {
    val in = Input(UInt(16.W))
    val out = Output(UInt(16.W))
  }).suggestName("io")
  addResource("/iotesters/AddTwoAddThree.v")
}

class UsesBBAddOne extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(16.W))
    val out1 = Output(UInt(16.W))
    val out2 = Output(UInt(16.W))
    val out3 = Output(UInt(16.W))
  })
  val bbAddOne = Module(new BBAddOne)
  bbAddOne.io.in := io.in
  io.out1 := bbAddOne.io.out

  val bbAddTwo = Module(new BBAddTwo)
  bbAddTwo.io.in := io.in
  io.out2 := bbAddTwo.io.out

  val bbAddThree = Module(new BBAddThree)
  bbAddThree.io.in := io.in
  io.out3 := bbAddThree.io.out
}

class UsesBBAddOneTester(c: UsesBBAddOne) extends PeekPokeTester(c) {
  poke(c.io.in, 1)
  step(1)
  expect(c.io.out1, 2)
  expect(c.io.out2, 3)
  expect(c.io.out3, 4)
  step(1)
}

class BlackBoxVerilogDeliverySpec extends AnyFreeSpec with Matchers {
  "blackbox verilog implementation should end up accessible to verilator" in {
    Driver.execute(Array("--backend-name", "verilator"), () => new UsesBBAddOne) { c =>
      new UsesBBAddOneTester(c)
    } should be (true)
  }

  "blackbox verilog implementation should end up accessible to vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)
    val targetDir = "test_run_dir/blackbox_vcs_test"
    Driver.execute(Array("--backend-name", "vcs", "--target-dir", targetDir), () => new UsesBBAddOne) { c =>
      new UsesBBAddOneTester(c)
    } should be(true)
    new java.io.File(targetDir, firrtl.transforms.BlackBoxSourceHelper.defaultFileListName).exists() should be (true)
  }
}
