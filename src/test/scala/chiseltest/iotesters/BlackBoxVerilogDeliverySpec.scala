// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import chisel3._
import chisel3.util.{HasBlackBoxInline, HasBlackBoxResource}
import chiseltest._
import chiseltest.simulator.{RequiresVcs, RequiresVerilator}
import firrtl.options.TargetDirAnnotation
import org.scalatest.freespec.AnyFreeSpec

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
    |    out = in + 1;
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

class BlackBoxVerilogDeliverySpec extends AnyFreeSpec with ChiselScalatestTester {
  "blackbox verilog implementation should end up accessible to verilator" taggedAs RequiresVerilator in {
    test(new UsesBBAddOne).withAnnotations(Seq(VerilatorBackendAnnotation)).runPeekPoke(new UsesBBAddOneTester(_))
  }

  "blackbox verilog implementation should end up accessible to vcs" taggedAs RequiresVcs in {
    val targetDir = "test_run_dir/blackbox_vcs_test"
    val options = Seq(VcsBackendAnnotation, TargetDirAnnotation(targetDir))

    test(new UsesBBAddOne).withAnnotations(options).runPeekPoke(new UsesBBAddOneTester(_))
    assert(os.exists(os.pwd / os.RelPath(targetDir) / firrtl.transforms.BlackBoxSourceHelper.defaultFileListName))
  }
}
