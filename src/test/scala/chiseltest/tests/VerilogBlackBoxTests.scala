// SPDX-License-Identifier: Apache-2.0
package chiseltest.tests

import chiseltest._
import chisel3._
import chisel3.util._
import chiseltest.simulator.RequiresVerilator
import org.scalatest.flatspec.AnyFlatSpec


class VerilogBlackBoxTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Verilog Blackboxes"

  def doTest(dut: UsesBBAddOne): Unit = {
    dut.io.in.poke(1.U)
    dut.clock.step()
    dut.io.out1.expect(2.U)
    dut.io.out2.expect(3.U)
    dut.io.out3.expect(4.U)
    dut.clock.step()
  }

  it should "be copied over so that they are accessible to Verilator"  taggedAs RequiresVerilator in {
    test(new UsesBBAddOne).withAnnotations(Seq(VerilatorBackendAnnotation))(doTest)
  }
}


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