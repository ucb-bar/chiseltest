// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.verilator

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class BlackBoxAdderIO extends Bundle {
  val a = Input(UInt(8.W))
  val b = Input(UInt(8.W))
  val q = Output(UInt(8.W))
}

class BlackBoxAdder extends BlackBox with HasBlackBoxInline {
  val io = IO(new BlackBoxAdderIO).suggestName("io")
  setInline("BlackBoxAdder.v",
  """module BlackBoxAdder(a, b, q);
    |input [7:0] a, b;
    |output [7:0] q;
    |assign q = a + b;
    |endmodule
    |""".stripMargin)
}

class BlackBoxAdderWrapper extends Module {
  val io = IO(new BlackBoxAdderIO)
  val m = Module(new BlackBoxAdder)
  m.io <> io
}

class VerilogBlackBoxTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Verilator Backend"

  val annos = Seq(VerilatorBackendAnnotation)

  it should "support Verilog black boxes" in {
    test(new BlackBoxAdderWrapper).withAnnotations(annos) { dut =>
      dut.io.a.poke(1.U)
    }
  }
}
