// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.verilator

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.{PlusArgsAnnotation, RequiresVerilator}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

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

// Inspired by plusarg_reader in rocket-chip
class PlusArgReader extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val out = Output(UInt(32.W))
  })
  setInline("PlusArgReader.v",
  """module PlusArgReader(
    |  output [31:0] out
    |);
    |  reg [32:0] argument;
    |  assign out = argument;
    |  initial begin
    |    if (!$value$plusargs("ARGUMENT=%d", argument)) begin
    |      argument = 32'hdeadbeef;
    |    end
    |  end
    |endmodule
    |""".stripMargin)
}

class PlusArgReaderWrapper(expected: Int) extends Module {
  val reader = Module(new PlusArgReader)
  assert(reader.io.out === expected.U, s"Expected $expected, got %x.\n", reader.io.out)
}

class VerilogBlackBoxTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Verilator Backend"

  val annos = Seq(VerilatorBackendAnnotation)

  it should "support Verilog black boxes" taggedAs RequiresVerilator in {
    val rand = new Random()
    val mask = (BigInt(1) << 8) - 1
    test(new BlackBoxAdderWrapper).withAnnotations(annos) { dut =>
      (0 until 1000).foreach { ii =>
        val a = BigInt(8, rand)
        val b = BigInt(8, rand)
        val q = (a + b) & mask
        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        dut.io.q.expect(q.U)
      }
    }
  }

  it should "support reading Verilog plusargs" taggedAs RequiresVerilator in {
    for (plusarg <- List(0, 123, 456)) {
      val allAnnos = annos :+ PlusArgsAnnotation(s"+ARGUMENT=$plusarg" :: Nil)
      test(new PlusArgReaderWrapper(plusarg)).withAnnotations(allAnnos) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()
      }
    }
  }
}
