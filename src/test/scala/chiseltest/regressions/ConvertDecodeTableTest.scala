// SPDX-License-Identifier: Apache-2.0

package chiseltest.regressions

import chisel3._
import chisel3.util.experimental.decode.{decoder, TruthTable}
import chisel3.util.{BitPat, Cat, Fill}
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// https://github.com/sequencer/arithmetic/blob/dd9bd585a8d444399eb5a31d088567e0ef56f43a/arithmetic/src/multiplier/Booth.scala
class Booth(width: Int)(radixLog2: Int, signed: Boolean = true) extends Module {
  val input = IO(Input(UInt(width.W)))
  val output = IO(
    Output(
      Vec(
        width / radixLog2 + 1, // = ceil(width / radixLog2)
        SInt((radixLog2 + 1).W)
      )
    )
  )
  def extend(x: Bits, len: Int, signed: Boolean = true): Bits = {
    if (x.getWidth >= len)
      x
    else {
      val fillBit = if (signed) x.head(1) else 0.B
      Fill(len - x.getWidth, fillBit) ## x.asUInt
    }
  }

  /** Because .asUInt() do not set .litOption properly */
  def sIntToBitPat(x: Int, w: Int): BitPat = {
    if (x >= 0)
      BitPat(x.U(w.W))
    else
      BitPat((x + (1 << w)).U(w.W))
  }

  val paddingLeftWidth = width + radixLog2 - width % radixLog2
  val paddedInput = Cat(extend(input, paddingLeftWidth, signed), 0.U(1.W))

  val boothEncodingCoeff = Seq.tabulate(radixLog2 + 1) {
    case i if i == radixLog2 => -(1 << (radixLog2 - 1))
    case i if i == 0         => 1
    case i                   => 1 << (i - 1)
  }

  val boothEncodingTable = TruthTable(
    Seq
      .tabulate(1 << (radixLog2 + 1)) { i =>
        Seq
          .tabulate(radixLog2 + 1)((bit: Int) => if (BigInt(i).testBit(bit)) 1 else 0)
          .zip(boothEncodingCoeff)
          .map { case (a, b) =>
            a * b
          }
          .sum
      }
      .zipWithIndex
      .map { case (o, i) =>
        val w = radixLog2 + 1
        (sIntToBitPat(i, w), sIntToBitPat(o, w))
      },
    BitPat.dontCare(radixLog2 + 1)
  )

  output := Seq
    .tabulate(output.size) { i =>
      decoder(paddedInput(radixLog2 * (i + 1), radixLog2 * i), boothEncodingTable)
    }
    .map(_.asSInt)
}

class ConvertDecodeTableAnnotation extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior.of("Convert DecodeTableAnnotation Regression")

  it should "work" in {
    test(new Booth(16)(8)) { dut =>
      dut.input.poke(7.U)
      dut.output(0).expect(7.S)
      dut.output(1).expect(0.S)
      dut.output(2).expect(0.S)
    }
  }
}
