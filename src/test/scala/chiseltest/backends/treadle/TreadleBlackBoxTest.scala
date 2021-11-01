// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.treadle

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.PlusArgsAnnotation
import treadle.{BlackBoxFactoriesAnnotation, ScalaBlackBox, ScalaBlackBoxFactory}
import treadle.blackboxes.PlusArg
import firrtl.ir.Type
import org.scalatest.flatspec.AnyFlatSpec

// Inspired by plusarg_reader in rocket-chip
class PlusArgReader extends BlackBox {
  val io = IO(new Bundle {
    val out = Output(UInt(32.W))
  })
}

// Mixing both is weird bug legal
class PlusArgReaderTreadleImpl extends ScalaBlackBoxFactory with ScalaBlackBox {
  def name = "PlusArgReader"

  def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] =
    if (blackBoxName == name) Some(this) else None

  private val argName: String = "ARGUMENT"
  private var argument: BigInt = 0xdeadbeefL

  def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    argument
  }

  def outputDependencies(outputName: String): Seq[String] = Nil

  override def setPlusArgs(plusArgs: Seq[PlusArg]): Unit = {
    for (PlusArg(name, value) <- plusArgs) {
      if (name == argName) {
        argument = BigInt(value, 10) // assuming form ARGUMENT=%d
      }
    }
  }
}

class PlusArgReaderWrapper(expected: Int) extends Module {
  val reader = Module(new PlusArgReader)
  assert(reader.io.out === expected.U, s"Expected $expected, got %x.\n", reader.io.out)
}

class TreadleBlackBoxTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Treadle Backend"

  it should "support reading Verilog-style plusargs" in {
    for (plusarg <- List(0, 123, 456)) {
      val annos = Seq(
        BlackBoxFactoriesAnnotation(new PlusArgReaderTreadleImpl :: Nil),
        PlusArgsAnnotation(s"+ARGUMENT=$plusarg" :: Nil)
      )
      test(new PlusArgReaderWrapper(plusarg)).withAnnotations(annos) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()
      }
    }
  }
}
