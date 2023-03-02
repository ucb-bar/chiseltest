// SPDX-License-Identifier: Apache-2.0

package treadle2.vcd

import java.io.{ByteArrayOutputStream, File, PrintStream}

import firrtl.options.Viewer.view
import firrtl.options.{StageOptions, TargetDirAnnotation}
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}
import firrtl.util.BackendCompilationUtilities
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import treadle2._

import scala.util.Random

// scalastyle:off magic.number
class VCDSpec extends AnyFlatSpec with Matchers {
  private def getVcd = {
    VCD("test_circuit")
  }

  behavior.of("vcd")

  it should "be able to generate unique ids " in {
    val vcd = getVcd

    val ids = new collection.mutable.HashSet[String]
    for (i <- 0 to 1000) {
      val id = vcd.getIdString(i)

      ids.contains(id) should be(false)
      ids += id

      id.forall { c =>
        c.toInt >= 33 && c.toInt <= 126
      } should be(true)
    }
  }

  it should "only remember the last change added" in {
    val vcd = getVcd
    val rand = new Random()
    var lastValue = 0

    vcd.setTime(100)

    for (_ <- 0 to 10) {
      for (i <- 0 to 10) {
        lastValue = rand.nextInt()
        vcd.wireChanged("testWire1", lastValue)
      }

      vcd.valuesAtTime(vcd.timeStamp).size should be(1)
      vcd.valuesAtTime(vcd.timeStamp).head.value should be(lastValue)

      vcd.incrementTime(10)
    }
  }

  it should "allow add wires" in {
    val vcd = getVcd

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    vcd.wires.contains("bob") should be(true)
    vcd.wires.contains("carol") should be(true)
    vcd.wires.contains("ted") should be(true)

    vcd.wires.contains("alice") should be(false)
  }

  it should "ignore calls to wire changed when value has not changed" in {
    val vcd = getVcd

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    // time starts at -1 to support initialized values
    vcd.incrementTime()
    for (i <- 0 to 10) {
      vcd.wireChanged("bob", i)
      vcd.wireChanged("carol", i / 2)
      vcd.wireChanged("ted", i / 4)
      vcd.incrementTime()
    }

    vcd.valuesAtTime(1).size should be(3)
    vcd.valuesAtTime(2).size should be(1)
    vcd.valuesAtTime(3).size should be(2)
    vcd.valuesAtTime(4).size should be(1)
    vcd.valuesAtTime(5).size should be(3)
    vcd.valuesAtTime(6).size should be(1)
  }

  it should "be able to serialize negative and positive values" in {
    val wire = Wire("testwire", "t", width = 4)
    val s = new StringBuilder
    for (i <- -8 to 7) {
      val change = Change(wire, i)
      val string = s"$i => ${change.serialize}"
      s ++= string + "\n"
    }
    s.toString().contains("-8 => b1000") should be(true)
    s.toString().contains("-1 => b1111") should be(true)
    s.toString().contains("0 => b0000") should be(true)
    s.toString().contains("1 => b0001") should be(true)
    s.toString().contains("7 => b0111") should be(true)
  }

  it should "serialize 1 bit numbers correctly" in {
    val c0 = Change(Wire("test1", "%", 1), 0)
    c0.serialize should be("0%")

    val c1 = Change(Wire("test1", "%", 1), 1)
    c1.serialize should be("1%")

    val c2 = Change(Wire("test1", "%", 1), -1)
    c2.serialize should be("1%")
  }


  behavior.of("vcd log containing negative numbers")

  it should "work correctly and be runnable from vcd output file" in {

    val input =
      """
        |circuit Adder :
        |  module Adder :
        |    input clock : Clock
        |    input a : SInt<8>
        |    input b : SInt<8>
        |    output c : SInt<10>
        |
        |    c <= add(a, b)
      """.stripMargin

    val options = Seq(
      WriteVcdAnnotation,
      TargetDirAnnotation("test_run_dir/vcd_reader_1"),
      OutputFileAnnotation("vcd_reader_1")
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { engine =>
      engine.poke("a", -1)
      engine.peek("a") should be(BigInt(-1))
      engine.poke("b", -7)
      engine.peek("b") should be(BigInt(-7))

      engine.step()
      engine.peek("c") should be(BigInt(-8))

      engine.poke("a", 255)
      engine.peek("a") should be(BigInt(-1))
      engine.poke("b", 249)
      engine.peek("b") should be(BigInt(-7))

      engine.step()
      engine.peek("c") should be(BigInt(-8))
    }

  }

  behavior.of("Using VCD output as a golden model test of a circuit")

  it should "be able to create a VCD then replay the VCD testing inputs" in {
    val stream = getClass.getResourceAsStream("/treadle/VcdAdder.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val options = Seq(
      WriteVcdAnnotation,
      TargetDirAnnotation("test_run_dir/vcd_reader_2"),
      OutputFileAnnotation("vcd_reader_2")
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { engine =>
      engine.step()
      engine.poke("io_a", 3)
      engine.poke("io_b", 5)
      engine.peek("io_a") should be(BigInt(3))
      engine.peek("io_b") should be(BigInt(5))

      engine.step()
      engine.peek("io_c") should be(BigInt(8))
    }
  }
}
