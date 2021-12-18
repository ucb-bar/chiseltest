// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import chisel3._
import chisel3.experimental.ChiselEnum
import chiseltest.ChiselScalatestTester
import chiseltest.iotesters._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class PeekPokeBundleSpec extends AnyFlatSpec with ChiselScalatestTester {
  // Define some data types to be used in the circuit.
  class ABundle extends Bundle {
    val aBool = Bool()
  }

  object MyEnum extends ChiselEnum {
    val e0, e1 = Value
  }

  class MyBundle extends Bundle {
   val aUInt4 = UInt(4.W)
   val aSInt5 = SInt(5.W)
   val aBundle = new ABundle()
   val aBottomBool = Bool()
   val anEnum = MyEnum()
 }

  // A trivial circuit that copies its input to its output.
  class MyCircuit extends Module {
    val io = IO( new Bundle {
      val in = Input(new MyBundle())
      val out = Output(new MyBundle())
    })
    io.out := io.in
  }

  // A tester for the trivial circuit.
  class BundlePeekPokeTesterMapVals(dut: MyCircuit = new MyCircuit) extends PeekPokeTester(dut) {
    // If only we had Bundle literals ...
    // This is extremely fragile. The map definitions must match the order of element definitions in the Bundle
    //  we're about to peek or poke.
    val myBundleMap = mutable.LinkedHashMap[String, BigInt]() ++ List[(String, BigInt)](
      "aUInt4"	-> BigInt(3),
      "aSInt5"	-> BigInt(2),
      "aBundle.aBool"	-> BigInt(1),
      "aBottomBool"	-> BigInt(0),
      "anEnum" -> MyEnum.e1
    )
    poke(dut.io.in, myBundleMap.values.toArray)
    step(1)
    expect(dut.io.out, myBundleMap.values.toArray)
  }

  // A tester for the trivial circuit.
  class BundlePeekPokeTesterMap(dut: MyCircuit = new MyCircuit) extends PeekPokeTester(dut) {
    // If only we had Bundle literals ...
    val myBundleMap = mutable.LinkedHashMap[String, BigInt]() ++ List[(String, BigInt)](
      "aUInt4"	-> BigInt(4),
      "aSInt5"	-> BigInt(5),
      "aBundle.aBool"	-> BigInt(0),
      "aBottomBool"	-> BigInt(1),
      "anEnum" -> MyEnum.e1
    )
    poke(dut.io.in, myBundleMap.toMap)
    step(1)

    expect(dut.io.out, myBundleMap.toMap)
  }

  // The test.
  behavior of "PeekPokeBundleSpec"

  it should "poke and peek bundles with LinkedHashMap values" in {
    test(new MyCircuit).runPeekPoke(new BundlePeekPokeTesterMapVals(_))
  }

  it should "poke and peek bundles with a LinkedHashMap" in {
    test(new MyCircuit).runPeekPoke(new BundlePeekPokeTesterMap(_))
  }
}
