// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec


/**
  * Passes a Vec of elements with one cycle delay
  * This is part of an example of using poke on a Vector input
  * @param numberOfElements  number of elements to be sorted
  * @param elementGenerator  generator for kind of elements to be sorted
  */
class VecPassThrough(val numberOfElements: Int, elementGenerator: => UInt) extends Module {
  val io = IO(new Bundle {
    val inVector = Input(Vec(numberOfElements, elementGenerator))
    val outVector = Output(Vec(numberOfElements, elementGenerator))
    val outVectorAsUInt = Output(UInt(inVector.getWidth.W))
  })

  val regVector = Reg(Vec(numberOfElements, elementGenerator))

  regVector <> io.inVector
  io.outVector <> regVector

  io.outVectorAsUInt := io.inVector.asUInt()
}

/**
  * Passes a Bundle of elements with one cycle delay
  * This is part of an example of using poke on a Bundle input
  */
class BundlePassThrough extends Module {
  val io = IO(new Bundle {
    val inBundle = Input(new PassThroughBundle)
    val outBundle = Output(new PassThroughBundle)
    val outBundleAsUInt = Output(UInt(9.W))
  })

  val regBundle = Reg(new PassThroughBundle)

  regBundle <> io.inBundle
  io.outBundle <> regBundle

  io.outBundleAsUInt := io.inBundle.asUInt()
}

class PassThroughBundle extends Bundle {
  val u1 = UInt(3.W)
  val u2 = UInt(9.W)
  val u3 = UInt(27.W)
}

/**
  * Demonstrate that calling poke with a IndexedSeq of BigInts
  * will poke the individual elements of a Bundle, first element of Seq goes to first element in bundle
  *
  * @param c is the device under test
  */
class BundlePeekPokeTester(c: BundlePassThrough) extends PeekPokeTester(c) {
  private val numberOfElements = 3

  private val vectorInputs = Array.tabulate(numberOfElements) { x => BigInt(x + 1) }
  println(s"scala array to poke into vector    ${vectorInputs.mkString(",")}")

  poke(c.io.inBundle, vectorInputs)

  private val allAtOncePeekedInputs = peek(c.io.inBundle)
  println(s"input peeked all at once           ${allAtOncePeekedInputs.mkString(",")}")

  private val individuallyPeekedInputs = Array(peek(c.io.inBundle.u1), peek(c.io.inBundle.u2), peek(c.io.inBundle.u3))
  println(s"input peeked individually          ${individuallyPeekedInputs.mkString(",")}")

  step(1)

  private val allAtOncePeekedOutputs = peek(c.io.outBundle)
  println(s"output peeked all at once          ${allAtOncePeekedOutputs.mkString(",")}")

  private val individuallyPeekedOutputs = Array(peek(c.io.inBundle.u1), peek(c.io.inBundle.u2), peek(c.io.inBundle.u3))
  println(s"output peeked individually         ${individuallyPeekedOutputs.mkString(",")}")
}


/**
  * Demonstrate that calling poke with a IndexedSeq of BigInts
  * will poke the individual elements of a Vec, perversely the first element of the seq will go into the last
  * element of the Vec and so on.  Equally perversely the peek on the Vec will place the first element of the
  * Vec into the first element of the resulting Seq
  *
  * @param c is the device under test
  */
class VecPeekPokeTester(c: VecPassThrough) extends PeekPokeTester(c) {
  private val numberOfElements = c.numberOfElements

  private val vectorInputs = Array.tabulate(numberOfElements) { x => BigInt(x) }
  println(s"scala array to poke into vector    ${vectorInputs.mkString(",")}")

  poke(c.io.inVector, vectorInputs)

  private val allAtOncePeekedInputs = peek(c.io.inVector)
  println(s"input peeked all at once           ${allAtOncePeekedInputs.mkString(",")}")

  private val individuallyPeekedInputs = vectorInputs.indices.map { index => peek(c.io.inVector(index)) }
  println(s"input peeked individually          ${individuallyPeekedInputs.mkString(",")}")

  // @NOTE: The poked array and returned peeked array are opposite
  assert(vectorInputs.zip(allAtOncePeekedInputs.reverse).forall { case (a, b) => a == b })

  vectorInputs.reverse.zipWithIndex.foreach { case (value, index) =>
    expect(c.io.inVector(index), value)
  }

  step(1)

  private val allAtOncePeekedOutputs = vectorInputs.indices.map { index => peek(c.io.outVector(index)) }
  println(s"output peeked all at once          ${allAtOncePeekedOutputs.mkString(",")}")

  private val individuallyPeekedOutputs = vectorInputs.indices.map { index => peek(c.io.inVector(index)) }
  println(s"output peeked individually         ${individuallyPeekedOutputs.mkString(",")}")
}

/**
  * Passes a Vec of elements with one cycle delay
  * This is part of an example of using poke on a Vector input
  */
class AggregatePassThrough(aggregateGenerator: => Aggregate) extends Module {
  val io = IO(new Bundle {
    val inputAggregate = Input(aggregateGenerator)
    val outputAggregate = Output(aggregateGenerator)
    val aggregateAsUInt = Output(UInt(aggregateGenerator.getWidth.W))
    val outputFromUInt = Output(aggregateGenerator)
  })

  val aggregateRegister = Reg(aggregateGenerator)

  aggregateRegister <> io.inputAggregate
  io.outputAggregate <> aggregateRegister

  io.aggregateAsUInt := aggregateRegister.asUInt()
  io.outputFromUInt := aggregateRegister.asTypeOf(aggregateGenerator)
}

/**
  * Demonstrate that calling poke with a IndexedSeq of BigInts
  * will poke the individual elements of a Vec
  *
  * @param c is the device under test
  */
class AggregateOrderingTester(c: AggregatePassThrough, numberOfElements: Int) extends PeekPokeTester(c) {

  private val startValue = if(numberOfElements < 7) "a" else "0"
  private val inputArray = Array.tabulate(numberOfElements) { x => BigInt(startValue, 16) + x }

  poke(c.io.inputAggregate, inputArray)
  private val peekedInput = peek(c.io.inputAggregate)

  step(1)

  private val peekedOutput = peek(c.io.outputAggregate)
  private val peekedUInt   = peek(c.io.aggregateAsUInt)
  private val peekedOutputFromUInt = peek(c.io.outputFromUInt)

  println(s"input array that will be poked   " + inputArray.map { bigInt => bigInt.toString(16) }.mkString(""))
  println(s"peek of the input                " + peekedInput.map { bigInt => bigInt.toString(16) }.mkString(""))
  println(s"peek of the output               " + peekedOutput.map { bigInt => bigInt.toString(16) }.mkString(""))
  println(s"peek of the input as UInt        " + peekedUInt.toString(16))
  println(s"peek of input fromBits of asUInt " + peekedOutputFromUInt.map { bigInt => bigInt.toString(16) }.mkString(""))
}

class Bundle5 extends Bundle {
  val u0 = UInt(4.W)
  val u1 = UInt(4.W)
  val u2 = UInt(4.W)
  val u3 = UInt(4.W)
  val u4 = UInt(4.W)
}

class BundleOfVecs extends Bundle {
  val v0 = Vec(2, UInt(4.W))
  val v1 = Vec(2, UInt(4.W))
  val v2 = Vec(2, UInt(4.W))
}

class Bundle2 extends Bundle {
  val u0 = UInt(4.W)
  val u1 = UInt(4.W)
}

class AggregateOrderingSpec extends AnyFreeSpec with ChiselScalatestTester {
  "the following examples illustrate the poking Vectors and Bundles with an array of BigInt" - {
    "Poking a 5 element Vec shows" in {
      test(new AggregatePassThrough(Vec(5, UInt(4.W)))).runPeekPoke(new AggregateOrderingTester(_, 5))
    }
    "Poking a 5 element bundle" in {
      test(new AggregatePassThrough(new Bundle5)).runPeekPoke(new AggregateOrderingTester(_, 5))
    }
    "Poking a bundle of 3 vecs of 2 uints each" in {
      test(new AggregatePassThrough(new BundleOfVecs)).runPeekPoke(new AggregateOrderingTester(_, 6))
    }
    "Poking a 3 vec of bundles of 2 uints each" in {
      test(new AggregatePassThrough(Vec(3, new Bundle2))).runPeekPoke(new AggregateOrderingTester(_, 6))
    }
  }

  """
    |The following test shows that counter-intuitive order of assignment when poking
    |a vec with an array of BigInts, currently poking reverse the order, but peeking does not
    |leading to a big inconsistency""".stripMargin - {
    "Poking vectors should be same as poking all elements" in {
      test(new VecPassThrough(10, UInt(16.W))).runPeekPoke(new VecPeekPokeTester(_))
    }
  }
  "The following illustrates that bundle ordering is " - {
    "Poking bundles should be same as poking all elements" in {
      test(new BundlePassThrough).runPeekPoke(new BundlePeekPokeTester(_))
    }
  }

}
