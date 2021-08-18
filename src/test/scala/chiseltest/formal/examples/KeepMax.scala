// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal.examples

import chisel3._
import chisel3.experimental.verification
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec

class KeepMaxVerify extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "KeppMax(1)" should "have a monotonically increasing output" taggedAs FormalTag in {
    verify(new KeepMax(1), Seq(BoundedCheck(4)))
  }

  "KeppMax(8)" should "have a monotonically increasing output" taggedAs FormalTag in {
    verify(new KeepMax(8), Seq(BoundedCheck(4)))
  }
}


/** KeepMax demo by Tom Alcorn
  * src: https://github.com/tdb-alcorn/chisel-formal/blob/master/src/test/scala/chisel3/formal/SanitySpec.scala
  */
class KeepMax(width: Int) extends Module {
  val in = IO(Input(UInt(width.W)))
  val out = IO(Output(UInt(width.W)))

  val max = RegInit(0.U(width.W))
  when (in > max) {
    max := in
  }
  out := max

  // get the value of io.out from 1 cycle in the past
  verification.assert(out >= past(out))
}
