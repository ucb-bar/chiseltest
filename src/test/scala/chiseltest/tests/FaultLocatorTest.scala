// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FaultLocatorTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Testers2")

  it should "locate source lines" in {
    val nameAndLine = intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        c.out.expect(0.U)
      }
    }.failedCodeFileNameAndLineNumberString.get
    assert(nameAndLine == "FaultLocatorTest.scala:17")
  }

  it should "locate source lines across threads" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        fork {
          c.clock.step()
          c.out.expect(0.U)
        }.join()
      }
    }
    assert(exc.getMessage().endsWith("at (FaultLocatorTest.scala:28)"))
  }

  it should "locate source lines in libraries" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new PassthroughQueue(Bool())) { c =>
        c.out.initSink()

        c.in.valid.poke(true.B)
        c.in.bits.poke(false.B) // Have this be a data failure only
        c.out.expectDequeueNow(true.B)
      }
    }
    // Only check the filename to avoid this being too brittle as implementation changes
    assert(exc.failedCodeFileNameAndLineNumberString.get.startsWith("DecoupledDriver.scala:"))
    assert(exc.getMessage().endsWith("at (FaultLocatorTest.scala:42)"))
  }

  it should "locate source lines, even in a different thread" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new PassthroughQueue(Bool())) { c =>
        c.out.initSink()

        c.in.valid.poke(true.B)
        c.in.bits.poke(false.B) // Have this be a data failure only
        fork {
          c.out.expectDequeueNow(true.B)
        }.join()
      }
    }
    assert(exc.failedCodeFileNameAndLineNumberString.get.startsWith("DecoupledDriver.scala:"))
    assert(exc.getMessage().endsWith("at (FaultLocatorTest.scala:58)"))
  }
}
