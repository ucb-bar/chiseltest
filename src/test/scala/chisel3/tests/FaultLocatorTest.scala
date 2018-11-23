package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.tester.TestAdapters._

class FaultLocatorTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "locate source lines" in {
    intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        c.out.expect(0.U)
      }
    }.failedCodeFileNameAndLineNumberString.get should equal ("FaultLocatorTest.scala:16")
  }

  it should "locate source lines in libraries" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new PassthroughModule(Decoupled(Bool()))) { c =>
        c.in.valid.poke(true.B)
        c.in.bits.poke(false.B)  // Have this be a data failure only
        val sink = new ReadyValidSink(c.out, c.clock)
        sink.dequeueNowExpect(true.B)
      }
    }
    // Only check the filename to avoid this being too brittle as TestAdapters.scala changes
    exc.failedCodeFileNameAndLineNumberString.get should startWith ("TestAdapters.scala:")
    exc.getMessage should include regex ("""\(lines in FaultLocatorTest\.scala:[^\)]*27.*\)""")
  }

  it should "locate source lines, even in a different thread" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new PassthroughModule(Decoupled(Bool()))) { c =>
        c.in.valid.poke(true.B)
        c.in.bits.poke(false.B)  // Have this be a data failure only
        val sink = new ReadyValidSink(c.out, c.clock)
        fork {
          sink.dequeueNowExpect(true.B)
        } .join
      }
    }
    exc.failedCodeFileNameAndLineNumberString.get should startWith ("TestAdapters.scala:")
    exc.getMessage should include regex ("""\(lines in FaultLocatorTest\.scala:[^\)]*42.*\)""")
  }
}
