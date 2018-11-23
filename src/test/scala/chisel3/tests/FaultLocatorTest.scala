package chisel3.tests

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.tester.TestAdapters._

class FaultLocatorTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2"

  it should "locate source lines" in {
    assert(intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)) { c =>
        c.out.expect(0.U)
      }
    }.failedCodeFileNameAndLineNumberString.get == "FaultLocatorTest.scala:16")
  }

  it should "locate source lines in libraries" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new PassthroughModule(Decoupled(Bool()))) { c =>
        c.in.valid.poke(false.B)
        val sink = new ReadyValidSink(c.out, c.clock)
        sink.dequeueNowExpect(true.B)
      }
    }
    // Only check the filename to avoid this being too brittle as TestAdapters.scala changes
    assert(exc.failedCodeFileNameAndLineNumberString.get.startsWith("TestAdapters.scala:"))
  }
}
