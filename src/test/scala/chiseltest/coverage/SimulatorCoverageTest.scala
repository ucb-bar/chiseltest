// SPDX-License-Identifier: Apache-2.0


package chiseltest.coverage

import chiseltest._
import firrtl.AnnotationSeq
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chiseltest.simulator.{DefaultTag, SimulatorAnnotation}
import org.scalatest.Tag

/** Ensure that all simulators give the same coverage feedback when run with the same tests.
  * To implement this for a particular simulator, just override the `backend` annotation.
  * */
abstract class SimulatorCoverageTest(name: String, backend: SimulatorAnnotation, tag: Tag = DefaultTag) extends AnyFlatSpec with ChiselScalatestTester {
  behavior of s"$name Coverage Collection"
  private def noAutoCov: AnnotationSeq = Seq(backend)

  it should "report count for all user cover points (no submodules)" taggedAs tag in {
    val r = test(new Test1Module(withSubmodules = false)).withAnnotations(noAutoCov) { dut =>
      dut.clock.step()
    }
    val cov = getCoverage(r)

    // the cover point in the main module are not prefixed
    assert(cov.keys.toList == List("user_cov"))

    // since we executed one step and all inputs are zero by default, we expect the count to be 1
    assert(cov("user_cov") == 1)
  }

  it should "report count for all user cover points (with submodules)" taggedAs tag in {
    val r = test(new Test1Module(withSubmodules = true)).withAnnotations(noAutoCov) { dut =>
      dut.clock.step()
    }
    val cov = getCoverage(r)

    // the cover point in the main module are not prefixed, but the one in the child module are
    assert(cov.keys.toList.sorted == List("c0.user_cov_2", "c1.user_cov_2", "user_cov"))

    // since we executed one step and all inputs are zero by default, we expect the count to be 3
    assert(cov("user_cov") == 1)
    assert(cov("c0.user_cov_2") == 0)
    assert(cov("c1.user_cov_2") == 0)
  }

  // this is an integration test that uses chisel3.experimental.verification.cover to estimate PI
  it should "allow us to estimate pi" taggedAs tag in {
    val rand = new scala.util.Random(0)
    val r = test(new PiEstimator).withAnnotations(noAutoCov) { dut =>
      (0 until 1000).foreach { _ =>
        dut.x.poke(BigInt(8, rand).S)
        dut.y.poke(BigInt(8, rand).S)
        dut.clock.step()
      }
    }

    val cov = getCoverage(r)
    val inCircle = cov("inCircleCover")
    val inRectangle = cov("inRectangleCover")
    val pi = 4.0 * inCircle.toDouble / inRectangle.toDouble
    val error = math.abs(pi - math.Pi)
    assert(error < 0.0329)
  }

  private def getCoverage(r: TestResult): Map[String, Long] = {
    val coverage = r.getAnnotationSeq.collect { case a: TestCoverage => a.counts }
    assert(coverage.size == 1, "Expecting exactly one coverage map!")
    coverage.head.toMap
  }
}

class TreadleCoverageTest extends SimulatorCoverageTest("Treadle", TreadleBackendAnnotation) {}

private class PiEstimator extends Module {
  val x = IO(Input(SInt(8.W)))
  val y = IO(Input(SInt(8.W)))
  val inCircle = IO(Output(Bool()))
  val inRectangle = IO(Output(Bool()))
  val radius = 100
  inCircle := (x * x + y * y) <= (radius * radius).S
  inRectangle := (x <= radius.S && x >= -radius.S) && (y <= radius.S && y >= -radius.S)
  cover(inCircle).suggestName("inCircleCover")
  cover(inRectangle).suggestName("inRectangleCover")
}
