// SPDX-License-Identifier: Apache-2.0


package chiseltest.coverage

import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest._
import chiseltest.internal.{BackendAnnotation, TreadleBackendAnnotation, VerilatorBackendAnnotation}
import firrtl.AnnotationSeq
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._

/** Ensure that all simulators give the same coverage feedback when run with the same tests.
  * To implement this for a particular simulator, just override the `backend` annotation.
  * */
abstract class SimulatorCoverageTest(name: String, backend: BackendAnnotation) extends AnyFlatSpec with ChiselScalatestTester {
  behavior of s"$name Coverage Collection"
  private def noAutoCov: AnnotationSeq = Seq(backend)

  it should "report count for all user cover points (no submodules)" in {
    val r = test(new Test1Module(withSubmodules = false)).withAnnotations(noAutoCov) { dut =>
      dut.clock.step()
    }
    val cov = getCoverage(r)

    // the cover point in the main module are not prefixed
    assert(cov.keys.toList == List("cover_0"))

    // since we executed one step (+ the implicit reset) and all inputs are zero by default, we expect the count to be 3
    assert(cov("cover_0") == 3)
  }

  it should "report count for all user cover points (with submodules)" in {
    val r = test(new Test1Module(withSubmodules = true)).withAnnotations(noAutoCov) { dut =>
      dut.clock.step()
    }
    val cov = getCoverage(r)

    // the cover point in the main module are not prefixed, but the one in the child module are
    assert(cov.keys.toList.sorted == List("c0.cover_0", "c1.cover_0", "cover_0"))

    // since we executed one step (+ the implicit reset) and all inputs are zero by default, we expect the count to be 3
    assert(cov("cover_0") == 3)
    assert(cov("c0.cover_0") == 0)
    assert(cov("c1.cover_0") == 0)
  }

  // this is an integration test that uses chisel3.experimental.verification.cover to estimate PI
  it should "allow us to estimate pi" in {
    val rand = new scala.util.Random(0)
    val r = test(new PiEstimator).withAnnotations(noAutoCov) { dut =>
      (0 until 1000).foreach { _ =>
        dut.x.poke(BigInt(8, rand).S)
        dut.y.poke(BigInt(8, rand).S)
        dut.clock.step()
      }
    }

    val cov = getCoverage(r)
    val inCircle = cov("cover_0")
    val inRectangle = cov("cover_1")
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
  chisel3.experimental.verification.cover(inCircle)
  chisel3.experimental.verification.cover(inRectangle)
}
