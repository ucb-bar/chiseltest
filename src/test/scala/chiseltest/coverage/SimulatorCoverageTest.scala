// SPDX-License-Identifier: Apache-2.0


package chiseltest.coverage

import chisel3.tester.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest._
import chiseltest.internal.{BackendAnnotation, TreadleBackendAnnotation, VerilatorBackendAnnotation}
import firrtl.AnnotationSeq
import logger.{LogLevel, LogLevelAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

/** Ensure that all simulators give the same coverage feedback when run with the same tests.
  * To implement this for a particular simulator, just override the `backend` annotation.
  * */
abstract class SimulatorCoverageTest(name: String, backend: BackendAnnotation) extends AnyFlatSpec with ChiselScalatestTester {
  behavior of s"$name Coverage Collection"
  private def noAutoCov: AnnotationSeq = Seq(backend)
  private def trace: AnnotationSeq = Seq(LogLevelAnnotation(LogLevel.Trace))

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
  }

  private def getCoverage(annos: AnnotationSeq): Map[String, Long] = {
    val coverage = annos.collect { case a: TestCoverage => a.counts }
    assert(coverage.size == 1, "Expecting exactly one coverage map!")
    coverage.head.toMap
  }
}

class TreadleCoverageTest extends SimulatorCoverageTest("Treadle", TreadleBackendAnnotation) {}
class VerilatorCoverageTest extends SimulatorCoverageTest("Verilator", VerilatorBackendAnnotation) {}
