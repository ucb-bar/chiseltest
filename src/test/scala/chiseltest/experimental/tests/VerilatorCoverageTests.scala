// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import java.io.{ByteArrayOutputStream, File, PrintStream}

import chisel3._
import chiseltest._
import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VerilatorCoverageTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  private def coverageTest(annotations: AnnotationSeq) = {
    val outputStream = new ByteArrayOutputStream()
    val targetDir = Console.withOut(new PrintStream(outputStream)) {
      test(new MultiIOModule {}).withAnnotations(annotations) { c => }.collectFirst{
        case TargetDirAnnotation(t) => t
      }.get
    }
    val coverage = new File(targetDir + s"${java.io.File.separator}logs${java.io.File.separator}coverage.dat")
    coverage.exists() should be(true)
    outputStream.toString
  }

  it should "allow specifying toggle coverage for Verilator" in {
    val output = coverageTest(Seq(VerilatorBackendAnnotation, ToggleCoverageAnnotation))
    output.contains("--coverage-toggle") should be(true)
    output.contains("--coverage-line") should be(false)
    output.contains("--coverage-user") should be(false)
  }

  it should "allow specifying line coverage for Verilator" in {
    val output = coverageTest(Seq(VerilatorBackendAnnotation, LineCoverageAnnotation))
    output.contains("--coverage-toggle") should be(false)
    output.contains("--coverage-line") should be(true)
    output.contains("--coverage-user") should be(false)
  }

  it should "allow specifying structural coverage for Verilator" in {
    val output = coverageTest(Seq(VerilatorBackendAnnotation, StructuralCoverageAnnotation))
    output.contains("--coverage-toggle") should be(true)
    output.contains("--coverage-line") should be(true)
    output.contains("--coverage-user") should be(false)
  }

  it should "allow specifying user coverage for Verilator" in {
    val output = coverageTest(Seq(VerilatorBackendAnnotation, UserCoverageAnnotation))
    output.contains("--coverage-toggle") should be(false)
    output.contains("--coverage-line") should be(false)
    output.contains("--coverage-user") should be(true)
  }

  it should "allow stacking coverage for Verilator" in {
    val output = coverageTest(Seq(VerilatorBackendAnnotation, UserCoverageAnnotation, StructuralCoverageAnnotation))
    output.contains("--coverage-toggle") should be(true)
    output.contains("--coverage-line") should be(true)
    output.contains("--coverage-user") should be(true)
  }
}
