// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import java.io.{ByteArrayOutputStream, File, PrintStream}

import chisel3._
import chiseltest._
import chiseltest.stage._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VerilatorCoverageTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "allow specifying toggle coverage for Verilator" in {
    val coverageName = "test_run_dir/Testers2_should_allow_specifying_toggle_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new MultiIOModule {}).withAnnotations(Seq(VerilatorBackendAnnotation, ToggleCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    coverage.exists() should be(true)
    output.contains("--coverage-toggle") should be(true)
    output.contains("--coverage-line") should be(false)
    output.contains("--coverage-user") should be(false)
  }

  it should "allow specifying line coverage for Verilator" in {
    val coverageName = "test_run_dir/Testers2_should_allow_specifying_line_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new MultiIOModule {}).withAnnotations(Seq(VerilatorBackendAnnotation, LineCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    coverage.exists() should be(true)
    output.contains("--coverage-toggle") should be(false)
    output.contains("--coverage-line") should be(true)
    output.contains("--coverage-user") should be(false)
  }

  it should "allow specifying structural coverage for Verilator" in {
    val coverageName = "test_run_dir/Testers2_should_allow_specifying_structural_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new MultiIOModule {}).withAnnotations(Seq(VerilatorBackendAnnotation, StructuralCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    coverage.exists() should be(true)
    output.contains("--coverage-toggle") should be(true)
    output.contains("--coverage-line") should be(true)
    output.contains("--coverage-user") should be(false)
  }

  it should "allow specifying user coverage for Verilator" in {
    val coverageName = "test_run_dir/Testers2_should_allow_specifying_user_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new MultiIOModule {}).withAnnotations(Seq(VerilatorBackendAnnotation, UserCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    coverage.exists() should be(true)
    output.contains("--coverage-toggle") should be(false)
    output.contains("--coverage-line") should be(false)
    output.contains("--coverage-user") should be(true)
  }

  it should "allow stacking coverage for Verilator" in {
    val coverageName = "test_run_dir/Testers2_should_allow_stacking_coverage_for_Verilator/logs/coverage.dat"
    val coverage = new File(coverageName)
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new MultiIOModule {}).withAnnotations(Seq(VerilatorBackendAnnotation, UserCoverageAnnotation, StructuralCoverageAnnotation)) { c => }
    }
    val output = outputStream.toString
    coverage.exists() should be(true)
    output.contains("--coverage-toggle") should be(true)
    output.contains("--coverage-line") should be(true)
    output.contains("--coverage-user") should be(true)
  }
}
