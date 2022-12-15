// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.verilator

import chisel3._
import chiseltest._
import chiseltest.experimental.sanitizeFileName
import chiseltest.simulator.{RequiresVerilator, VerilatorFlags}
import firrtl.AnnotationSeq
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

private class TestModule extends Module {
  val in = IO(Input(Bool()))
  val reg = RegNext(~in)
  val out = IO(Output(Bool()))
  cover(out && in)
  out := reg
}

class VerilatorCoverageTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "allow specifying Verilog toggle coverage for Verilator" taggedAs RequiresVerilator in {
    clean()
    val annos = Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq("--coverage-toggle")))
    runTest(annos)
    val counts = coverageTypes()
    assert(counts.user == 1, "user coverage is always included!")
    assert(counts.toggle == 5)
    assert(counts.line == 0)
  }

  it should "allow specifying Verilog line coverage for Verilator" taggedAs RequiresVerilator in {
    clean()
    val annos = Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq("--coverage-line")))
    runTest(annos)
    val counts = coverageTypes()
    assert(counts.user == 1, "user coverage is always included!")
    assert(counts.toggle == 0)
    assert(counts.line == 3 || counts.line == 1) // different verilator versions add different numbers of cover points
  }

  it should "allow specifying Verilog structural coverage for Verilator" taggedAs RequiresVerilator in {
    clean()
    val annos = Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq("--coverage-toggle", "--coverage-line")))
    runTest(annos)
    val counts = coverageTypes()
    assert(counts.user == 1, "user coverage is always included!")
    assert(counts.toggle == 5)
    assert(counts.line == 3 || counts.line == 1) // different verilator versions add different numbers of cover points
  }

  it should "always generate user coverage" taggedAs RequiresVerilator in {
    clean()
    val annos = Seq(VerilatorBackendAnnotation)
    runTest(annos)
    val counts = coverageTypes()
    assert(counts.user == 1, "user coverage is always included!")
    assert(counts.toggle == 0)
    assert(counts.line == 0)
  }

  // run a basic test in order to generate some interesting coverage data
  private def runTest(annos: AnnotationSeq): Unit = {
    val rand = new scala.util.Random(0)
    test(new TestModule).withAnnotations(annos) { dut =>
      (0 until 50).foreach { _ =>
        val in = rand.nextBoolean()
        dut.in.poke(in.B)
        dut.clock.step()
        dut.out.expect((!in).B)
      }
    }
  }

  private def testDir: os.Path = os.pwd / "test_run_dir" / sanitizeFileName(scalaTestContext.value.get.name)

  private def clean(): Unit = {
    if(os.exists(testDir)) { os.remove.all(testDir) }
  }

  private def loadCoverage(): Seq[Map[String, String]] = {
    assert(os.exists(testDir))
    val coverageFile = testDir / "coverage.dat"
    assert(os.exists(coverageFile))
    val lines = os.read.lines(coverageFile).drop(1)
    lines.map(_.split('\'').toList).map {
        case List(_, dict, countStr) =>
          dict.drop(1).split('\u0001').map(_.split('\u0002').toList).collect { case Seq(k, v) => k -> v }.toMap
        case _ => Map[String, String]()
    }.toSeq
  }

  private case class Counts(user: Int, toggle: Int, line: Int)
  private def coverageTypes(): Counts = {
    val cov = loadCoverage()
    Counts(
      user = cov.count(_("page").startsWith("v_user")),
      toggle = cov.count(_("page").startsWith("v_toggle")),
      line = cov.count(_("page").startsWith("v_line")),
    )
  }
}
