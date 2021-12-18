// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import chisel3._
import chiseltest._
import chiseltest.experimental.sanitizeFileName
import chiseltest.simulator.{RequiresVcs, VcsFlags, VcsSimFlags}
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

class VcsCoverageTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  private def SimAndCompileFlags(flags: Seq[String]): AnnotationSeq = Seq(VcsFlags(flags), VcsSimFlags(flags))

  it should "allow specifying line coverage for Vcs" taggedAs(RequiresVcs) in {
    clean()
    val annos = Seq(VcsBackendAnnotation) ++ SimAndCompileFlags(Seq("-cm", "line"))
    runTest(annos)

    // TODO: check output file
  }

  it should "allow specifying toggle coverage for Vcs" taggedAs(RequiresVcs) in {
    clean()
    val annos = Seq(VcsBackendAnnotation) ++ SimAndCompileFlags(Seq("-cm", "tgl"))
    runTest(annos)

    // TODO: check output file
  }

  it should "allow specifying branch coverage for Vcs" taggedAs(RequiresVcs) in {
    clean()
    val annos = Seq(VcsBackendAnnotation) ++ SimAndCompileFlags(Seq("-cm", "branch"))
    runTest(annos)

    // TODO: check output file
  }

  it should "allow specifying conditional coverage for Vcs" taggedAs(RequiresVcs) in {
    clean()
    val annos = Seq(VcsBackendAnnotation) ++ SimAndCompileFlags(Seq("-cm", "cond"))
    runTest(annos)

    // TODO: check output file
  }

  it should "allow specifying structural coverage for Vcs" taggedAs(RequiresVcs) in {
    clean()
    val annos = Seq(VcsBackendAnnotation) ++ SimAndCompileFlags(Seq("-cm", "line+tgl+branch+cond"))
    runTest(annos)

    // TODO: check output file
  }

  it should "allow specifying user coverage for Vcs" taggedAs(RequiresVcs) in {
    clean()
    val annos = Seq(VcsBackendAnnotation) ++ SimAndCompileFlags(Seq("-cm", "assert"))
    runTest(annos)

    // TODO: check output file
  }

  it should "allow stacking coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)
    clean()
    val annos = Seq(VcsBackendAnnotation) ++ SimAndCompileFlags(Seq("-cm", "line+tgl+branch+cond+assert"))
    runTest(annos)

    // TODO: check output file
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
}
