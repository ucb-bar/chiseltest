// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import firrtl.options.Dependency
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.{AnnotationSeq, EmittedFirrtlCircuitAnnotation, EmittedFirrtlModuleAnnotation}
import firrtl.stage.RunFirrtlTransformAnnotation

class LineCoverageTest extends AnyFlatSpec {
  behavior of "LineCoverage"

  private val annos = Seq(RunFirrtlTransformAnnotation(Dependency(LineCoveragePass)))


  it should "add cover statements" in {
    val (result, rAnnos) = compile(new Test1Module())
    val l = result.split('\n').map(_.trim)

    // we expect four custom cover points
    val c = """cover(clock, UInt<1>("h1"), UInt<1>("h1"), "") : """
    assert(l.contains(c + "l_0"))
    assert(l.contains(c + "l_1"))
    assert(l.contains(c + "l_2"))
    assert(l.contains(c + "l_3"))

    // we should have 4 coverage annotations as well
    val a = rAnnos.collect{ case a: LineCoverageAnnotation => a }
    assert(a.size == 4)

    // lines for each coverage point (relative to the "class Test1Module " line)
    val offset = 6
    val lines = Map(
      "l_3" -> Seq(5, 7, 11, 17, 21, 26, 27),
      "l_0" -> Seq(8),
      "l_1" -> Seq(12),
      "l_2" -> Seq(14),
    )

    // all annotations should only point to `LineCoverageTest.scala`
    a.foreach { l =>
      assert(l.lines.size == 1)
      assert(l.lines.head._1 == "Test1Module.scala")
      assert(l.lines.head._2 == lines(l.target.ref).map(_ + offset))
    }
  }

  private def compile[M <: Module](gen: => M): (String, AnnotationSeq) = {
    val stage = new ChiselStage

    val r = stage.execute(Array("-X", "high"), ChiselGeneratorAnnotation(() => gen) +: annos)
    val src = r.collect {
        case EmittedFirrtlCircuitAnnotation(a) => a
        case EmittedFirrtlModuleAnnotation(a)  => a
      }.map(_.value).mkString("")

    (src, r)
  }
}
