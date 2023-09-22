// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import chisel3._
import chiseltest._
import chiseltest.coverage.circuits.Test1Module
import firrtl2._
import firrtl2.annotations.CircuitTarget
import firrtl2.options.Dependency
import firrtl2.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class LineCoverageTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "parse the results from the simulator" in {
    val r = runTest()

    val data = LineCoverage.processCoverage(r)
    assert(data.files.size == 1)

    val file = data.files.head
    assert(file.name == "Test1Module.scala")

    val offset = 10
    val expected = List(
      // Test1Module
      (5, 5),
      (7, 5),
      (8, 0), // apparently `a` is never four
      (11, 5),
      (12, 1),
      (14, 4),
      (17, 5),
      (21, 5),
      (26, 5),
      (27, 5),
      (30, 5),
      (31, 5),
      (32, 5),
      (33, 5),
      // SubModule1 (instantiated twice!)
      (39, 10)
    )

    assert(file.lines == expected.map(e => (e._1 + offset, e._2)))
  }

  it should "generate a textual report" in {
    val data = LineCoverage.processCoverage(runTest())
    val code = new CodeBase(os.pwd / "test")
    val report = LineCoverage.textReport(code, data.files.head).toVector
    val lines = report.map(_.trim)

    // check some lines
    val offset = 10 + 2 // the 2 accounts for the table headers
    assert(lines(0 + offset).startsWith(s"${1 + offset - 2} |     | class Test1Module("))
    assert(lines(4 + offset).startsWith(s"${5 + offset - 2} |   5 |   b := 0.U // line 5"))
    assert(lines(7 + offset).startsWith(s"${8 + offset - 2} |   0 |     b := 1.U"))

    // this is how you would print the whole report
    // println(report.mkString("\n"))
  }

  private def runTest(): AnnotationSeq = {
    val rand = new scala.util.Random(0)
    val r = test(new Test1Module(withSubmodules = true)).withAnnotations(LineCoverage.annotations) { dut =>
      (0 until 4).foreach { _ =>
        dut.a.poke(BigInt(3, rand).U)
        dut.clock.step()
      }
    }
    r.getAnnotationSeq
  }
}

class LineCoverageInstrumentationTest extends AnyFlatSpec with CompilerTest {
  behavior.of("LineCoverage")

  override protected def annos = Seq(RunFirrtlTransformAnnotation(Dependency(LineCoveragePass)))

  it should "add cover statements" in {
    val (result, rAnnos) = compile(new Test1Module(), "low")
    val l = result.split('\n').map(_.trim)

    // we expect four custom cover points
    val c = """cover(clock, UInt<1>("h1"), UInt<1>("h1"), "") : """
    assert(l.contains(c + "l_0"))
    assert(l.contains(c + "l_1"))
    assert(l.contains(c + "l_2"))
    assert(l.contains(c + "l_3"))

    // we should have 4 coverage annotations as well
    val a = rAnnos.collect { case a: LineCoverageAnnotation => a }
    assert(a.size == 4)

    // lines for each coverage point (relative to the "class Test1Module " line)
    val offset = 10
    val lines = Map(
      "l_3" -> Seq(5, 7, 11, 17, 21, 26, 27),
      "l_0" -> Seq(8),
      "l_1" -> Seq(12),
      "l_2" -> Seq(14)
    )

    // all annotations should only point to `LineCoverageTest.scala`
    a.foreach { l =>
      assert(l.lines.size == 1)
      assert(l.lines.head._1 == "Test1Module.scala")
      assert(l.lines.head._2 == lines(l.target.ref).map(_ + offset))
    }
  }

  it should "work with deduplicated submodules" in {
    val (result, rAnnos) = compile(new Test1Module(withSubmodules = true), "low")
    // println(result)
    val l = result.split('\n').map(_.trim)

    // should only have two modules
    assert(l.count(_.startsWith("module")) == 2)

    // line coverage annotations are on a per module basis thus we expect 4 + 1 annotations
    val a = rAnnos.collect { case a: LineCoverageAnnotation => a }
    assert(a.length == 4 + 1)

    // check the annotation of the submodule
    val subAs = a.filterNot(_.target.module == "Test1Module")
    assert(subAs.size == 1)
    assert(subAs.head.lines.size == 1)
    assert(subAs.head.lines.head._1 == "Test1Module.scala")
    val offset = 10
    assert(subAs.head.lines.head._2 == Seq(39).map(_ + offset))
  }

  it should "ignore modules when they are appropriately annotated" in {
    val ignore = Seq(DoNotCoverAnnotation(CircuitTarget("Test1Module").module("Test1Module")))
    val (result, rAnnos) = compile(new Test1Module(withSubmodules = true), "low", ignore)
    // println(result)
    // val l = result.split('\n').map(_.trim)

    // we only expect the submodule to have a line coverage annotation
    val a = rAnnos.collect { case a: LineCoverageAnnotation => a }
    assert(a.length == 1)
    assert(a.head.target.module == "SubModule1")
  }
}
