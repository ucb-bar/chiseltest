// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import chisel3._
import chisel3.experimental.ExtModule
import chiseltest._
import chiseltest.coverage.circuits.Test1Module
import firrtl2.AnnotationSeq
import firrtl2.annotations.{CircuitTarget, ReferenceTarget}
import firrtl2.options.Dependency
import firrtl2.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class ToggleTestModule extends Module {
  val in = IO(Input(UInt(8.W)))
  val out0 = IO(Output(UInt(8.W)))
  val out1 = IO(Output(UInt(8.W)))

  val c0 = Module(new ToggleTestChild)
  c0.in := in
  out0 := c0.out0
  out1 := c0.out1
  val w_test = WireInit(in)
  dontTouch(w_test)
}

class ToggleTestChild extends Module {
  val in = IO(Input(UInt(8.W)))
  val out0 = IO(Output(UInt(8.W)))
  val out1 = IO(Output(UInt(8.W)))
  out0 := in
  out1 := RegNext(in)
  val g0 = Module(new ToggleTestGrandChild)
  dontTouch(g0.reset)
}

class ToggleTestGrandChild extends Module {
  // empty, just has a reset
}

class ToggleTestWithBottomUpReset extends RawModule {
  val clock = IO(Input(Clock()))
  val resetGen = Module(new ResetBlackbox)
  val reset = resetGen.reset
  withClockAndReset(clock, reset) {
    val in = IO(Input(UInt(8.W)))
    val out0 = IO(Output(UInt(8.W)))
    val out1 = IO(Output(UInt(8.W)))

    val c0 = Module(new ToggleTestChild)
    c0.in := in
    out0 := c0.out0
    out1 := c0.out1
    val w_test = WireInit(in)
    dontTouch(w_test)
  }
}

class ResetBlackbox extends ExtModule {
  val reset = IO(Output(Bool()))
}

class ToggleCoverageTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("ToggleCoverage")

  it should "parse the results from the simulator" ignore {
    val r = runTest()

    val data = ToggleCoverage.processCoverage(r)
    assert(data.inst.length == 1 + 1 + 1)

    // there are three instances that have been covered
    assert(data.inst.map(_._1._1).sorted == List("", "c0", "c0.g0"))

    // there are three modules that have been covered
    assert(
      data.inst.map(_._1._2).distinct.sorted ==
        List("ToggleTestChild", "ToggleTestGrandChild", "ToggleTestModule")
    )
  }

  private def runTest(): AnnotationSeq = {
    val r = test(new ToggleTestModule()).withAnnotations(WriteVcdAnnotation +: ToggleCoverage.all) { dut =>
      // stepping without togelling should not change anything
      (0 until 4).foreach { _ => dut.clock.step() }

      // we toggle some select bits
      val bits = Seq(0, 3, 5, 7)

      bits.foreach { b =>
        dut.in.poke((BigInt(1) << b).U)
        dut.clock.step()
      }
    }
    r.getAnnotationSeq
  }
}

class ToggleCoverageInstrumentationTest extends AnyFlatSpec with CompilerTest {
  behavior.of("ToggleCoverage Instrumentation")

  override protected def annos = Seq(RunFirrtlTransformAnnotation(Dependency(ToggleCoveragePass)))

  it should "add cover statements" in {
    val (result, rAnnos) = compile(new Test1Module(), "sverilog")
    // println(result)
    val l = result.split('\n').map(_.trim)
  }

  it should "only create one counter when there are obvious aliases" in {
    val (result, rAnnos) = compile(new ToggleTestModule(), "low", a = ToggleCoverage.all)
    // println(result)
    val l = result.split('\n').map(_.trim)

    val annos = rAnnos.collect { case a: ToggleCoverageAnnotation => a }

    // we expect the covered signals to be the following
    val expected = List(
      // all signals in the ToggleTestChild module
      "ToggleTestChild.g0.reset",
      "ToggleTestChild.in",
      "ToggleTestChild.out0",
      "ToggleTestChild.out1",
      "ToggleTestChild.out1_REG",
      "ToggleTestChild.reset",
      // all signals in the ToggleTestGrandChild module
      "ToggleTestGrandChild.reset",
      // all signals in the ToggleTestModule module
      "ToggleTestModule.c0.in",
      "ToggleTestModule.c0.out0",
      "ToggleTestModule.c0.out1",
      "ToggleTestModule.c0.reset",
      "ToggleTestModule.in",
      "ToggleTestModule.out0",
      "ToggleTestModule.out1",
      "ToggleTestModule.reset",
      "ToggleTestModule.w_test"
    )

    val coverNames = annos.flatMap { a => a.signals.map(refToString) }.distinct.sorted
    assert(coverNames == expected)

    // Check how many how many cover statements there are
    val covers = l.filter(_.startsWith("cover("))
    val coverCount = covers.length

    // We expect there to be fewer actual cover statement than signals covered.
    // The following signals alias:
    // - ToggleTestChild.REG -> ToggleTestChild.out1 -> ToggleTestModule.out1
    // - ToggleTestModule.in -> ToggleTestChild.in -> ToggleTestChild.out0 -> ToggleTestModule.out0
    // - ToggleTestModule.reset -> ToggleTestChild.reset
    // Thus we expect the following number of cover statements:
    val expectedCoverBits = 8 + 8 + 1
    assert(coverCount == expectedCoverBits, "\n" + covers.mkString("\n"))

    // We expect there to be only a single `reg enToggle` because there should be
    // no cover statements in the child module since all signals are exposed on the IOs.
    val enToggleLines = l.filter(_.contains("reg enToggle"))
    assert(enToggleLines.length == 1, enToggleLines.mkString("\n"))
  }

  it should "work, even if the toplevel module is ignored" in {
    val ignored = DoNotCoverAnnotation(CircuitTarget("ToggleTestModule").module("ToggleTestModule"))
    val (result, _) = compile(new ToggleTestModule(), "low", a = ignored +: ToggleCoverage.all)
    // println(result)
    val l = result.split('\n').map(_.trim)

    // we still expect there to be exactly 17 cover statements since the toplevel is purely a pass-through module
    val covers = l.filter(_.startsWith("cover("))
    val coverCount = covers.length
    val expectedCoverBits = 8 + 8 + 1
    assert(coverCount == expectedCoverBits, "\n" + covers.mkString("\n"))
  }

  it should "work, even with a reset that is generated by an ext module" in {
    val (result, _) = compile(new ToggleTestWithBottomUpReset(), "low", a = ToggleCoverage.all)
    // println(result)
    val l = result.split('\n').map(_.trim)

    // we still expect there to be exactly 17 cover statements
    val covers = l.filter(_.startsWith("cover("))
    val coverCount = covers.length
    val expectedCoverBits = 8 + 8 + 1
    assert(coverCount == expectedCoverBits, "\n" + covers.mkString("\n"))
  }

  private def refToString(r: ReferenceTarget): String =
    r.toString().split('|').last.replace('>', '.')
}
