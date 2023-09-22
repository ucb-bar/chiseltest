// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import chisel3._
import chisel3.util._
import chiseltest.coverage.circuits.FifoRegister
import firrtl2._
import firrtl2.options.Dependency
import firrtl2.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec.AnyFlatSpec

object FsmState extends ChiselEnum {
  val A, B, C = Value
}

class ExampleFsm1 extends Module {
  val in = IO(Input(Bool()))
  val out = IO(Output(UInt(3.W)))
  import FsmState._

  val state = RegInit(A)
  switch(state) {
    is(A) { state := Mux(in, A, B) }
    is(B) {
      when(in) { state := B }.otherwise { state := C }
    }
  }
  out := state.asUInt
}

class FsmInfoPassTests extends AnyFlatSpec with CompilerTest {

  override protected def annos = Seq(RunFirrtlTransformAnnotation(Dependency(FsmInfoPass)))

  private def getSingleInfo(rAnnos: AnnotationSeq): FsmInfoAnnotation = {
    val infos = rAnnos.collect { case a: FsmInfoAnnotation => a }
    assert(infos.length == 1, "expected exactly one info since there is only one FSM in the design")
    infos.head
  }

  it should "properly analyze the FIFO register FSM" in {
    val (_, rAnnos) = compile(new FifoRegister(8), "low")
    val info = getSingleInfo(rAnnos)

    checkInfo(
      info,
      states = Seq(0 -> "Empty", 1 -> "Full"),
      start = Some("Empty"),
      transitions = Seq(
        "Empty" -> "Empty",
        "Empty" -> "Full",
        "Full" -> "Empty",
        "Full" -> "Full"
      )
    )
  }

  it should "analyze the Example FSM 1" in {
    val (_, rAnnos) = compile(new ExampleFsm1, "low")
    val info = getSingleInfo(rAnnos)

    checkInfo(
      info,
      states = Seq(0 -> "A", 1 -> "B", 2 -> "C"),
      start = Some("A"),
      transitions = Seq(
        "A" -> "A",
        "A" -> "B",
        "B" -> "B",
        "B" -> "C",
        "C" -> "C"
      )
    )
  }

  val ResourceDir = os.pwd / "test" / "resources"

  it should "properly analyze the FSMs in RISC-V Mini" in {
    val (_, rAnnos) =
      compileFile(ResourceDir / "RiscVMiniTileTester.fir", ResourceDir / "RiscVMiniTileTester.fsm.json", "low")
    val infos = rAnnos.collect { case a: FsmInfoAnnotation => a }

    // The Cache has a state machine
    val cacheInfo = infos.find(_.target.toString().contains("Cache")).get
    checkInfo(
      cacheInfo,
      states = Seq(
        0 -> "sIdle",
        1 -> "sReadCache",
        2 -> "sWriteCache",
        3 -> "sWriteBack",
        4 -> "sWriteAck",
        5 -> "sRefillReady",
        6 -> "sRefill"
      ),
      start = Some("sIdle"),
      // transitions from a manual analysis
      transitions = Seq(
        // is(sIdle)
        "sIdle" -> "sIdle", // !io.cpu.req.valid
        "sIdle" -> "sWriteCache", //  io.cpu.req.valid &&  io.cpu.req.bits.mask.orR
        "sIdle" -> "sReadCache", //  io.cpu.req.valid && !io.cpu.req.bits.mask.orR
        // is(sReadCache)
        "sReadCache" -> "sWriteCache", //  hit &&  io.cpu.req.valid &&  io.cpu.req.bits.mask.orR
        "sReadCache" -> "sReadCache", //  hit &&  io.cpu.req.valid && !io.cpu.req.bits.mask.orR
        "sReadCache" -> "sIdle", //  hit && !io.cpu.req.valid
        "sReadCache" -> "sWriteBack", // !hit &&  io.nasti.aw.fire
        "sReadCache" -> "sRefill", // !hit && !io.nasti.aw.fire &&  io.nasti.ar.fire
        "sReadCache" -> "sReadCache", // !hit && !io.nasti.aw.fire && !io.nasti.ar.fire
        // is(sWriteCache)
        "sWriteCache" -> "sIdle", //  (hit || is_alloc_reg || io.cpu.abort)
        "sWriteCache" -> "sWriteBack", // !(hit || is_alloc_reg || io.cpu.abort) &&  io.nasti.aw.fire
        "sWriteCache" -> "sRefill", // !(hit || is_alloc_reg || io.cpu.abort) && !io.nasti.aw.fire &&  io.nasti.ar.fire
        "sWriteCache" -> "sWriteCache", // !(hit || is_alloc_reg || io.cpu.abort) && !io.nasti.aw.fire && !io.nasti.ar.fire
        // is(sWriteBack)
        "sWriteBack" -> "sWriteAck", //  write_wrap_out
        "sWriteBack" -> "sWriteBack", // !write_wrap_out
        // is(sWriteAck)
        "sWriteAck" -> "sRefillReady", //  io.nasti.b.fire
        "sWriteAck" -> "sWriteAck", // !io.nasti.b.fire
        // is(sRefillReady)
        "sRefillReady" -> "sRefill", //  io.nasti.b.fire
        "sRefillReady" -> "sRefillReady", // !io.nasti.b.fire
        // is(sRefill)
        "sRefill" -> "sWriteCache", //  read_wrap_out &&  cpu_mask.orR
        "sRefill" -> "sIdle", //  read_wrap_out && !cpu_mask.orR
        "sRefill" -> "sRefill" // !read_wrap_out
      )
    )

    val arbiterInfo = infos.find(_.target.toString().contains("MemArbiter")).get
    checkInfo(
      arbiterInfo,
      states = Seq(0 -> "sIdle", 1 -> "sICacheRead", 2 -> "sDCacheRead", 3 -> "sDCacheWrite", 4 -> "sDCacheAck"),
      start = Some("sIdle"),
      transitions = Seq(
        // is(sIdle)
        "sIdle" -> "sDCacheWrite", //  io.dcache.aw.fire
        "sIdle" -> "sDCacheRead", // !io.dcache.aw.fire &&  io.dcache.ar.fire
        "sIdle" -> "sICacheRead", // !io.dcache.aw.fire && !io.dcache.ar.fire &&  io.icache.ar.fire
        "sIdle" -> "sIdle", // !io.dcache.aw.fire && !io.dcache.ar.fire && !io.icache.ar.fire
        // is(sICacheRead)
        "sICacheRead" -> "sIdle", //  (io.nasti.r.fire && io.nasti.r.bits.last)
        "sICacheRead" -> "sICacheRead", // !(io.nasti.r.fire && io.nasti.r.bits.last)
        // is(sDCacheRead)
        "sDCacheRead" -> "sIdle", //  (io.nasti.r.fire && io.nasti.r.bits.last)
        "sDCacheRead" -> "sDCacheRead", // !(io.nasti.r.fire && io.nasti.r.bits.last)
        // is(sDCacheWrite)
        "sDCacheWrite" -> "sDCacheAck", //  (io.dcache.w.fire && io.dcache.w.bits.last)
        "sDCacheWrite" -> "sDCacheWrite", // !(io.dcache.w.fire && io.dcache.w.bits.last)
        // is(sDCacheAck)
        "sDCacheAck" -> "sIdle", //  io.nasti.b.fire
        "sDCacheAck" -> "sDCacheAck" // !io.nasti.b.fire
      )
    )
  }

  private def checkInfo(
    info:        FsmInfoAnnotation,
    states:      Seq[(Int, String)],
    start:       Option[String],
    transitions: Seq[(String, String)]
  ): Unit = {
    // ensure unique state names
    val uniqueNames = states.map(_._2).distinct
    assert(uniqueNames.length == states.length, "We assume unique state names!")

    // check for missing or misnamed states
    val infoStateIndex = info.states.toMap
    states.foreach { case (ii, name) =>
      assert(infoStateIndex.contains(ii), s"State $name ($ii) missing from info annotation!")
      assert(infoStateIndex(ii) == name, s"State #$ii is named ${infoStateIndex(ii)} instead of $name")
    }

    // check for states that were not expected
    assert(states.length == info.states.length, s"More states than expected in info annotation: ${info.states}")

    // check for missing transitions:
    val infoTransitionIndex = info.transitions.map { case (from, to) =>
      s"${infoStateIndex(from)} -> ${infoStateIndex(to)}"
    }.toSet
    transitions.foreach { case (from, to) =>
      val name = s"$from -> $to"
      assert(infoTransitionIndex.contains(name), "missing transition")
    }

    // check for unexpected transition
    val expectedTransitionIndex = transitions.map { case (from, to) => s"$from -> $to" }.toSet
    infoTransitionIndex.toSeq.sorted.foreach { name =>
      assert(expectedTransitionIndex.contains(name), "unexpected transition")
    }
  }
}
