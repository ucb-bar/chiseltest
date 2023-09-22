// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import chisel3._
import chiseltest._
import chiseltest.coverage.circuits.FifoRegister
import firrtl2._
import firrtl2.options.Dependency
import firrtl2.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class FsmCoverageTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("FsmCoverage")

  it should "accurately count the states and transitions" in {
    val r = runTest()

    val data = FsmCoverage.processCoverage(r)
    assert(data.length == 1, "There is exactly one FSM in the design!")

    val fsm = data.head
    assert(fsm.name == "FifoRegister.stateReg")
    assert(fsm.states.map(_._1) == List("Empty", "Full"))
    assert(
      fsm.transitions.map(_._1) == List("Empty" -> "Empty", "Empty" -> "Full", "Full" -> "Empty", "Full" -> "Full")
    )

    val totalCycles = 15 // excludes single reset cycle in the beginning
    assert(fsm.states.map(_._2).sum == totalCycles, "We are in exactly one state each cycle!")
    assert(
      fsm.transitions.map(_._2).sum == totalCycles - 1,
      "We take exactly one transition every cycle besides the first one"
    )

    val t = fsm.transitions.toMap
    assert(t("Empty" -> "Empty") == 6)
    assert(t("Empty" -> "Full") == 4)
    assert(t("Full" -> "Empty") == 3)
    assert(t("Full" -> "Full") == 1)
  }

  private def runTest(): AnnotationSeq = {
    val rand = new scala.util.Random(0)
    val r = test(new FifoRegister(8)).withAnnotations(FsmCoverage.annotations ++ Seq(WriteVcdAnnotation)) { dut =>
      (0 until 4).foreach { _ =>
        // push until full
        while (dut.io.enq.ready.peek().litToBoolean) {
          dut.io.enq.bits.poke(BigInt(8, rand).U)
          val skip = rand.nextBoolean()
          dut.io.enq.valid.poke((!skip).B)
          dut.io.deq.ready.poke(false.B)
          dut.clock.step()
        }

        // pop until empty
        while (dut.io.deq.valid.peek().litToBoolean) {
          dut.io.enq.valid.poke(false.B)
          val skip = rand.nextBoolean()
          dut.io.deq.ready.poke((!skip).B)
          dut.clock.step()
        }
      }
    }
    r.getAnnotationSeq
  }
}

class FsmCoverageInstrumentationTest extends AnyFlatSpec with CompilerTest {
  behavior.of("FsmCoverage Instrumentation")

  override protected def annos = Seq(RunFirrtlTransformAnnotation(Dependency(FsmCoveragePass)))

  it should "add cover statements" in {
    val (result, rAnnos) = compile(new FifoRegister(8), "low")
    // println(result)
    val l = result.split('\n').map(_.trim).map(_.split('@').head.trim)

    // we expect six custom cover points (2 states, 4 transitions)
    assert(l.contains("""cover(clock, eq(stateReg, UInt<1>("h0")), not(reset), "") : stateReg_Empty"""))
    assert(l.contains("""cover(clock, eq(stateReg, UInt<1>("h1")), not(reset), "") : stateReg_Full"""))
    assert(
      l.contains(
        """cover(clock, and(eq(stateReg_prev, UInt<1>("h0")), eq(stateReg, UInt<1>("h1"))), stateReg_t_valid, "") : stateReg_Empty_to_Full"""
      )
    )
    val coverCount = l.count(_.contains("cover("))
    assert(coverCount == 6)

    // we should have 1 coverage annotation for 2 + 4 (cover) + 1 (stateReg) targets
    val a = rAnnos.collect { case a: FsmCoverageAnnotation => a }
    assert(a.size == 1)
    assert(a.head.targets.length == (2 + 4 + 1))
  }

}
