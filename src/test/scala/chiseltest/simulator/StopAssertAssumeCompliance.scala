// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import org.scalatest.Tag

/** Ensures that the `step` function works correctly, no or one clock. */
abstract class StopAssertAssumeCompliance(sim: Simulator, tag: Tag = DefaultTag) extends ComplianceTest(sim, tag) {
  private val src =
    """circuit test:
      |  module child:
      |    input clock: Clock
      |    input stop0En: UInt<1>
      |    input stop1En: UInt<1>
      |    input stop2En: UInt<1>
      |    input stop3En: UInt<1>
      |
      |    stop(clock, stop0En, 0) : stop0
      |    stop(clock, stop1En, 1) : stop1
      |    assert(clock, UInt(0), stop2En, "") : stop2
      |    assume(clock, UInt(0), stop3En, "") : stop3
      |
      |  module test:
      |    input clock: Clock
      |    input stop0En: UInt<1>
      |    input stop1En: UInt<1>
      |    input stop2En: UInt<1>
      |    input stop3En: UInt<1>
      |    input selParent: UInt<1>
      |    input selChild0: UInt<1>
      |    input selChild1: UInt<1>
      |
      |    when selParent:
      |      stop(clock, stop0En, 0) : stop0
      |      stop(clock, stop1En, 1) : stop1
      |      assert(clock, UInt(0), stop2En, "") : stop2
      |      assume(clock, UInt(0), stop3En, "") : stop3
      |
      |    inst c0 of child
      |    c0.clock <= clock
      |    c0.stop0En <= and(selChild0, stop0En)
      |    c0.stop1En <= and(selChild0, stop1En)
      |    c0.stop2En <= and(selChild0, stop2En)
      |    c0.stop3En <= and(selChild0, stop3En)
      |
      |    inst c1 of child
      |    c1.clock <= clock
      |    c1.stop0En <= and(selChild1, stop0En)
      |    c1.stop1En <= and(selChild1, stop1En)
      |    c1.stop2En <= and(selChild1, stop2En)
      |    c1.stop3En <= and(selChild1, stop3En)
      |""".stripMargin

  private def start(withVCD: Boolean = false): SimulatorContext = {
    val annos = if(withVCD) Seq(WriteVcdAnnotation) else Seq()
    val dut = load(src, annos)
    // set every input to zero by default
    dut.poke("stop0En", 0)
    dut.poke("stop1En", 0)
    dut.poke("stop2En", 0)
    dut.poke("stop3En", 0)
    dut.poke("selParent", 0)
    dut.poke("selChild0", 0)
    dut.poke("selChild1", 0)
    assert(dut.step() == StepOk)
    dut
  }

  it should "not throw an exception if the stop is only triggered combinatorially" taggedAs tag in {
    val dut = start()
    dut.poke("selParent", 1)

    (0 until 4).foreach { ii =>
      dut.poke(s"stop${ii}En", 1)
      // no step
      dut.poke(s"stop${ii}En", 0)
      assert(dut.step() == StepOk)
    }
    dut.finish()
  }

  it should "throw a StopException when encountering a stop during a step" taggedAs tag in {
    val dut = start(true)
    dut.poke("selParent", 1) // we are only testing the parent module

    dut.poke("stop0En", 1)
    dut.poke("stop1En", 1)
    dut.poke("stop2En", 1)
    dut.poke("stop3En", 1)
    val r0 = dut.step()
    assert(r0.asInstanceOf[StepInterrupted].after == 1)
    assert(r0.asInstanceOf[StepInterrupted].isFailure)
    if(sim == TreadleSimulator) { // only treadle will report the exact statements that were triggered
      assert(r0.asInstanceOf[StepInterrupted].sources == List("stop0", "stop1", "stop2", "stop3"))
    }

    dut.poke("stop0En", 0)
    dut.poke("stop1En", 0)
    dut.poke("stop2En", 0)
    dut.poke("stop3En", 0)
    assert(dut.step() == StepOk)

    // only a stop(0) is not considered a failure
    dut.poke("stop0En", 1)
    val r1 = dut.step()
    assert(r1.asInstanceOf[StepInterrupted].after == 1)
    assert(!r1.asInstanceOf[StepInterrupted].isFailure)
    if(sim == TreadleSimulator) { // only treadle will report the exact statements that were triggered
      assert(r1.asInstanceOf[StepInterrupted].sources == List("stop0"))
    }

    dut.finish()
  }

  it should "throw a StopException when encountering a stop during a step in a child" taggedAs tag in {
    val dut = start(true)
    dut.poke("selChild0", 1) // we are only testing the parent module

    dut.poke("stop0En", 1)
    dut.poke("stop1En", 1)
    dut.poke("stop2En", 1)
    dut.poke("stop3En", 1)
    val r0 = dut.step()
    assert(r0.asInstanceOf[StepInterrupted].after == 1)
    assert(r0.asInstanceOf[StepInterrupted].isFailure)
    if(sim == TreadleSimulator) { // only treadle will report the exact statements that were triggered
      assert(r0.asInstanceOf[StepInterrupted].sources == List("c0.stop0", "c0.stop1", "c0.stop2", "c0.stop3"))
    }

    dut.poke("stop0En", 0)
    dut.poke("stop1En", 0)
    dut.poke("stop2En", 0)
    dut.poke("stop3En", 0)
    assert(dut.step() == StepOk)

    // only a stop(0) is not considered a failure
    dut.poke("stop0En", 1)
    dut.poke("stop1En", 0)
    dut.poke("stop2En", 0)
    dut.poke("stop3En", 0)
    val r1 = dut.step()
    assert(r1.asInstanceOf[StepInterrupted].after == 1)
    assert(!r1.asInstanceOf[StepInterrupted].isFailure)
    if(sim == TreadleSimulator) { // only treadle will report the exact statements that were triggered
      assert(r1.asInstanceOf[StepInterrupted].sources == List("c0.stop0"))
    }

    dut.finish()
  }

}
