// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends

import chiseltest.simulator.SimulatorContext

/** Represents the inputs and starting state needed to re-create an execution trace. */
private case class Trace(
  inputs:  Seq[Seq[(String, BigInt)]],
  regInit: Seq[(String, BigInt)],
  memInit: Seq[(String, Seq[BigInt])])

private object Trace {

  /** Currently this only works with treadle since other simulators do not allow poking state. */
  def replayOnSim(trace: Trace, dut: SimulatorContext): Unit = {
    // initialize
    trace.regInit.foreach { case (name, value) => dut.poke(name, value) }
    trace.memInit.foreach { case (name, values) =>
      values.zipWithIndex.foreach { case (value, addr) =>
        dut.pokeMemory(name, addr, value)
      }
    }

    // run
    trace.inputs.foreach { inputs =>
      inputs.foreach { case (name, value) => dut.poke(name, value) }
      dut.step()
    }

    dut.finish()

    // TODO: check that the right assertion was hit!
  }
}
