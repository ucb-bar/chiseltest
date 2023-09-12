// Copyright 2018-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.internal

import chisel3.{Clock, Data}
import chiseltest.{ChiselAssertionError, StopException, TimeoutException, UnpeekableException}
import chiseltest.simulator.{SimulatorContext, StepInterrupted, StepOk}

/** Regulates I/O access from different threads. Keeps track of timeout. */
private class AccessCheck(design: DesignInfo, tester: SimulatorContext) {
  private var timeout:    Int = SimController.DefaultTimeout
  private var idleCycles: Int = 0
  def pokeBits(signal: Data, value: BigInt): Unit = {
    val name = design.resolveName(signal)
    tester.poke(name, value)
    idleCycles = 0
  }

  def peekBits(signal: Data): BigInt = {
    val name = design
      .getName(signal)
      .getOrElse(
        throw new UnpeekableException(
          s"Signal $signal not found. Perhaps you're peeking a non-IO signal.\n  If so, consider using the chiseltest.experimental.expose API."
        )
      )
    tester.peek(name)
  }

  def setTimeout(signal: Clock, cycles: Int): Unit = {
    require(signal == design.clock, "timeout currently only supports master clock")
    require(cycles >= 0, s"Negative timeout $cycles is not supported! Use 0 to disable the timeout.")
    timeout = cycles
    idleCycles = 0
  }

  /** Performs a step on the actual simulation (as opposed to a "virtual" thread step) */
  def simulationStep(cycles: Int): Int = {
    // throw any available exceptions before stepping
    Context().env.checkpoint()
    val delta = if (timeout == 0) cycles else Seq(cycles, timeout - idleCycles).min
    tester.step(delta) match {
      case StepOk =>
        // update and check timeout
        idleCycles += delta
        if (timeout > 0 && idleCycles == timeout) {
          throw new TimeoutException(s"timeout on ${design.clock} at $timeout idle cycles")
        }
        delta
      case StepInterrupted(after, true, _) =>
        val msg = s"An assertion in ${design.name} failed.\n" +
          "Please consult the standard output for more details."
        throw new ChiselAssertionError(msg, cycles + after)
      case StepInterrupted(after, false, _) =>
        val msg = s"A stop() statement was triggered in ${design.name}."
        throw new StopException(msg, cycles + after)
    }
  }
}
