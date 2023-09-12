// Copyright 2018-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.internal

import chisel3.experimental.Direction
import chisel3.reflect.DataMirror
import chisel3.{Clock, Data}
import chiseltest.{
  ChiselAssertionError,
  StopException,
  ThreadOrderDependentException,
  TimeoutException,
  UnpeekableException
}
import chiseltest.simulator.{SimulatorContext, StepInterrupted, StepOk}

import scala.collection.mutable

/** Meta data about a signal that can be peeked or poked. */
private class SignalInfo(
  /** Name used by the simulator. */
  val name: String,
  /** Unique ID. */
  val id: Int,
  /** Indicates that the signal cannot be poked. */
  val readOnly: Boolean,
  /** Value of the last poke. */
  var lastPokeValue: BigInt,
  /** Time of the last poke. */
  var lastPokeAt: Int = -1,
  /** Id of the thread that performed the last poke. */
  var lastPokeFrom: Int = -1) {}

/** Regulates I/O access from different threads. Keeps track of timeout. */
private class AccessCheck(design: DesignInfo, tester: SimulatorContext) {
  private var timeout:    Int = SimController.DefaultTimeout
  private var idleCycles: Int = 0
  private val signals = new mutable.HashMap[Data, SignalInfo]()
  private def lookupSignal(signal: Data): SignalInfo = signals.getOrElseUpdate(
    signal, {
      val readOnly = DataMirror.directionOf(signal) != Direction.Input
      val name = design.resolveName(signal)
      val value = tester.peek(name)
      val id = signals.size
      new SignalInfo(name, id, readOnly, value)
    }
  )

  def pokeBits(threadInfo: ThreadInfoProvider, signal: Data, value: BigInt): Unit = {
    val activeThreadId = threadInfo.getActiveThreadId
    val stepCount = threadInfo.getStepCount.toInt
    val info = lookupSignal(signal)
    assert(!info.readOnly, "can only poke input! This should have been detected earlier.")
    // check for conflicting pokes
    if (info.lastPokeAt == stepCount && info.lastPokeFrom != activeThreadId) {
      throw new ThreadOrderDependentException("Conflicting pokes!") // TODO: better message
    }

    // check to see if the same value is already applied
    if (info.lastPokeValue == value) {
      // nothing to do!
    } else {
      tester.poke(info.name, value)
      info.lastPokeValue = value
      // only reset timeout if the poke has an effect
      idleCycles = 0
    }
    // record poke
    info.lastPokeFrom = activeThreadId
    info.lastPokeAt = stepCount
  }

  def peekBits(info: ThreadInfoProvider, signal: Data): BigInt = {
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
