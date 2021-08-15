// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3._
import chiseltest.{
  ClockResolutionException,
  ClockTimeoutNotImplementedException,
  PeekClockImplementedException,
  PokeClockImplementedException,
  Region,
  ThreadNotImplementedException
}

import scala.collection.mutable

/** Common interface definition for tester backends. Internal API.
  */
trait BackendInterface {

  // Basic API should be implemented by all backend.
  /** Writes a value to a writable wire.
    * Throws an exception if write is not writable.
    */
  def pokeBits(signal: Data, value: BigInt): Unit

  /** Returns the current value on a wire.
    * If stale is true, returns the current combinational value (after previous pokes have taken effect).
    * If stale is false, returns the value at the beginning of the current cycle.
    */
  def peekBits(signal: Data, stale: Boolean): BigInt

  /** Advances the target clock by one cycle.
    */
  def step(signal: Clock, cycles: Int): Unit

  def expectBits(
    signal:  Data,
    value:   BigInt,
    message: Option[String],
    decode:  Option[BigInt => String],
    stale:   Boolean
  ): Unit

  // Optional API
  /** Writes a value to a clock.
    *
    * This is not supported by most of cycle-based simulators.
    */
  def pokeClock(signal: Clock, value: Boolean): Unit = {
    throw new PokeClockImplementedException(s"poke to clock not implemented in ${getClass.getName}")
  }

  /** Read the value of a clock.
    *
    * This is not supported by most of cycle-based simulators.
    */
  def peekClock(signal: Clock): Boolean = {
    throw new PeekClockImplementedException(s"peek to clock not implemented in ${getClass.getName}")
  }

  /** Sets the timeout of the clock: the number of cycles the clock can advance without
    * some non-nop poke operation.
    * Setting cycles=0 disables the timeout.
    * Setting cycles=1 means every cycle must have some non-nop poke operation.
    * Resets the idle counter associated with the specified clock.
    *
    * TODO:
    *   consider deprecating This API.
    *   This consumes too much overhead to maintain a timeout function,
    *   a better solution is adding Annotation and FIRRTL pass to add the timeout circuit,
    *   let simulator to handle timeout.
    */
  def setTimeout(signal: Clock, cycles: Int): Unit = {
    throw new ClockTimeoutNotImplementedException(s"setTimeout not implemented in ${getClass.getName}")
  }

  // Threading API
  def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): AbstractTesterThread = {
    throw new ThreadNotImplementedException(s"fork/join not implemented in ${getClass.getName}")
  }

  def doJoin(threads: Seq[AbstractTesterThread], stepAfter: Option[Clock]): Unit = {
    throw new ThreadNotImplementedException(s"fork/join not implemented in ${getClass.getName}")
  }

  def doTimescope(contents: () => Unit): Unit = {
    throw new ThreadNotImplementedException(s"timescope not implemented in ${getClass.getName}")
  }

  /** Returns the stack trace elements of parent threads. If currently in the root thread, returns
    * empty.
    * TODO: refactor this, figure out how to do this in a structurally cleaner way
    */
  def getParentTraceElements: Seq[StackTraceElement] = Seq()

  // Circuit introspection functionality
  /** Returns set of clocks associated with sources of the signal,
    *
    * TODO:
    *   consider deprecating This API.
    *   this API can achieved by FIRRTL transform, not a runtime tester.
    */
  def getSourceClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  /** Returns set of clocks associated with sinks of the signal
    *
    * TODO:
    *   consider deprecating This API.
    *   this API can achieved by FIRRTL transform, not a runtime tester.
    */
  def getSinkClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  // Test Instance State
  protected val testMap = mutable.HashMap[Any, Any]()

  /** Sets the value associated with a key in a per-test map.
    */
  def setVar(key: Any, value: Any): Unit = {
    testMap.put(key, value)
  }

  /** Returns the value associated with the key in a per-test map.
    */
  def getVar(key: Any): Option[Any] = {
    testMap.get(key)
  }
}
