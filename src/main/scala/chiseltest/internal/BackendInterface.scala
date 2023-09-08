// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chiseltest.Region
import chisel3._
import firrtl2.AnnotationSeq

import scala.collection.mutable

/** Common interface definition for tester backends. Internal API.
  */
trait BackendInterface[T <: Module] {

  /** Runs of tests are wrapped in this, for any special setup/teardown that needs to happen. Takes the test function,
    * which takes the module used as the testing interface. TesterContext setup is done externally.
    *
    * @return
    *   coverage annotations
    *
    * Internal API
    */
  def run(dut: T, testFn: T => Unit): AnnotationSeq

  /** Writes a value to a writable wire. Throws an exception if write is not writable.
    */
  def pokeBits(signal: Data, value: BigInt): Unit

  /** Returns the current value on a wire. Returns the current combinational value (after previous pokes have taken
    * effect).
    */
  def peekBits(signal: Data): BigInt

  /** Sets the timeout of the clock: the number of cycles the clock can advance without some non-nop poke operation.
    * Setting cycles=0 disables the timeout. Setting cycles=1 means every cycle must have some non-nop poke operation.
    * Resets the idle counter associated with the specified clock.
    */
  def setTimeout(signal: Clock, cycles: Int): Unit

  /** Advances the target clock by one cycle.
    */
  def step(signal: Clock, cycles: Int): Unit

  /** Returns the current step, i.e., the number of clock cycles performed by the test so far, excluding any initial
    * reset cycles performed by the chiseltest library at the start of the test.
    */
  def getStepCount(signal: Clock): Long

  def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): SimThreadId

  def doJoin(threads: Seq[SimThreadId], stepAfter: Option[Clock]): Unit

  def doTimescope(contents: () => Unit): Unit

  /** Returns the stack trace elements of parent threads. If currently in the root thread, returns empty. TODO: refactor
    * this, figure out how to do this in a structurally cleaner way
    */
  def getParentTraceElements: Seq[StackTraceElement] = Seq()
}
