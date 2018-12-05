// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.experimental.MultiIOModule
import firrtl.ExecutionOptionsManager

class ThreadOrderDependentException(message: String) extends Exception(message)
class SignalOverwriteException(message: String) extends Exception(message)

trait AbstractTesterThread {

}

class TesterThreadList(protected val elts: Seq[AbstractTesterThread]) {
  def toSeq(): Seq[AbstractTesterThread] = elts

  def join() {
    elts foreach { thread => Context().backend.doJoin(thread) }
  }

  def ++(others: TesterThreadList): TesterThreadList = {
    new TesterThreadList(elts ++ others.elts)
  }

  def fork(runnable: => Unit): TesterThreadList = {
    new TesterThreadList(elts :+ Context().backend.doFork(() => runnable))
  }
}

/** Common interface definition for tester backends. Internal API.
  */
trait BackendInterface {
  /** Writes a value to a writable wire.
    * Throws an exception if write is not writable.
    */
  def pokeBits(signal: Bits, value: BigInt): Unit

  /** Returns the current value on a wire.
    * If stale is true, returns the current combinational value (after previous pokes have taken effect).
    * If stale is false, returns the value at the beginning of the current cycle.
    */
  def peekBits(signal: Bits, stale: Boolean): BigInt

  def expectBits(signal: Bits, value: BigInt, message: Option[String], stale: Boolean): Unit

  /** Advances the target clock by one cycle.
    */
  def step(signal: Clock, cycles: Int): Unit

  def doFork(runnable: () => Unit): AbstractTesterThread

  def doJoin(thread: AbstractTesterThread): Unit

  def doTimescope(contents: () => Unit): Unit
}

/** Backend associated with a particular circuit, and can run tests
  */
trait BackendInstance[T <: MultiIOModule] extends BackendInterface {
  /** Runs of tests are wrapped in this, for any special setup/teardown that needs to happen.
    * Takes the test function, which takes the module used as the testing interface.
    * TesterContext setup is done externally.
    *
    * Internal API
    */
  def run(testFn: T => Unit): Unit
}

/** Interface into the testing environment, like ScalaTest
  */
trait TestEnvInterface {
  /** Logs a tester failure at this point.
    * Failures queued until the next checkpoint.
    */
  def testerFail(msg: String): Unit
  /** Expect a specific value on a wire, calling testerFail if the expectation isn't met.
    * Failures queued until the next checkpoint.
    */
  def testerExpect(expected: Any, actual: Any, signal: String, msg: Option[String]): Unit
  /** If there are any failures, reports them and end the test now.
    */
  def checkpoint()
}
