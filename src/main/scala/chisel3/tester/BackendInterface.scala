// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.experimental.MultiIOModule
import org.scalatest.exceptions.TestFailedException

import scala.collection.mutable

class ThreadOrderDependentException(message: String) extends Exception(message)
class TimeoutException(message: String) extends Exception(message)
class FailedExpectException(message: String, val failedCodeStackDepth: Int) extends Exception(message)

// when interfacing with the testdriver before stepping the clock after moving to an earlier region
class TemporalParadox(message: String) extends Exception(message)

trait AbstractTesterThread

/** Base class for regions, akin to Verilog regions for ordering events between threads within the same timestep.
  * order is the order regions run in, with 0 being the default, and incrementing regions running later.
  * TODO: have a more extensible ordering than ints.
  */
sealed class Region {
  protected def getPos(): Int = {
    val pos = Region.allRegions.indexOf(this)
    require(pos >= 0)
    pos
  }

  def isBefore(other: Region): Boolean = this.getPos < other.getPos
  def isAfter(other: Region): Boolean = this.getPos > other.getPos
  def isEqual(other: Region): Boolean = this.getPos == other.getPos
}

object Region {
  val default = TestdriverMain
  val allRegions = Seq(default, Monitor)
}

// Testdriver starts in this. Not to be specified in user code
object TestdriverMain extends Region
object Monitor extends Region


class TesterThreadList(protected val elts: Seq[AbstractTesterThread]) {
  def toSeq(): Seq[AbstractTesterThread] = elts

  def join() {
    Context().backend.doJoin(elts, None)
  }

  def joinAndStep(clock: Clock) {
    Context().backend.doJoin(elts, Some(clock))
  }

  def ++(others: TesterThreadList): TesterThreadList = {
    new TesterThreadList(elts ++ others.elts)
  }

  val fork: ForkBuilder = new ForkBuilder(None, None, elts)
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

  /**
   * Sets the timeout of the clock: the number of cycles the clock can advance without
   * some non-nop poke operation.
   * Setting cycles=0 disables the timeout.
   * Setting cycles=1 means every cycle must have some non-nop poke operation.
   * Resets the idle counter associated with the specified clock.
   */
  def setTimeout(signal: Clock, cycles: Int): Unit

  /** Advances the target clock by one cycle.
    */
  def step(signal: Clock, cycles: Int): Unit

  def doFork(runnable: () => Unit, name: Option[String], region: Option[Region]): AbstractTesterThread

  def doJoin(threads: Seq[AbstractTesterThread], stepAfter: Option[Clock]): Unit

  def doTimescope(contents: () => Unit): Unit

  /** Returns the stack trace elements of parent threads. If currently in the root thread, returns
    * empty.
    * TODO: refactor this, figure out how to do this in a structurally cleaner way
    */
  def getParentTraceElements: Seq[StackTraceElement] = Seq()

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

  protected val batchedFailures: mutable.ArrayBuffer[Exception] = new mutable.ArrayBuffer

  def topFileName: Option[String]

  val useTestFailedException: Boolean = false

  /** Logs a tester failure at this point.
    * Failures queued until the next checkpoint.
    */
  def testerFail(msg: String): Unit = {
    batchedFailures += (
            if(useTestFailedException) {
              new TestFailedException(msg, failedCodeStackDepth = 4)
            }
            else {
              new FailedExpectException(msg, 4)
            })
  }

  protected def getExpectDetailedTrace(trace: Seq[StackTraceElement], inFile: String): String = {
    val fullTrace = Context().backend.getParentTraceElements ++ trace

    // In the threading case, this needs to be overridden to trace through parent threads
    val lineNumbers = fullTrace.collect {
      case ste if ste.getFileName == inFile => ste.getLineNumber
    }.mkString(", ")
    if (lineNumbers.isEmpty) {
      s" (no lines in $inFile)"
    } else {
      s" (lines in $inFile: $lineNumbers)"
    }
  }

  /** Expect a specific value on a wire, calling testerFail if the expectation isn't met.
    * Failures queued until the next checkpoint.
    */
  def testerExpect(expected: Any, actual: Any, signal: String, msg: Option[String]): Unit = {
    if (expected != actual) {
      val appendMsg = msg match {
        case Some(_) => s": $msg"
        case _ => ""
      }

      val trace = new Throwable
      val expectStackDepth = trace.getStackTrace.indexWhere(ste =>
        ste.getClassName == "chisel3.tester.package$testableData" && ste.getMethodName == "expect")
      require(expectStackDepth != -1,
        s"Failed to find expect in stack trace:\r\n${trace.getStackTrace.mkString("\r\n")}")

      val trimmedTrace = trace.getStackTrace.drop(expectStackDepth + 2)
      val detailedTrace = topFileName.map(getExpectDetailedTrace(trimmedTrace.toSeq, _)).getOrElse("")

      val message = s"$signal=$actual did not equal expected=$expected$appendMsg$detailedTrace"
      val stackIndex = expectStackDepth + 1
      batchedFailures += (
              if(useTestFailedException) {
                new TestFailedException(message, failedCodeStackDepth = stackIndex)
              }
              else {
                new FailedExpectException(message, stackIndex)
              })
    }
  }

  /** If there are any failures, reports them and end the test now.
    */
  def checkpoint(): Unit = {
    // TODO: report multiple exceptions simultaneously
    for (failure <- batchedFailures) {
      throw failure
    }
  }

}
