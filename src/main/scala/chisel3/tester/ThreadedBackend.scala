// See LICENSE for license details.

package chisel3.tester

import java.util.concurrent.{Semaphore, SynchronousQueue, TimeUnit}
import scala.collection.mutable

import chisel3._

/** Base trait for backends implementing concurrency by threading. Also implements timescopes.
  */
trait ThreadedBackend {
  //
  // Variable references
  //
  val combinationalPaths: Map[Data, Set[Data]]  // set of combinational Data inputs for each output
  val dataNames: Map[Data, String]  // FIRRTL string name for each Data

  //
  // Common variables
  //

  var currentTimestep: Int = 0  // current simulator timestep, in number of base clock cycles

  //
  // Timescope data structures and utilities
  //

  // Implemented by the simulator interface backend, similar to pokeBits, but without going back into the thread checker
  // Used to revert a poke at the end of a timescope.
  def revertPokeBits(signal: Bits, value: BigInt)

  abstract class BaseTimescope {

  }

  // Root timescope, cannot be modified, with no signals poked.
  class RootTimescope extends BaseTimescope {

  }

  class Timescope(val parentTimescope: BaseTimescope,
      val openedTimestep: Int,  // timestep this timescope was spawned on
      val parentActionId: Int, // spawn time in actionId of parent timescope
      val newThread: Option[TesterThread] = None)
      extends BaseTimescope {
    var nextActionId: Int = 0  // actionId to be assigned to the next action
    var closedTimestep: Option[Int] = None  // timestep this timescope was closed on, if it is closed

    // Latest poke on a signal in this timescope
    abstract class PokeRecord
    case class SignalPokeRecord(timestep: Int, actionId: Int, value: BigInt, trace: Throwable) extends PokeRecord

    val pokes = mutable.HashMap[Data, PokeRecord]()  // Latest poke on each data
  }

  //
  // Threading checking data structures and utilities
  //

  // List of the top Timescope for every Data object.
  // During the testdriver phase, new timescopes may be added that supersede previous ones, but
  // none are to be removed until during the checking phase.
  val activePokes = mutable.HashMap[Data, mutable.ListBuffer[Timescope]]()

  case class PeekRecord(timescope: Timescope, timestep: Int, actionId: Int, trace: Throwable)
  // Active peeks on a signal, instantaneous on the current timestep
  // TODO: should this last until the next associated clock edge?
  protected val signalPeeks = mutable.HashMap[Data, mutable.ListBuffer[PeekRecord]]()



  // combinationalPaths: map of sink Data to all source Data nodes.
  protected class ThreadingChecker(
      combinationalPaths: Map[Data, Set[Data]], dataNames: Map[Data, String]) {



    /**
     * Logs a poke operation for later checking.
     * Returns whether to execute it, based on priorities compared to other active pokes.
     */
    def doPoke(thread: TesterThread, signal: Data, value: BigInt, priority: Int, trace: Throwable): Unit = {
      val timescope = threadTimescopes(thread).last
      val pokeRecord = SignalPokeRecord(timescope, priority, value, nextActionId, trace)
      timescope.pokes.put(signal, pokeRecord)
      nextActionId = nextActionId + 1

      val signalTimescopeStack = signalPokes.getOrElseUpdate(signal, mutable.ListBuffer())
      val signalTimescopeLast = signalTimescopeStack.lastOption

      // Don't stack repeated copies of the same timescope.
      // If the timescope exists but isn't the last on the stack, it will be caught during end-of-timestep checking.
      if (!signalTimescopeLast.isDefined || signalTimescopeLast.get != timescope) {
        signalTimescopeStack.append(timescope)
      }
    }

    /**
     * Logs a peek operation for later checking.
     */
    def doPeek(thread: TesterThread, signal: Data, trace: Throwable): Unit = {
      signalPeeks.getOrElseUpdate(signal, mutable.ListBuffer())
          .append(PeekRecord(thread, nextActionId, trace))
      nextActionId = nextActionId + 1
    }

    /**
     * Creates a new timescope in the specified thread.
     */
    def newTimescope(parentThread: TesterThread, parentTimescope: Option[Timescope]): Timescope = {
      val newTimescope = new Timescope(parentThread, parentTimescope)
      threadTimescopes.getOrElseUpdate(parentThread, mutable.ListBuffer()).append(
          newTimescope)
      newTimescope
    }

    /**
     * Closes the specified timescope, returns a map of wires to values of any signals that need to be updated.
     */
    def closeTimescope(timescope: Timescope): Map[Data, Option[BigInt]] = {
      // Clear timescope from thread first
      val timescopeList = threadTimescopes(timescope.parentThread)
      require(timescopeList.last == timescope)
      timescopeList.trimEnd(1)
      if (timescopeList.isEmpty) {
        threadTimescopes.remove(timescope.parentThread)
      }

      // Clear the timescope from signal pokes, and revert if this is the active poke
      // Return a list of PokeRecords to revert to
      val revertMap = timescope.pokes.map { case (data, pokeRecord) =>
        val dataPokes = signalPokes(data)
        // TODO: better error message when closing a timescope on the same timestep as a malformed operation
        require(dataPokes.count(_ == timescope) == 1)

        val revertRecord = if (dataPokes.size >= 2) {  // potentially something to revert to
          if (dataPokes.last == timescope) {
            Some((data, dataPokes(dataPokes.size - 2)))
          } else {
            None  // this isn't the last timescope, no-op
          }
        } else {  // revert to X
          Some((data, XPokeRecord(timescope.parentThread, nextActionId)))
        }

        signalPokes(data) -= timescope
        if (signalPokes(data).isEmpty) {
          signalPokes.remove(data)
        }

        revertRecord
      }.flatten
      nextActionId = nextActionId + 1

      // Register those pokes as happening on this timestep
      timescope.pokes foreach { case (data, _) =>
        revertPokes.getOrElseUpdate(data, mutable.ListBuffer()).append(timescope)
      }

      revertMap.map { case (data, pokeRecord) => (data, pokeRecord match {
        case signal: SignalPokeRecord => Some(signal.value)
        case _: XPokeRecord => None
      } ) }.toMap
    }

    /**
     * Starts a new timestep, checking if there were any conflicts on the previous timestep (and
     * throwing exceptions if there were).
     */
    def timestep(): Unit = {

    }
  }


  protected class TesterThread(runnable: => Unit,
      parentTimescope: BaseTimescope, parentActionId: Int)
      extends AbstractTesterThread {
    val waiting = new Semaphore(0)
    var done: Boolean = false

    val bottomTimescope = new Timescope(parentTimescope, currentTimestep, parentActionId, Some(this))
    var topTimescope = bottomTimescope  // Currently open timescope in this thread

    val thread = new Thread(new Runnable {
      def run() {
        try {
          waiting.acquire()

          runnable

          require(bottomTimescope == topTimescope)  // ensure timescopes unrolled properly
          done = true
          threadFinished(TesterThread.this)
          scheduler()
        } catch {
          case e: InterruptedException =>
            // currently used as a signal to kill the thread without doing cleanup
            // (testdriver may be left in an inconsistent state, and the test should not continue)
            // TODO: allow other uses for InterruptedException?
            case e @ (_: Exception | _: Error) =>
              onException(e)
        }
      }
    })
  }

  protected var currentThread: Option[TesterThread] = None
  protected val driverSemaphore = new Semaphore(0)  // blocks runThreads() while it's running

  // TODO: does this need to be replaced with concurrent data structures?
  protected val activeThreads = mutable.ArrayBuffer[TesterThread]()  // list of threads scheduled for sequential execution
  protected val blockedThreads = mutable.HashMap[Clock, Seq[TesterThread]]()  // threads blocking on a clock edge
  protected val joinedThreads = mutable.HashMap[TesterThread, Seq[TesterThread]]()  // threads blocking on another thread
  protected val allThreads = mutable.ArrayBuffer[TesterThread]()  // list of all threads

  /**
   * Runs the specified threads, blocking this thread while those are running.
   * Newly formed threads or unblocked join threads will also run.
   *
   * Prior to this call: caller should remove those threads from the blockedThread list.
   * TODO: does this interface suck?
   *
   * Updates internal thread queue data structures. Exceptions will also be queued through onException() calls.
   * TODO: can (should?) this provide a more functional interface? eg returning what threads are blocking on?
   */
  protected def runThreads(threads: Seq[TesterThread]) {
    activeThreads ++= threads
    scheduler()
    driverSemaphore.acquire()
  }

  /**
   * Invokes the thread scheduler, which should be done anytime a thread needs to pass time.
   * Prior to this call: caller should add itself to the blocked / joined threads list
   * (unless terminating).
   * After this call: caller should block on its semaphore (unless terminating). currentThread
   * will no longer be valid.
   *
   * Unblocks the next thread to be run, possibly also also stepping time via advanceTime().
   * When there are no more active threads, unblocks the driver thread via driverSemaphore.
   */
  protected def scheduler() {
    if (!activeThreads.isEmpty) {
      val nextThread = activeThreads.head
      currentThread = Some(nextThread)
      activeThreads.trimStart(1)
      nextThread.waiting.release()
    } else {
      currentThread = None
      driverSemaphore.release()
    }
  }

  /**
   * Called when an exception happens inside a thread.
   * Can be used to propagate the exception back up to the main thread.
   * No guarantees are made about the state of the system on an exception.
   *
   * The thread then terminates, and the thread scheduler is invoked to unblock the next thread.
   * The implementation should only record the exception, which is properly handled later.
   */
  protected def onException(e: Throwable)

  /**
   * Called on thread completion to remove this thread from the running list.
   * Does not terminate the thread, does not schedule the next thread.
   */
  protected def threadFinished(thread: TesterThread) {
    allThreads -= thread
    joinedThreads.remove(thread) match {
      case Some(testerThreads) => activeThreads ++= testerThreads
      case None =>
    }
  }

  def fork(runnable: => Unit): TesterThread = {
    val newThread = new TesterThread(runnable, nextActionId,
        currentThread.map(_.parents).toSet.flatten ++ currentThread.toSet)
    allThreads += newThread
    activeThreads += newThread
    newThread.thread.start()
    newThread
  }

  def join(thread: AbstractTesterThread) = {
    val thisThread = currentThread.get
    val threadTyped = thread.asInstanceOf[TesterThread]  // TODO get rid of this, perhaps by making it typesafe
    if (!threadTyped.done) {
      joinedThreads.put(threadTyped, joinedThreads.getOrElseUpdate(threadTyped, Seq()) :+ thisThread)
      scheduler()
      thisThread.waiting.acquire()
    }
  }
}
