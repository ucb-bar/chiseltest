// See LICENSE for license details.

package chisel3.tester

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}

import chisel3._

import scala.collection.mutable

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

  sealed abstract class BaseTimescope {
    def threadOption: Option[TesterThread]
  }

  sealed trait HasOverridingPokes extends BaseTimescope {
    // List of timescopes overriding a signal, including ones closed / reverted on this timestep
    private[tester] val overridingPokes = new mutable.HashMap[Data, mutable.ListBuffer[Timescope]]
    def closedTimestep: Option[Int]
  }
  sealed trait HasParent extends BaseTimescope {
    def parentTimescope: BaseTimescope
    def parentActionId: Int
  }

  // Root timescope, cannot be modified, with no signals poked.
  class RootTimescope extends BaseTimescope with HasOverridingPokes {
    override def threadOption: Option[TesterThread] = None
    override def closedTimestep: Option[Int] = None
  }

  var rootTimescope: Option[RootTimescope] = None  // TODO unify with something else? Or replace w/ rootThread?

  // Timescope that is the root of a thread, contains no signals but can have a parent
  // TODO: should this be a trait of thread?
  case class ThreadRootTimescope(parentTimescope: BaseTimescope,
      openedTimestep: Int,
      parentActionId: Int,
      thread: TesterThread) extends BaseTimescope with HasParent {
    override def threadOption = Some(thread)
  }

  case class PokeRecord(timestep: Int, actionId: Int, value: BigInt, trace: Throwable)

  class Timescope(val parentTimescope: BaseTimescope,
      val openedTimestep: Int,  // timestep this timescope was spawned on
      val parentActionId: Int)  // spawn time in actionId of parent timescope,
                                // from parent's perspective all actions happen instantaneously in this timestep
      extends BaseTimescope with HasOverridingPokes with HasParent {
    var nextActionId: Int = 0  // actionId to be assigned to the next action
    var closedTimestep: Option[Int] = None  // timestep this timescope was closed on, if it is closed

    override def threadOption: Option[TesterThread] = parentTimescope.threadOption

    // All pokes on a signal in this timescope, ordered from first to last
    // TODO: can we get away with just first and most recent?
    private [tester] val pokes = new mutable.HashMap[Data, mutable.ListBuffer[PokeRecord]]  // Latest poke on each data
  }

  private [tester] val pokes = new mutable.HashMap[Data, Timescope]

  object TimescopeUtils {
    // From the current timescope upwards, returns the nearest timescope poking the target signal
    // and the actionId to the child downwards
    def getNearestPoke(signal: Data, timescope: BaseTimescope, actionId: Int): (HasOverridingPokes, Int) = timescope match {
      case timescope: RootTimescope => (timescope, actionId)
      case timescope: Timescope if timescope.pokes.contains(signal) => (timescope, actionId)
      case timescope: HasParent => getNearestPoke(signal, timescope.parentTimescope, timescope.parentActionId)
    }

    /**
     * Returns the linear path from startTimescope (as the first element, inclusive)
     * to destTimescope (as the last element, inclusive)
     * destActionId is the actionId in destTimescope to the next timescope (or the action of interest)
     */
    def getLinearPath(startTimescope: HasOverridingPokes, destTimescope: BaseTimescope, destActionId: Int):
        Seq[(BaseTimescope, Int)] = {
      val prefix: Seq[(BaseTimescope, Int)] = destTimescope match {
        case _ if destTimescope == startTimescope => Seq()
        case destTimescope: HasParent =>
          getLinearPath(startTimescope, destTimescope.parentTimescope, destTimescope.parentActionId)
        case _ => throw new IllegalArgumentException("no path from startTimescope to destTimescope")
      }
      prefix :+ ((destTimescope, destActionId))
    }

    /**
     * Returns the deepest common ancestor timescope from the first element in the linearPath to the last element in
     * the linearPath and the destination, and the actionIds to the next element for the linearPath, and destination.
     */
    def getCommonAncestor(linearPath: Seq[(BaseTimescope, Int)], destTimescope: Timescope, destActionId: Int):
        (HasOverridingPokes, Int, Int) = {
      // TODO: clean up all the asInstanceOf
      val destinationLinearPath = getLinearPath(linearPath.head._1.asInstanceOf[HasOverridingPokes], destTimescope, destActionId)
      val commonPrefix = (linearPath zip destinationLinearPath).takeWhile {
        case ((lpTimescope, _), (dTimescope, _)) => lpTimescope == dTimescope
      }
      val commonAncestor = commonPrefix.last
      (commonAncestor._1._1.asInstanceOf[HasOverridingPokes], commonAncestor._1._2, commonAncestor._2._2)
    }

    /**
     * Returns all the leaf-level overriding pokes, not include the argument timescope (if it has overriding pokes)
     */
    def getLeafOverridingPokes(timescope: HasOverridingPokes, signal: Data): Seq[Timescope] = {
      def innerOverridingPokes(timescope: Timescope): Seq[Timescope] = {
        timescope.overridingPokes.get(signal) match {
          case Some(overridingPokes) => overridingPokes.map(innerOverridingPokes).fold(Seq())(_ ++ _)
          case None => Seq(timescope)
        }
      }

      timescope.overridingPokes.get(signal) match {
        case Some(overridingPokes) => overridingPokes.map(innerOverridingPokes).fold(Seq())(_ ++ _)
        case None => Seq()
      }
    }
  }

  //
  // Threading checking data structures and utilities
  //

  case class PeekRecord(timescope: Timescope, timestep: Int, actionId: Int,
      deps: Map[Data, Option[Timescope]], trace: Throwable)

  // Active peeks on a signal, instantaneous on the current timestep
  // TODO: should this last until the next associated clock edge?
  private[tester] val signalPeeks = new mutable.HashMap[Data, mutable.ListBuffer[PeekRecord]]

  /**
   * Logs a poke operation for later checking.
   * Returns whether to execute it, based on priorities compared to other active pokes.
   */
  def doPoke(signal: Data, value: BigInt, trace: Throwable): Unit = {
    if (currentThread.get.backwardsInTime) {
      throw new TemporalParadox("Need to advance time after moving to a earlier region")
    }

    val timescope = currentThread.get.getTimescope
    // On the first poke, add a link from the previous timescope
    if (!timescope.pokes.contains(signal)) {
      /**
       * Return if the timescope modified signal before childTimescope spawned
       * (or, if childTimescope considers timescope a parent wrt the signal)
       */
      def timescopeContainsThreadSignal(signal: Data,
          timescope: Timescope, childTimescope: ThreadRootTimescope): Boolean = {
        timescope.pokes.get(signal) match {
          case None => false
          case Some(pokeRecords) => pokeRecords.head.actionId < childTimescope.parentActionId
        }
      }

      def getContainingTimescope(signal: Data,
          timescope: BaseTimescope, childTimescope: BaseTimescope): HasOverridingPokes = {
        (timescope, childTimescope) match {
          case (timescope: RootTimescope, _) => timescope
          case (timescope: Timescope, _: Timescope) if timescope.pokes.contains(signal) => timescope
          case (timescope: Timescope, childTimescope: ThreadRootTimescope)
              if timescopeContainsThreadSignal(signal, timescope, childTimescope) => timescope
          // TODO: dedup above
          case (timescope: HasParent, _) => getContainingTimescope(signal, timescope.parentTimescope, timescope)
        }
      }

      getContainingTimescope(signal, timescope.parentTimescope, timescope)
          .overridingPokes.getOrElseUpdate(signal, mutable.ListBuffer[Timescope]()).append(
          timescope)
    }

    // Update the timescope
    timescope.pokes.getOrElseUpdate(signal, mutable.ListBuffer[PokeRecord]()).append(
        PokeRecord(currentTimestep, timescope.nextActionId, value, trace))
    timescope.nextActionId += 1
    pokes.put(signal, timescope)
  }

  /**
   * Logs a peek operation for later checking.
   */
  def doPeek(signal: Data, trace: Throwable): Unit = {
    if (currentThread.get.backwardsInTime) {
      throw new TemporalParadox("Need to advance time after moving to a earlier region")
    }

    val timescope = currentThread.get.getTimescope
    val deps = combinationalPaths.getOrElse(signal, Set(signal)).map { signal =>
      (signal, pokes.get(signal))
    }.toMap

    signalPeeks.getOrElseUpdate(signal, mutable.ListBuffer()) +=
        PeekRecord(timescope, currentTimestep, timescope.nextActionId, deps, trace)
    timescope.nextActionId += 1
  }

  /**
   * Creates a new timescope in the current thread.
   */
  def newTimescope(): Timescope = {
    val newTimescope = currentThread.get.topTimescope match {
      case timescope: Timescope =>
        val newTimescope = new Timescope(timescope, currentTimestep, timescope.nextActionId)
        timescope.nextActionId += 1
        newTimescope
      case timescope => new Timescope(timescope, currentTimestep, 0)
    }
    currentThread.get.topTimescope = newTimescope
    newTimescope
  }

  /**
   * Closes the specified timescope, returns a map of wires to values of any signals that need to be updated.
   */
  def closeTimescope(timescope: Timescope): Map[Data, Option[BigInt]] = {
    if (currentThread.get.backwardsInTime) {
      throw new TemporalParadox("Need to advance time after moving to a earlier region")
    }

    require(timescope eq currentThread.get.getTimescope)

    // Mark timescope as closed
    timescope.closedTimestep = Some(currentTimestep)
    currentThread.get.topTimescope = timescope.parentTimescope

    // Build a revert map of poked signals
    timescope.pokes.map { case (data, _) =>
      def getPreviousPoke(startingTimescope: BaseTimescope, signal: Data): Option[PokeRecord] = {
        // TODO make this less side-effect-like
        startingTimescope match {
          case _: RootTimescope =>
            pokes.remove(signal)
            None
          case startingTimescope: ThreadRootTimescope => getPreviousPoke(startingTimescope.parentTimescope, signal)
          case startingTimescope: Timescope => startingTimescope.pokes.get(signal) match {
            case Some(pokeRecord) =>
              pokes.put(signal, startingTimescope)
              Some(pokeRecord.last)
            case None => getPreviousPoke(startingTimescope.parentTimescope, signal)
          }
        }
      }
      val previousPoke = getPreviousPoke(timescope.parentTimescope, data)
      (data, previousPoke.map(_.value))
    }.toMap
  }

  /**
   * Starts a new timestep, checking if there were any conflicts on the previous timestep (and
   * throwing exceptions if there were).
   */
  def timestep(): Unit = {
    // Check peeks first, before timescope overridingPokes gets purged
    // These are checked by walking up the tree of timescopes, and ensuring the closest poke
    // has not been overridden.
    // Conflicting pokes will be detected by poke checking.
    signalPeeks.toSeq.foreach { case (signal, peeks) =>
      // Check both the signal and combinational sources
      val upstreamSignals = combinationalPaths.getOrElse(signal, Set()) + signal
      // TODO: optimization of peeks within a thread
      peeks.foreach { peekRecord => upstreamSignals.foreach { combSignal =>
        val peekTimescope = peekRecord.timescope
        val (pokeTimescope, peekActionId) = TimescopeUtils.getNearestPoke(combSignal, peekTimescope, peekRecord.actionId)

        //
        // Bulk of the check logic is here
        //
        if (pokeTimescope.threadOption != peekTimescope.threadOption) {
          // If the poke is from another (parent) thread, need to check that it has not changed since the immediate
          // child spawned.
          pokeTimescope.closedTimestep match {
            case Some(closedTimestep) if closedTimestep < currentTimestep =>  // signal cannot be changed by timescope revert
              throw new ThreadOrderDependentException(s"${dataNames(combSignal)} -> ${dataNames(signal)}: Timescope revert")
            case _ =>  // revert is fine on the current timestep:
              // if the peeking thread was just spawned, is should be encapsulated and run immediately
              // if the peeking thread was spawned before, it would have run before the parent run
          }
          pokeTimescope match {
            case timescope: Timescope => if (timescope.pokes(combSignal).last.actionId > peekActionId) {
              throw new ThreadOrderDependentException(s"${dataNames(combSignal)} -> ${dataNames(signal)}: Timescope signal changed, poked after peek")
            }
            case _: RootTimescope =>  // catch issues with override checks below
          }
        }

        // Check override signals
        val pokeToPeekPath = TimescopeUtils.getLinearPath(pokeTimescope, peekTimescope, peekActionId)
        val overridingPokes = TimescopeUtils.getLeafOverridingPokes(pokeTimescope, combSignal)
        overridingPokes.foreach { overridingTimescope =>
          // Use a dummy actionId for the poke, because it should never be used (overridingTimescope should not be on the path to the peek)
          // 0 is used because where it matter, it will fail.
          val (branchTimescope, branchPeekActionId, branchPokeActionId) = TimescopeUtils.getCommonAncestor(pokeToPeekPath, overridingTimescope, 0)
          if (overridingTimescope.threadOption == peekTimescope.threadOption) {  // override in peek timescope
            // Pokes within the same thread as the peek are always fine
          } else if (branchTimescope.threadOption == peekTimescope.threadOption) {  // branched to another thread, from peek thread
            if (branchPeekActionId > branchPokeActionId) {  // thread must have spawned after the peek
              throw new ThreadOrderDependentException(s"${dataNames(combSignal)} -> ${dataNames(signal)}: Poking thread spawned before peek")
            }
          } else {  // branch point was in a parent thread
            if (overridingTimescope.threadOption == branchTimescope.threadOption) {  // ... remaining in the same thread as the parent
              if (branchPeekActionId > branchPokeActionId) {  // thread must have spawned after the peek
                throw new ThreadOrderDependentException(s"${dataNames(combSignal)} -> ${dataNames(signal)}: Poking thread spawned before peek")
              }
            } else {  // ... into another thread
              throw new ThreadOrderDependentException(s"${dataNames(combSignal)} -> ${dataNames(signal)}: Divergent poking / peeking threads")
            }
          }
        }
      } }
    }

    // Clear peeks
    signalPeeks.clear()


    // Timescopes are valid if there is a linear chain from root to latest
    def processTimescope(signal: Data, timescope: HasOverridingPokes): Option[Timescope] = {
      val pokes = timescope.overridingPokes.getOrElse(signal, mutable.ListBuffer())
      if (pokes.exists(_.closedTimestep.isDefined)) {  // remove closed child timescopes
        if (pokes.exists(timescope =>
            timescope.closedTimestep.isEmpty && timescope.openedTimestep < currentTimestep)) {
          throw new ThreadOrderDependentException("Mix of ending timescopes and old timescopes")
        }
        val (endingPokes, nonEndingPokes) = pokes.partition(_.closedTimestep.isDefined)
        endingPokes.foreach { processTimescope(signal, _) }  // Recursively check child closing timescopes
        pokes.clear()
        pokes ++= nonEndingPokes
      }
      if (timescope.closedTimestep.isDefined) {  // if this timescope is closed, ensure there are no children
        if (pokes.nonEmpty) {
          throw new ThreadOrderDependentException(s"Non-enclosed timescopes")
        }
      }
      if (pokes.length > 1) {  // multiple overlapping pokes, is an error
        // STE 0 is pokeBits method, 1 is poke abstraction
        // TODO: better stack trace element detection, chain reporting
        val pokeTraces = pokes.map { _.pokes(signal).last.trace.getStackTrace()(2) }
        throw new ThreadOrderDependentException(s"Overlapping pokes from $pokeTraces")
      } else if (pokes.length == 1) {  // just one poke, report the furthest down that chain
        processTimescope(signal, pokes.head)
      } else {  // all children are closed, this is the latest one in this chain
        timescope.overridingPokes.remove(signal)
        timescope match {
          case timescope: Timescope => Some(timescope)
          case _: RootTimescope => None
        }
      }
    }

    //noinspection ScalaUnusedSymbol
    // Check that there is a clean poke ordering, and build a map of pokes Data -> Timescope
    // TODO: structurally nasty =(, and currently pokeTimescopes is un-used
    val pokeTimescopes = rootTimescope.get.overridingPokes.toMap.map { case (signal, _) =>
      (signal, processTimescope(signal, rootTimescope.get))
    }.collect {
      case (signal, Some(lastTimescope)) => (signal, lastTimescope)
    }
    // TODO: check that lastTimescope signal is actually the one effective?
  }

  def doRegion(region: Region, contents: () => Unit): Unit = {
    require(Region.allRegions.contains(region))
    val thisThread = currentThread.get

    val prevRegion = thisThread.region
    val prevRegionPos = Region.allRegions.indexOf(prevRegion)
    val regionPos = Region.allRegions.indexOf(region)
    require(prevRegionPos >= 0 && regionPos >= 0)
    thisThread.region = region
    if (regionPos > prevRegionPos) {
      scheduler()
      thisThread.waiting.acquire()
    } else if (regionPos < prevRegionPos) {
      // when moving backwards in time, do not invoke the scheduler, instead rely on the caller
      // immediately stepping the clock afterwards, and checking that no testdriver actions are
      // invoked inbetween
      thisThread.backwardsInTime = true
    }
    require(thisThread.region == region)

    contents()

    require(thisThread.region == region)
    thisThread.region = prevRegion
    if (prevRegionPos < regionPos) {
      thisThread.backwardsInTime = true
    } else if (prevRegionPos > regionPos) {
      scheduler()
      thisThread.waiting.acquire()
    }
  }

  protected val interruptedException = new ConcurrentLinkedQueue[Throwable]()
  /**
   * Called when an exception happens inside a thread.
   * Can be used to propagate the exception back up to the main thread.
   * No guarantees are made about the state of the system on an exception.
   *
   * The thread then terminates, and the thread scheduler is invoked to unblock the next thread.
   * The implementation should only record the exception, which is properly handled later.
   */
  protected def onException(e: Throwable) {
    interruptedException.offer(e)
  }

  protected class TesterThread(runnable: () => Unit,
      openedTimestep: Int, parentTimescope: BaseTimescope, parentActionId: Int)
      extends AbstractTesterThread {
    val level: Int = parentTimescope.threadOption match {
      case Some(parentThread) => parentThread.level + 1
      case None => 0
    }
    val waiting = new Semaphore(0)
    var done: Boolean = false

    // Scheduling information
    var joinedOn: Option[TesterThread] = None
    var clockedOn: Option[Clock] = None
    var region: Region = DefaultRegion  // current region
    var backwardsInTime: Boolean = false  // if this thread previously was in a future region,
                                       // and cannot interact with the testdriver until a clock advance

    // TODO: perhaps accessors eg pushTimescope, popTimescope?
    protected val bottomTimescope = ThreadRootTimescope(parentTimescope, openedTimestep, parentActionId, this)
    var topTimescope: BaseTimescope = bottomTimescope  // Currently open timescope in this thread
    def getTimescope: Timescope = {
      require(topTimescope.asInstanceOf[Timescope].closedTimestep.isEmpty)
      topTimescope.asInstanceOf[Timescope]
    }

    //noinspection ConvertExpressionToSAM
    //TODO: code analysis suggests "Convert expression to Single Abstract Method", will that work?
    val thread = new Thread(new Runnable {
      def run() {
        try {
          waiting.acquire()

          timescope {  // TODO breaks consistent level of abstraction
            runnable()
          }

          require(bottomTimescope == topTimescope)  // ensure timescopes unrolled properly
          done = true
          threadFinished(TesterThread.this)
        } catch {
          case _: InterruptedException =>
            // currently used as a signal to kill the thread without doing cleanup
            // (test driver may be left in an inconsistent state, and the test should not continue)
            // TODO: allow other uses for InterruptedException?
          case e @ (_: Exception | _: Error) => onException(e)
        } finally {
          // TODO should there be something similar to done if thread was terminated by an exception
          scheduler()
        }
      }
    })
  }

  protected var currentThread: Option[TesterThread] = None
  protected val driverSemaphore = new Semaphore(0)  // blocks the driver thread while tests are running

  // List of threads in order they are run
  private[tester] val allThreads = new mutable.ArrayBuffer[TesterThread]

  // TODO make this a class and Option[SchedulerState] var that contains currentThread and driverSemaphore?
  object schedulerState {  // temporary scheduler information not persisting between runThreads invocations
    private[tester] var currentRegion: Option[Region] = None
    private[tester] var currentThreadIndex: Int = 0
    // List of clocks that have stepped (run threads blocked on these clocks)
    private[tester] var clocks = mutable.Set[Clock]()
  }

  /**
   * Runs the specified threads, blocking this thread while those are running.
   * Newly formed threads or unblocked join threads will also run.
   * Returns a list of threads run (either passed in, newly forked, or joined) that are waiting
   * on a clock edge.
   *
   * TODO: uses shared schedule state, as an optimization (so that control doesn't need to return
   * to the driver thread between runs. But control still needs to return to the calling thread
   * between timesteps.
   * TODO: this provides a separation which isolates the threading infrastructure from clock
   * control, but there are other structures which accomplish the same thing.
   */
  protected def runThreads(clocks: Set[Clock]): Unit = {
    // TODO validate and order incoming thread order
    require(schedulerState.currentRegion.isEmpty)
    require(schedulerState.currentThreadIndex == 0)
    require(schedulerState.clocks.isEmpty)

    currentTimestep += 1
    schedulerState.clocks ++= clocks

    for (thread <- allThreads) {
      thread.backwardsInTime = false
    }

    for (region <- Region.allRegions) {
      schedulerState.currentRegion = Some(region)
      schedulerState.currentThreadIndex = 0
      
      scheduler()
      driverSemaphore.acquire()

      if (!interruptedException.isEmpty) {
        throw interruptedException.poll()
      }

      require(schedulerState.currentRegion.get == region)
      require(schedulerState.currentThreadIndex == allThreads.size)

      timestep()
    }

    schedulerState.currentRegion = None
    schedulerState.currentThreadIndex = 0
    schedulerState.clocks.clear()
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
    def threadCanRun(thread: TesterThread): Boolean = {
      val blockedByJoin = thread.joinedOn match {
        case None => false
        case Some(joinedThread) if allThreads.contains(joinedThread) => true
        // TODO: move clearing of joinedOn into threadFinished
        case Some(joinedThread) if !allThreads.contains(joinedThread) => thread.joinedOn = None; false
      }
      val blockedByClock = thread.clockedOn match {
        case None => false
        case Some(clock) => !schedulerState.clocks.contains(clock)
      }
      val blockedByRegion = thread.region != schedulerState.currentRegion.get
      !blockedByJoin && !blockedByClock && !blockedByRegion
    }

    while (schedulerState.currentThreadIndex < allThreads.size &&
        !threadCanRun(allThreads(schedulerState.currentThreadIndex))) {
      schedulerState.currentThreadIndex += 1
    }

    if (!interruptedException.isEmpty || schedulerState.currentThreadIndex >= allThreads.size) {
      currentThread = None
      driverSemaphore.release()
    } else {
      val nextThread = allThreads(schedulerState.currentThreadIndex)
      nextThread.clockedOn = None
      currentThread = Some(nextThread)
      nextThread.waiting.release()
    }
  }

  /**
   * Called on thread completion to remove this thread from the running list.
   * Does not terminate the thread, does not schedule the next thread.
   */
  protected def threadFinished(thread: TesterThread): Unit = {
    allThreads -= thread
  }

  def doFork(runnable: () => Unit): TesterThread = {
    val timescope = currentThread.get.getTimescope
    val thisThread = currentThread.get  // locally save this thread
    val newThread = new TesterThread(runnable, currentTimestep, timescope, timescope.nextActionId)
    timescope.nextActionId += 1

    // schedule the new thread to run immediately, then return to this thread
    allThreads.insert(schedulerState.currentThreadIndex, newThread)
    newThread.thread.start()
    scheduler()
    thisThread.waiting.acquire()
    newThread
  }

  def doJoin(joinThread: AbstractTesterThread): Unit = {
    val thisThread = currentThread.get
    val joinThreadTyped = joinThread.asInstanceOf[TesterThread]  // TODO get rid of this, perhaps by making it typesafe
    require(thisThread.level < joinThreadTyped.level)
    thisThread.joinedOn = Some(joinThreadTyped)
    scheduler()
    thisThread.waiting.acquire()
  }
}
