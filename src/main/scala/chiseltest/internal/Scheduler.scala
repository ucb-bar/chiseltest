// Copyright 2018-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.internal

import java.util.concurrent.Semaphore
import scala.annotation.tailrec
import scala.collection.mutable

private class ThreadInfo(
  /** Unique thread id. */
  val id: Int,
  /** Human readable name of the thread */
  val name: String,
  /** Priority derived from the thread [[chiseltest.Region]]. Lower numbers mean higher priority. */
  val priority: Int,
  /** Java thread. */
  var underlying: Option[Thread],
  /** Status of the thread. */
  var status: ThreadStatus,
  /** Semaphore that pauses thread. */
  val semaphore: Semaphore,
  /** An exception from a child thread that needs to be propagated */
  var pendingException: Option[Throwable] = None) {
  def serializeShort(currentStep: Int, activeThreadId: Int): String = {
    val isActive = id == activeThreadId
    val idStr = if (isActive) s"[${id}]" else id.toString
    val pausedStr = ThreadStatus.serialize(status, currentStep)
    idStr + ": " + pausedStr
  }

  override def toString = s"$name ($id)"
}

private sealed trait ThreadStatus {}
private case object ThreadActive extends ThreadStatus
private case class ThreadWaitingForJoin(otherThread: Int) extends ThreadStatus
private case class ThreadWaitingUntil(step: Int) extends ThreadStatus
private case object ThreadTerminating extends ThreadStatus
private case object ThreadFinished extends ThreadStatus
private object ThreadStatus {
  @inline def serialize(status: ThreadStatus, currentStep: Int): String = status match {
    case ThreadActive                      => "R"
    case ThreadWaitingForJoin(otherThread) => s"J($otherThread)"
    case ThreadWaitingUntil(step)          => s"P(${step - currentStep})"
    case ThreadTerminating                 => "T"
    case ThreadFinished                    => "F"
  }
}

private class TerminateSimThreadException extends Exception

/** Information needed by the [[AccessCheck]] class. Should return quickly! */
private trait ThreadInfoProvider {
  def getActiveThreadId:       Int
  def getActiveThreadPriority: Int
  def getStepCount:            Int
  def isParentOf(id: Int, childId: Int): Boolean
}

/** Manages multiple Java threads that all interact with the same simulation and step synchronously. Currently only
  * supports a single global clock that all threads are synchronized to.
  */
private class Scheduler(simulationStep: (Int, Int) => Int) extends ThreadInfoProvider {
  private val EnableDebug:         Boolean = false
  private val DebugThreadSwitches: Boolean = false
  private def debugPrefix = s"$activeThreadId@$currentStep: "
  private def debug(msg: => String): Unit = if (EnableDebug) println(debugPrefix + msg)
  private def debugSwitch(from: Int, to: Int, reason: => String): Unit = if (DebugThreadSwitches) {
    println(
      s"@$currentStep: ${threads(from).serializeShort(currentStep, activeThreadId)} ->" +
        s" ${threads(to).serializeShort(currentStep, activeThreadId)} $reason"
    )
    dbgThreadList(showAlways = true)
  }

  private def dbgThreadList(showAlways: Boolean = false): Unit = if (EnableDebug || showAlways) {
    val msg = threadsInSchedulerOrder.map(_.serializeShort(currentStep, activeThreadId)).mkString(", ")
    println(debugPrefix + "  --> " + msg)
  }

  private val MainThreadId = 0

  /** current active thread */
  private var activeThreadId = MainThreadId
  override def getActiveThreadId:       Int = activeThreadId
  override def getActiveThreadPriority: Int = threads(activeThreadId).priority

  /** all threads */
  private val threads = new mutable.ArrayBuffer[ThreadInfo]()
  threads.addOne(new ThreadInfo(MainThreadId, "main", 0, None, ThreadActive, new Semaphore(0)))

  /** order in which threads are scheduled */
  private val threadOrder = new ThreadOrder
  private def threadsInSchedulerOrder = threadOrder.getOrder.map(threads(_))
  override def isParentOf(id: Int, childId: Int) = threadOrder.isParentOf(id, childId)

  /** Keep track of global simulation time. */
  private var currentStep: Int = 0

  override def getStepCount: Int = currentStep

  @inline private def createThread(name: String, id: Int, runnable: () => Unit): (Thread, Semaphore) = {
    val semaphore = new Semaphore(0)
    val thread = new Thread(
      null,
      () => {
        // BLOCK #1: after being started
        semaphore.acquire() // wait until it is our turn
        try {
          onResumeThread(id) // we might already be asked to terminate when resuming, so run this in the try block
          runnable() // execute user code
          // we need to first wait for all child threads to terminate (only the main thread kills)
          joinThreadsImpl(threadOrder.getChildren(id))
        } catch {
          case _: TerminateSimThreadException => // everything OK, we are just being terminated
          case e @ (_: Exception | _: Error) =>
            debug("Caught exception. Shutting down and propagating exception to parent.")
            // an exception means that we will be terminating
            threads(id).status = ThreadTerminating
            // add exception to parent thread
            val parentId = threadOrder.getParent(id).get
            val parent = threads(parentId)
            assert(
              parent.pendingException.isEmpty,
              s"Parent thread $parent already has an exception! Cannot add ${e.getMessage}"
            )
            parent.pendingException = Some(e)
            // terminate all child threads
            terminateAllChildThreads()
        } finally {
          finishThread(id) // finish thread execution
        }
      },
      name
    )
    thread.start()
    (thread, semaphore)
  }

  /** Must be called by a thread right after it resumes execution. */
  @inline private def onResumeThread(id: Int): Unit = {
    activeThreadId = id
    val threadInfo = threads(id)
    // check if we are requested to terminate
    if (threadInfo.status == ThreadTerminating) {
      throw new TerminateSimThreadException
    }
    // check to see if there is an exception we need to propagate from a child thread
    threadInfo.pendingException match {
      case Some(e) =>
        threadInfo.pendingException = None
        throw e
      case None =>
    }
    threadInfo.status = ThreadActive
  }

  /** Called by every thread right before it is done. */
  private def finishThread(id: Int): Unit = {
    val info = threads(id)
    assert(
      info.status == ThreadActive || info.status == ThreadTerminating,
      s"${info.serializeShort(currentStep, activeThreadId)}"
    )
    debug(s"finishThread(id=$id (${info.name}))")
    threadOrder.finishThread(id)
    info.status = ThreadFinished
    // remove pointer to underlying thread so that it can be garbage collected
    info.underlying = None
    // display thread status for debugging
    dbgThreadList()
    // now that we are done, we need to schedule a new thread, if there are any unfinished threads left
    if (!allThreadsAreFinished) {
      val freeAfterJoin = threadsWithUnblockedJoin
      freeAfterJoin.headOption match {
        case Some(unblockedThread) =>
          // if there is a thread that just got unblocked by us finishing, we want to switch to that
          debugSwitch(id, unblockedThread.id, "finish unblocks join")
          wakeUpThread(unblockedThread)
        case None =>
          // otherwise we make sure a thread waiting on a step will be available and then execute that
          stepSimulationToNearestWait()
          val nextThread = findNextThread()
          debugSwitch(id, nextThread.id, "finish")
          wakeUpThread(nextThread)
      }
    }
  }

  def forkThread(runnable: () => Unit, name: Option[String], priority: Int): SimThreadId = {
    // generate an ID, name and data structure for the new thread
    val id = threads.length
    val fullName = name.getOrElse(s"chiseltest_thread_$id")
    debug(s"forkThread($fullName ($id) P$priority) from ${activeThreadId}")
    // the new thread starts as paused
    val (newJavaThread, newSemaphore) = createThread(fullName, id, runnable)
    threads.addOne(
      new ThreadInfo(id, fullName, priority, Some(newJavaThread), ThreadWaitingUntil(currentStep), newSemaphore)
    )
    threadOrder.addThread(parent = activeThreadId, id = id, priority = priority)
    // potentially yield to the new thread before returning
    yieldForFork()
    new SimThreadId(id)
  }

  @inline private def yieldForFork(): Unit = {
    debug(s"yieldForFork()");
    dbgThreadList()
    // set active thread to paused (we might actually need to resume it since a newly forked thread might have lower priority)
    val originalActiveThreadId = activeThreadId
    val activeThread = threads(originalActiveThreadId)
    activeThread.status = ThreadWaitingUntil(currentStep)
    // find a thread that is ready to run
    val nextThread = findNextThread()
    // if that thread is different from our current thread, perform a switch
    if (nextThread.id != originalActiveThreadId) {
      debugSwitch(activeThreadId, nextThread.id, "fork")
      wakeUpThread(nextThread)
      // BLOCK #5: as part of a fork
      activeThread.semaphore.acquire()
      onResumeThread(originalActiveThreadId)
    } else {
      // we are running again
      activeThread.status = ThreadActive
    }
  }

  /** Suspends the active thread for `cycles` steps and schedules a new one to run. */
  @inline private def yieldForStep(cycles: Int): Unit = {
    debug(s"yieldForStep(cycles = $cycles)"); dbgThreadList()
    // find a thread that is ready to run
    val nextThread = findNextThread()
    // set active thread status to paused
    val originalActiveThreadId = activeThreadId
    val activeThread = threads(originalActiveThreadId)
    activeThread.status = ThreadWaitingUntil(currentStep + cycles)
    // switch threads
    debugSwitch(activeThreadId, nextThread.id, s"yield for step ($cycles)")
    wakeUpThread(nextThread)
    // BLOCK #2: as part of a step
    activeThread.semaphore.acquire()
    onResumeThread(originalActiveThreadId)
  }

  @inline private def wakeUpThread(nextThread: ThreadInfo): Unit = {
    // check thread (for debugging)
    val semaphoreNeedsToBeReleased = nextThread.status match {
      case ThreadActive => throw new RuntimeException(s"Cannot resume active thread! $nextThread")
      case ThreadWaitingForJoin(otherThreadId) =>
        val otherThread = threads(otherThreadId)
        assert(otherThread.status == ThreadFinished, s"Cannot resume thread $nextThread waiting on $otherThread")
        false
      case ThreadFinished => throw new RuntimeException(s"Cannot resume finished thread! $nextThread")
      case ThreadWaitingUntil(target) =>
        assert(target == currentStep, s"Cannot resume thread! $nextThread")
        true
      case ThreadTerminating => // we can only terminate threads that were waiting for a semaphore
        true
    }
    // only threads that are blocked on a step (and not e.g. a join) need their semaphore to be release
    if (semaphoreNeedsToBeReleased) {
      nextThread.semaphore.release()
    }
  }

  @inline private def allThreadsAreFinished: Boolean = threads.forall(_.status == ThreadFinished)

  @inline private def canBeScheduled(status: ThreadStatus): Boolean = status match {
    case ThreadWaitingUntil(step)          => step == currentStep
    case ThreadWaitingForJoin(otherThread) =>
      // check if other thread is finished
      threads(otherThread).status == ThreadFinished
    case _ => false
  }

  /** Determines which thread needs to run next. */
  @inline private def findNextThread(): ThreadInfo = {
    val nextThreadOption = threadsInSchedulerOrder.find(t => canBeScheduled(t.status))
    val nextThread = nextThreadOption.getOrElse {
      debug("Deadlock condition: could not find any thread that can be executed.")
      dbgThreadList()
      throw new RuntimeException("Deadlock!")
    }
    // print debug info and check invariants
    debug(s"  --> nextThreadId = ${nextThread.id}")
    nextThread
  }

  @inline private def threadsWaitingUntil: Iterable[Int] =
    threads.map(_.status).collect { case ThreadWaitingUntil(step) => step }
  @inline private def threadsWithUnblockedJoin: Iterable[ThreadInfo] = threadsInSchedulerOrder.flatMap { t =>
    t.status match {
      case ThreadWaitingForJoin(otherThread) if threads(otherThread).status == ThreadFinished => Some(t)
      case _                                                                                  => None
    }
  }

  /** Steps the currently active thread. Needs to be called in the context of the active thread! */
  def stepThread(cycles: Int): Unit = {
    require(cycles > 0)
    // find all wait cycles
    val waitForSteps = threadsWaitingUntil
    if (waitForSteps.isEmpty) { // all other threads are either finished, or waiting for a join
      doStep(cycles)
    } else {
      val targetStep = currentStep + cycles
      // what is the furthest point in the future that we can step all paused threads?
      val nextWake = waitForSteps.min
      debug(s"stepThread(cycles = $cycles): minPause = ${nextWake - currentStep}");
      dbgThreadList()
      // if all threads are paused for more than we want to step, we do not need to context switch
      if (nextWake > targetStep) {
        doStep(cycles)
      }
      // otherwise we need to potentially first step some other threads
      else {
        // pretend that we are suspended
        val activeThread = threads(activeThreadId)
        activeThread.status = ThreadWaitingUntil(targetStep)
        // perform the biggest step we can
        val stepSize = stepSimulationToNearestWait()
        // yield to the scheduler
        yieldForStep(cycles - stepSize)
      }
    }
  }

  /** Advances the simulation to the closest step that a thread is waiting for.
    * @return
    *   size of the step taken
    */
  private def stepSimulationToNearestWait(): Int = {
    // find all wait cycles
    val waitForSteps = threads.map(_.status).collect { case ThreadWaitingUntil(step) => step }
    // if no thread is waiting, then there is nothing to do
    if (waitForSteps.isEmpty) {
      return 0
    }
    // what is the furthest point in the future that we can step all paused threads?
    val nextWake = waitForSteps.min
    // perform the biggest step we can
    val stepSize = nextWake - currentStep
    if (stepSize > 0) {
      doStep(stepSize)
    }
    stepSize
  }

  /** Performs the simulation step */
  @inline private def doStep(cycles: Int): Unit = {
    debug(s"doStep(cycles = $cycles)")
    val delta = simulationStep(currentStep, cycles)
    currentStep += delta
  }

  def joinThreads(ids: Seq[SimThreadId]): Unit = joinThreadsImpl(ids.map(_.id))

  private def joinThreadsImpl(ids: Seq[Int]): Unit = {
    if (ids.isEmpty) { return }
    debug(s"joinThreads(ids = $ids)")
    // cache ID
    val joiningThreadId = activeThreadId
    // check to make sure we are doing something meaningful
    assert(!ids.contains(joiningThreadId), "cannot join on the active thread!")
    // join all threads that aren't stopped yet
    ids.map(threads(_)).foreach { other =>
      if (other.status != ThreadFinished) {
        // before we block on the join, we need to find another thread to start
        // this might _not_ be the thread we will be joining with!
        stepSimulationToNearestWait()
        val nextThread = findNextThread()
        // we remember which thread we are blocking on
        threads(joiningThreadId).status = ThreadWaitingForJoin(other.id)
        debugSwitch(joiningThreadId, nextThread.id, s"waiting for ${other.id}")
        wakeUpThread(nextThread)
        // BLOCK #3: waiting for other thread to finish if is still alive!
        other.underlying.foreach(t => t.join())
        onResumeThread(joiningThreadId)
      }
    }
  }

  /** Stops all threads bellow the current one from running. */
  private def terminateAllChildThreads(): Unit = {
    // remember the info and all initial child threads
    val terminatingThread = threads(activeThreadId)
    var childThreads = threadOrder.getChildren(terminatingThread.id).toSet

    // check for an early exit
    if (childThreads.isEmpty) {
      return // no child threads ==> nothing to do here
    }

    // show what we are trying to terminate
    debug("terminating: " + childThreads.map(threads(_)).mkString(", "))

    // run until there are no more child threads
    while (childThreads.nonEmpty) {
      // find the first child thread in scheduler order because we want to terminate them bottom up
      val t = threadsInSchedulerOrder.find(t => childThreads(t.id)).get

      // change thread status
      assert(!t.status.isInstanceOf[ThreadWaitingForJoin], s"Cannot terminate thread waiting for join $t")
      t.status = ThreadTerminating

      // schedule thread and wait for control to return
      terminatingThread.status = ThreadWaitingUntil(currentStep)
      wakeUpThread(t)

      // BLOCK #4: waiting to terminate threads
      terminatingThread.semaphore.acquire()
      onResumeThread(terminatingThread.id)

      // update set of child threads
      childThreads = threadOrder.getChildren(terminatingThread.id).toSet
    }
  }

  /** Shuts down the main thread by waiting for all other threads to finish, */
  def finishMainThread(): Unit = {
    assert(activeThreadId == MainThreadId, "TODO: deal with exceptions inside of threads correctly!")
    terminateAllChildThreads()
    finishThread(MainThreadId)
  }
}

/** Maintains a tree of threads and uses it in order to derive in which order threads need to run. */
private class ThreadOrder {
  private class Node(var thread: Int, val parent: Int, val priority: Int, var children: Seq[Node] = null) {
    def isAlive: Boolean = thread > -1
  }
  private val root = new Node(0, parent = -1, priority = 0)
  private val idToNode = mutable.ArrayBuffer[Node](root)
  private var threadOrder: Seq[Int] = Seq()

  /** Maintains a sorted list of all priorities. */
  private var priorities: Seq[Int] = Seq(0)

  /** Returns non-finished threads in the order in which they should be scheduled */
  def getOrder: Seq[Int] = {
    // if the root thread is still alive, but no threads are in the current order
    if (threadOrder.isEmpty && root.thread == 0) { threadOrder = calculateOrder() }
    threadOrder
  }

  /** Returns the parent thread ID if there is a parent. */
  def getParent(id: Int): Option[Int] = {
    val node = idToNode(id)
    if (node.parent > -1) { Some(node.parent) }
    else { None }
  }

  /** Returns true iff `id` is a parent of `childId` */
  @tailrec
  final def isParentOf(id: Int, childId: Int): Boolean = {
    val childNode = idToNode(childId)
    if (childNode.parent == -1) { false }
    else if (childNode.parent == id) { true }
    else { isParentOf(id, childNode.parent) }
  }

  /** Returns ids of all (transitive) child threads */
  def getChildren(thread: Int): Seq[Int] = {
    val todo = mutable.Stack[Node]()
    val result = mutable.ArrayBuffer[Int]()
    todo.push(idToNode(thread))
    while (todo.nonEmpty) {
      val node = todo.pop()
      if (node.children != null) {
        node.children.foreach { child =>
          if (child.isAlive) {
            result.addOne(child.thread)
            todo.push(child)
          }
        }
      }
    }
    result.toSeq
  }

  /** Add a new thread. */
  def addThread(parent: Int, id: Int, priority: Int): Unit = {
    // invalidate order
    threadOrder = Seq()
    // update priorities
    if (!priorities.contains(priority)) {
      priorities = (priorities :+ priority).sorted
    }
    // create new child node
    val childNode = new Node(id, parent, priority)
    assert(idToNode.length == id, "Expected ids to always increase by one...")
    idToNode.addOne(childNode)
    // insert pointer into parent node
    val parentNode = idToNode(parent)
    if (parentNode.children == null) {
      parentNode.children = List(childNode)
    } else {
      parentNode.children = parentNode.children :+ childNode
    }
  }

  /** Marks thread as finished. */
  def finishThread(id: Int): Unit = {
    // invalidate order
    threadOrder = Seq()
    // check to make sure there are no alive children
    val node = idToNode(id)
    if (node.children != null) {
      val lifeChildren = node.children.filter(_.thread > -1)
      assert(
        lifeChildren.isEmpty,
        f"Cannot finish thread $id since some of its children are still alive: ${lifeChildren.map(_.thread)}"
      )
    }
    // mark node as finished
    node.children = null
    node.thread = -1
  }

  private def calculateOrder(): Seq[Int] = {
    assert(root.thread == 0, "We lost the main thread!")
    val order = mutable.ArrayBuffer[(Int, Int)]()
    val todo = mutable.Stack[Node]()
    // we want to run threads from the bottom up (children first) and the older children before the younger children
    todo.push(root)
    while (todo.nonEmpty) {
      val node = todo.pop()
      order.addOne((node.thread, node.priority))
      if (node.children != null) {
        val lifeChildren = node.children.filter(_.thread > -1)
        node.children = lifeChildren
        todo.pushAll(lifeChildren)
      }
    }
    // reverse order
    val reversed = order.toSeq.reverse

    // sort by priority if there is more than one
    if (priorities.length > 1) {
      val sortedByPriority = priorities.flatMap(p => reversed.filter(_._2 == p)).map(_._1)
      sortedByPriority
    } else {
      // strip priority
      reversed.map(_._1)
    }
  }
}
