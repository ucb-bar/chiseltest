// See LICENSE for license details.

package chiseltest.backends

import chisel3._
import chiseltest.internal._
import chiseltest.{Region, TimeoutException}
import firrtl.AnnotationSeq
import logger.LazyLogging
import treadle.TreadleTesterAnnotation

import scala.collection.mutable

// TODO: is Seq[CombinationalPath] the right API here? It's unclear where name -> Data resolution should go
class TreadleBackend[T <: MultiIOModule](val annos: AnnotationSeq) extends BackendInstance[T] with ThreadedBackend[T] with LazyLogging {
  val tester = annos.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
    throw new Exception(
      s"TreadleTesterPhase could not build a treadle tester from these annotations" +
        annos.mkString("Annotations:\n", "\n  ", "")
    )
  )

  protected def resolveName(signal: Data): String = { // TODO: unify w/ dataNames?
    dataNames.getOrElse(signal, signal.toString)
  }

  override def pokeClock(signal: Clock, value: Boolean): Unit = {
    // TODO: check thread ordering
    val intValue = if (value) 1 else 0
    tester.poke(dataNames(signal), intValue)
    logger debug (s"${resolveName(signal)} <- $intValue")
  }

  override def peekClock(signal: Clock): Boolean = {
    doPeek(signal, new Throwable)
    val a = tester.peek(dataNames(signal))
    logger debug (s"${resolveName(signal)} -> $a")
    a > 0
  }

  override def pokeBits(signal: Data, value: BigInt): Unit = {
    doPoke(signal, value, new Throwable)
    if (tester.peek(dataNames(signal)) != value) {
      idleCycles.clear()
    }
    tester.poke(dataNames(signal), value)
    logger debug (s"${resolveName(signal)} <- $value")
  }

  override def peekBits(signal: Data, stale: Boolean): BigInt = {
    require(!stale, "Stale peek not yet implemented")

    doPeek(signal, new Throwable)
    val a = tester.peek(dataNames(signal))
    logger debug (s"${resolveName(signal)} -> $a")
    a
  }

  override def expectBits(signal: Data, value: BigInt, message: Option[String], decode: Option[BigInt => String], stale: Boolean): Unit = {
    require(!stale, "Stale peek not yet implemented")

    logger debug (s"${resolveName(signal)} ?> $value")
    Context().env.testerExpect(value, peekBits(signal, stale), resolveName(signal), message, decode)
  }

  protected val clockCounter: mutable.HashMap[Clock, Int] = mutable.HashMap()

  protected def getClockCycle(clk: Clock): Int = {
    clockCounter.getOrElse(clk, 0)
  }

  protected def getClock(clk: Clock): Boolean = tester.peek(dataNames(clk)).toInt match {
    case 0 => false
    case 1 => true
  }

  protected val lastClockValue: mutable.HashMap[Clock, Boolean] = mutable.HashMap()

  override def doTimescope(contents: () => Unit): Unit = {
    val createdTimescope = newTimescope()

    contents()

    closeTimescope(createdTimescope).foreach { case (data, valueOption) =>
      valueOption match {
        case Some(value) =>
          if (tester.peek(dataNames(data)) != value) {
            idleCycles.clear()
          }
          tester.poke(dataNames(data), value)
          logger debug (s"${resolveName(data)} <- (revert) $value")
        case None =>
          idleCycles.clear()
          tester.poke(dataNames(data), 0) // TODO: randomize or 4-state sim
          logger debug (s"${resolveName(data)} <- (revert) DC")
      }
    }
  }

  override def step(signal: Clock, cycles: Int): Unit = {
    // TODO: maybe a fast condition for when threading is not in use?
    for (_ <- 0 until cycles) {
      // If a new clock, record the current value so change detection is instantaneous
      if (signal != dut.clock && !lastClockValue.contains(signal)) {
        lastClockValue.put(signal, getClock(signal))
      }

      val thisThread = currentThread.get
      thisThread.clockedOn = Some(signal)
      schedulerState.currentThreadIndex += 1
      scheduler()
      thisThread.waiting.acquire()
    }
  }

  override def run(testFn: T => Unit): Unit = {
    rootTimescope = Some(new RootTimescope)
    val mainThread = new TesterThread(() => {
      /** @todo consume init anonation here. */
      tester.poke("reset", 1)
      tester.step(1)
      tester.poke("reset", 0)

      testFn(dut)
    }, TimeRegion(0, Region.default), rootTimescope.get, 0, Region.default, None)
    mainThread.thread.start()
    require(allThreads.isEmpty)
    allThreads += mainThread

    try {
      while (!mainThread.done) { // iterate timesteps
        clockCounter.put(dut.clock, getClockCycle(dut.clock) + 1)

        logger debug (s"clock step")

        // TODO: allow dependent clocks to step based on test stimulus generator
        // TODO: remove multiple invocations of getClock
        // Unblock threads waiting on dependent clock
        val steppedClocks = Seq(dut.clock) ++ lastClockValue.collect {
          case (clock, lastValue) if getClock(clock) != lastValue && getClock(clock) => clock
        }
        steppedClocks foreach { clock =>
          clockCounter.put(dut.clock, getClockCycle(clock) + 1) // TODO: ignores cycles before a clock was stepped on
        }
        lastClockValue foreach { case (clock, _) =>
          lastClockValue.put(clock, getClock(clock))
        }

        runThreads(steppedClocks.toSet)

        idleLimits foreach { case (clock, limit) =>
          idleCycles.put(clock, idleCycles.getOrElse(clock, -1) + 1)
          if (idleCycles(clock) >= limit) {
            throw new TimeoutException(s"timeout on $clock at $limit idle cycles")
          }
        }

        tester.step(1)
      }
    } finally {
      rootTimescope = None

      for (thread <- allThreads.clone()) {
        // Kill the threads using an InterruptedException
        if (thread.thread.isAlive) {
          thread.thread.interrupt()
        }
      }

      tester.report() // needed to dump VCDs
    }
  }
}
