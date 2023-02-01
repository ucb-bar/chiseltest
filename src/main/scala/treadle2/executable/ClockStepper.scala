// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import treadle2.chronometry.UTC
import treadle2.utils.Render

import scala.collection.mutable

trait ClockStepper {
  var cycleCount: Long = 0L
  def run(steps: Int): Unit
  def getCycleCount: Long = cycleCount
  def addTask(taskTime: Long)(task: () => Unit): Unit
  val clockAssigners: mutable.HashMap[Symbol, ClockAssigners] = new mutable.HashMap()
  def bumpClock(clockSymbol: Symbol, value: BigInt): Unit = {}
  def combinationalBump(value: Long): Unit = {}
}

class NoClockStepper extends ClockStepper {
  override def run(steps: Int): Unit = {}

  override def addTask(taskTime: Long)(task: () => Unit): Unit = {
    throw TreadleException(s"Timed task cannot be added to circuits with no clock")
  }

  val clockSymbols: Set[Symbol] = Set.empty
}

case class ClockAssigners(upAssigner: Assigner, downAssigner: Assigner)

case class SimpleSingleClockStepper(
  engine:             ExecutionEngine,
  dataStore:          DataStore,
  clockSymbol:        Symbol,
  resetSymbolOpt:     Option[Symbol],
  clockPeriod:        Long,
  clockInitialOffset: Long,
  wallTime:           UTC)
    extends ClockStepper {

  var clockIsHigh: Boolean = false
  def clockIsLow:  Boolean = !clockIsHigh

  val upPeriod:   Long = clockPeriod / 2
  val downPeriod: Long = clockPeriod - upPeriod

  var resetTaskTime: Long = -1L

  var isFirstRun: Boolean = true

  var combinationalBumps: Long = 0L

  /** This function is (and should only) be used by the VcdReplayTester
    * @param clockSymbol clock to bump
    * @param value        new clock value should be zero or one, all non-zero values are treated as one
    */
  override def bumpClock(clockSymbol: Symbol, value: BigInt): Unit = {
    engine.setValue(clockSymbol.name, value)
    cycleCount += 1
  }

  override def combinationalBump(value: Long): Unit = {
    combinationalBumps += value
    wallTime.incrementTime(value)
  }

  /** Execute specified number of clock cycles (steps)
    * @param steps number of clock cycles to advance
    */
  //scalastyle:off method.length
  override def run(steps: Int): Unit = {

    /** This handles the possibility that a reset clearing was scheduled to occur during the time
      * interval
      */
    def handlePossibleReset(increment: Long): Long = {
      if (resetTaskTime > wallTime.currentTime && wallTime.currentTime + increment >= resetTaskTime) {
        val incrementToReset = resetTaskTime - wallTime.currentTime
        wallTime.incrementTime(incrementToReset)

        resetSymbolOpt.foreach { resetSymbol =>
          engine.setValue(resetSymbol.name, 0)
          if (increment - incrementToReset > 0) {
            engine.evaluateCircuit()
          }
        }
        resetTaskTime = -1L

        increment - incrementToReset
      } else {
        increment
      }
    }

    /** Raise the clock and propagate changes
      */
    def raiseClock(): Unit = {
      engine.setIntValue(clockSymbol, 1)
      engine.evaluateCircuit()

      val remainingIncrement = handlePossibleReset(upPeriod)

      wallTime.incrementTime(remainingIncrement)
      combinationalBumps = 0L
    }

    /** lower the clock
      */
    def lowerClock(): Unit = {
      engine.setIntValue(clockSymbol, 0)
      combinationalBumps = 0L
    }

    for (_ <- 0 until steps) {
      if (engine.verbose) {
        Render.headerBar(s"step ${cycleCount + 1} started")
      }

      if (engine.inputsChanged) {
        engine.evaluateCircuit()
      }

      cycleCount += 1

      /* This bit of code adjusts for any combinational delays occur since the  down clock */
      val downIncrement = if (isFirstRun) {
        isFirstRun = false
        clockInitialOffset - wallTime.currentTime
      } else {
        downPeriod - combinationalBumps
      }

      val remainingIncrement = handlePossibleReset(downIncrement)

      wallTime.incrementTime(remainingIncrement)

      raiseClock()

      lowerClock()

      if (engine.verbose) {
        Render.headerBar(s"Done step: $cycleCount finished")
      }
    }
  }

  override def addTask(taskTime: Long)(task: () => Unit): Unit = {
    if (resetTaskTime >= 0) {
      throw TreadleException(s"Timed add second reset task to single clock")
    }
    resetTaskTime = taskTime
  }
}

//TODO (Chick) Add support for combinational delays here.
/** Manage multiple top-level clocks
  * step is interpreted here to mean advance to the next clock cycle considering all the clocks
  *      multiple clocks may fire at that time
  * @param engine         engine for this stepper
  * @param clockInfoList  externally specified clocks and their properties
  * @param wallTime       handle to top level wall time
  */
class MultiClockStepper(engine: ExecutionEngine, clockInfoList: Seq[ClockInfo], wallTime: UTC) extends ClockStepper {
  val dataStore: DataStore = engine.dataStore
  val scheduler: Scheduler = engine.scheduler

  val shortestPeriod: Long = clockInfoList.map(_.period).min

  clockInfoList.foreach { clockInfo =>
    val clockSymbol = engine.symbolTable(clockInfo.name)

    // this sets clock high and will call register updates
    wallTime.addRecurringTask(clockInfo.period, clockInfo.initialOffset, s"${clockInfo.name}/up") { () =>
      cycleCount += 1
      engine.setValue(clockSymbol.name, BigInt(1))
    }

    // this task sets clocks low
    wallTime.addRecurringTask(
      clockInfo.period,
      clockInfo.initialOffset + clockInfo.upPeriod,
      s"${clockInfo.name}/down"
    ) { () =>
      engine.setValue(clockSymbol.name, BigInt(0))
    }
  }

  /** This function is (and should only) be used by the VcdReplayTester
    * @param clockSymbol clock to bump
    * @param value        new clock value should be zero or one, all non-zero values are treated as one
    */
  override def bumpClock(clockSymbol: Symbol, value: BigInt): Unit = {
    val assigner = clockAssigners(clockSymbol)
    if (value > Big(0)) {
      assigner.upAssigner.run()
    } else {
      assigner.downAssigner.run()
    }
  }

  /** One step is defined here as the running until the next up clock transition
    * @param steps the number of up clocks to find and execute
    */
  override def run(steps: Int): Unit = {

    for (_ <- 0 until steps) {
      if (engine.inputsChanged) {
        engine.evaluateCircuit()
      }

      var upTransitionProcessed = false

      def runHeadTask(): Unit = {
        wallTime.runNextTask().foreach { taskRun =>
          if (taskRun.taskName.endsWith("/up")) {
            cycleCount += 1
            upTransitionProcessed = true
          }
        }
      }

      while (!upTransitionProcessed) {
        runHeadTask()
      }

      /*
      there could be multiple clocks temporarily set to run at this
      same time, let them all run
       */
      while (wallTime.eventQueue.head.time == wallTime.currentTime) {
        runHeadTask()
      }
    }
  }

  override def addTask(taskTime: Long)(task: () => Unit): Unit = {
    wallTime.addOneTimeTask(taskTime)(task)
  }
}
