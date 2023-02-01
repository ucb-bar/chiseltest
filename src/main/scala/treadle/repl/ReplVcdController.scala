// SPDX-License-Identifier: Apache-2.0

package treadle.repl

import treadle.executable.ExecutionEngine
import treadle.vcd.VCD
import treadle.TreadleRepl
import treadle.utils.VcdRunner

import scala.tools.jline.console.ConsoleReader
import scala.util.matching.Regex

class ReplVcdController(val repl: TreadleRepl, val engine: ExecutionEngine, val vcd: VCD) {
  val console: ConsoleReader = repl.console

  // The following three elements track state of running the vcd file
  val timeStamps: Array[Long] = vcd.valuesAtTime.keys.toList.sorted.toArray

  // The following control the current list state of the vcd file
  var currentListLocation: Int = 0
  var currentListSize:     Int = 10

  // The following control the current execution options
  var testAfterRun:  Boolean = true
  var justSetInputs: Boolean = true

  val IntPattern: Regex = """(-?\d+)""".r

  val vcdRunner: VcdRunner = new VcdRunner(repl.currentTreadleTester, vcd)

  def currentTimeIndex: Int = vcdRunner.nextEvent

  def now: String = {
    showEvent(currentTimeIndex)
  }

  def showEvent(timeIndex: Int): String = {
    vcdRunner.eventSummary(timeIndex)
  }

  def showInputs(timeIndex: Int): Unit = {
    var hasStep = false
    if (timeIndex == currentTimeIndex) console.print(Console.GREEN)
    console.println(now)
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      if (vcdRunner.inputs.contains(change.wire.name)) {
        console.println(s"       ${change.wire.name} <= ${change.value}")
      }
    }
    if (timeIndex == currentTimeIndex) console.print(Console.RESET)
  }

  def showChanges(timeIndex: Int, showDetail: Boolean = false): Unit = {
    if (timeIndex == currentTimeIndex) console.print(Console.GREEN)
    console.println(showEvent(timeIndex))
    if (showDetail) {
      vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
        console.println(s"        ${change.wire.fullName} <= ${change.value}")
      }
    }
    if (timeIndex == currentTimeIndex) console.print(Console.RESET)
  }

  //scalastyle:off method.length
  /** Applies changes to circuit based on current vcd time step to current inputs.
    *
    * @note At time step zero all possible changes are applied.
    * @return
    */
  def doChanges(): Unit = {
    vcdRunner.executeNextEvent()
  }

  def runUsage: String = {
    """vcd run                    run one event
      |vcd run all                run all remaining
      |vcd run to step            run event until a step occurs (clock up transition)
      |vcd run to <event-number>  run up to given event-number
      |vcd run <number-of-events> run this many events, from current
      |vcd run set <event>        set next event to run
      |vcd run test               set run to test outputs as events are processed
      |vcd run notest             turn off testing of outputs as events are processed
      |vcd run justSetInputs      just set the inputs from the vcd script
      |vcd run setAllWires        set all wires from vcd script
      |vcd run verbose            run in verbose mode (the default)
      |vcd run noverbose          do not run in verbose mode
      |""".stripMargin
  }

  //scalastyle:off cyclomatic.complexity method.length
  def run(parameters: Array[String]): Unit = {
    parameters.toList match {
      case Nil =>
        vcdRunner.executeNextEvent()

      case "to" :: tail =>
        tail match {
          case IntPattern(nString) :: _ =>
            val n = nString.toInt
            if (n <= currentTimeIndex) {
              console.println(s"run to $n, error, $n must be greater then current time index ${currentTimeIndex + 1}")
            } else {
              while (vcdRunner.nextEvent <= n & currentTimeIndex < timeStamps.length) {
                vcdRunner.executeNextEvent()
              }
              if (testAfterRun) vcdRunner.testWires(vcdRunner.previousEvent, justOutputs = true)
            }
          case "step" :: _ =>
            var upClockFound = false
            while (vcdRunner.hasNextEvent && !upClockFound) {
              upClockFound = vcdRunner.nextEventHasClockUp
              vcdRunner.executeNextEvent()
            }
            if (testAfterRun) vcdRunner.testWires(vcdRunner.previousEvent, justOutputs = true)
        }
      case "test" :: _ =>
        testAfterRun = true
      case "notest" :: _ =>
        testAfterRun = false
      case "justSetInputs" :: _ =>
        vcdRunner.justSetInputs = true
      case "setAllWires" :: _ =>
        vcdRunner.justSetInputs = false
      case "verbose" :: _ =>
        vcdRunner.verbose = true
      case "noverbose" :: _ =>
        vcdRunner.verbose = false
      case "all" :: _ =>
        while (vcdRunner.hasNextEvent) {
          vcdRunner.executeNextEvent()
        }
      case arg :: Nil =>
        arg match {
          case IntPattern(nString) =>
            for (_ <- 0 until nString.toInt.max(vcdRunner.events.length)) {
              vcdRunner.executeNextEvent()
            }
            if (testAfterRun) vcdRunner.testWires(vcdRunner.previousEvent, justOutputs = true)
          case _ =>
            console.println(s"Unknown run command ${parameters.mkString(" ")}")
            console.println(runUsage)
        }
      case "set" :: tail =>
        tail match {
          case IntPattern(nString) :: _ =>
            vcdRunner.setNextEvent(nString.toInt)
          case _ =>
            console.println(s"vcd next set requires event number")
        }
      case _ =>
        console.println(s"Unknown next command ${parameters.mkString(" ")}")
        console.println(runUsage)
    }
  }
  //scalastyle:on cyclomatic.complexity

  def test(parameters: Array[String]): Unit = {
    parameters.toList match {
      case "outputs" :: _ =>
        vcdRunner.testWires(vcdRunner.previousEvent, justOutputs = true)
        println(vcdRunner.getTestResults)
      case "all" :: _ =>
        vcdRunner.testWires(vcdRunner.previousEvent, justOutputs = false)
        println(vcdRunner.getTestResults)
      case _ =>
        console.println(s"Unknown test command argument ${parameters.mkString(" ")}")
        console.println(testUsage)
    }
  }

  def show(lo: Int, hi: Int): Unit = {
    for (timeIndex <- lo until hi) {
      if (timeIndex < timeStamps.length) {
        showChanges(timeIndex, showDetail = lo == hi - 1)
      }
    }
  }

  def showCurrent(): Unit = {
    val (lo, hi) = (0.max(currentListLocation), timeStamps.length.min(currentListLocation + currentListSize))
    show(lo, hi)
    currentListLocation += currentListSize
  }

  def listUsage: String = {
    """vcd list
      |vcd list all
      |vcd list <event-number>
      |vcd list <event-number> <window-size>
    """.stripMargin
  }

  def testUsage: String = {
    """vcd test outputs
      |vcd test all
    """.stripMargin
  }

  def list(parameters: Array[String]): Unit = {
    parameters.toList match {
      case Nil =>
        showCurrent()
      case "all" :: _ =>
        show(lo = 0, hi = timeStamps.length)
        currentListLocation = currentTimeIndex + 1
      case IntPattern(nString) :: IntPattern(eventString) :: _ =>
        currentListLocation = nString.toInt
        currentListSize = eventString.toInt
        showCurrent()
      case IntPattern(nString) :: _ =>
        currentListLocation = nString.toInt
        showCurrent()
      case _ =>
        console.println(s"Unknown list command list ${parameters.mkString(" ")} should be more like")
        console.println(listUsage)
    }
  }

  def usage: String = {
    runUsage + listUsage + testUsage
  }

  def loadVcd(parameters: Array[String]): Unit = {
    parameters.toList match {
      case fileName :: _ =>
        repl.loadVcdScript(fileName)
      case Nil =>
        repl.currentTreadleTesterOpt.foreach { tester =>
          val vcdName = tester.engine.ast.main + ".vcd"
          repl.loadVcdScript(vcdName)
        }
    }
  }

  /** command parser for vcd family of repl commands
    *
    * @param args arguments from user
    */
  def processListCommand(args: Array[String]): Unit = {
    args.headOption match {
      case Some("load") =>
        loadVcd(args.tail)
      case Some("inputs") =>
        showInputs(currentTimeIndex)
      case Some("run") =>
        run(args.tail)
      case Some("list") =>
        list(args.tail)
      case Some("info") =>
        console.println(vcd.info)
        console.println(f"run event:      $currentTimeIndex%8d")
        console.println(f"list position:  $currentListLocation%8d")
        console.println(f"list size:      $currentListSize%8d")
      case Some("test") =>
        test(args.tail)
      case Some("help") =>
        console.println(usage)
      case _ =>
        console.println(usage)
    }
  }
}
