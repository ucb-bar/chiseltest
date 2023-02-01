// SPDX-License-Identifier: Apache-2.0

//
package treadle2.utils

import treadle2.TreadleTester
import treadle2.executable.ExecutionEngine
import treadle2.vcd.{VCD, Wire}

/** Executes a vcd file as a script against a [[TreadleTester]] instance.
  * @param tester  the circuit to be updated
  * @param vcd     the vcd values to use
  */
class VcdRunner(val tester: TreadleTester, val vcd: VCD) {
  val events:  Array[Long] = vcd.events
  val engine:  ExecutionEngine = tester.engine
  val inputs:  Set[String] = engine.symbolTable.inputPortsNames.toSet
  val outputs: Set[String] = tester.engine.symbolTable.outputPortsNames.toSet

  val clockNames: Set[String] = tester.clockStepper.clockAssigners.keys.map(_.name).toSet

  var verbose:          Boolean = false
  var justSetInputs:    Boolean = true
  var testAliasedWires: Boolean = false

  var eventsRun:      Long = 0L
  var inputValuesSet: Long = 0L
  var valuesTested:   Long = 0L
  var testSuccesses:  Long = 0L
  var testFailures:   Long = 0L

  var nextEvent:     Int = 0
  def previousEvent: Int = nextEvent - 1

  /** Are there more events to run
    * @return
    */
  def hasNextEvent: Boolean = {
    nextEvent < events.length
  }

  def indexOutOfRange(index: Int, caller: String): Boolean = {
    if (index < 0) {
      println(s"In $caller: Index < 0")
      true
    } else if (index >= events.length) {
      println(s"In $caller: index $index is out of range ${events.length}")
      true
    } else {
      false
    }
  }

  /** Does the symbol name exist in the engine
    * @param symbolName symbol of interest
    * @return
    */
  def hasName(symbolName: String): Boolean = {
    engine.symbolTable.contains(symbolName)
  }

  def setValue(wire: Wire, newValue: BigInt): Unit = {
    val fullName = wire.fullName
    if (hasName(fullName)) {
      tester.poke(fullName, newValue)
      println(s"$fullName <= ${engine.symbolTable(fullName).normalize(newValue)}")
      inputValuesSet += 1
    }
  }

  /** Zero all test results
    */
  def clearTestResults(): Unit = {
    valuesTested = 0
    testSuccesses = 0
    testFailures = 0
  }

  /** One line string showing results of last (or cumulative) tests
    * @return
    */
  def getTestResults: String = {
    s"tested: $valuesTested, correct values $testSuccesses" + (
      if (testFailures > 0) {
        Console.RED + s", failures $testFailures" + Console.RESET
      } else {
        ""
      }
    )
  }

  /** Test that a wire is set to the correct value, print if error or verbose
    * @param wire      wire to check
    * @param newValue  value it should be
    */
  def checkValue(wire: Wire, newValue: BigInt): Unit = {
    val fullName = wire.fullName

    valuesTested += 1

    if (hasName(fullName)) {
      val circuitValue = engine.getValue(fullName)

      val result = if (tester.peek(fullName) == newValue) {
        testSuccesses += 1
        "ok"
      } else {
        testFailures += 1
        Console.RED + "bad" + Console.RESET
      }
      if (verbose || result != "ok") {
        println(s"Testing $fullName: circuit $circuitValue, vcd $newValue $result")
      }
    }
  }

  /** Set all initial values
    */
  def setInitialValues(): Unit = {
    vcd.initialValues.foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        setValue(wire, change.value)
      }
    }
  }

  /** poke the top level inputs, if the input is a clock
    * call the associated assigner so that dependent registers get flipped
    *
    * @param eventNumber current time from VCS file.
    */
  def setInputs(eventNumber: Int): Unit = {
    if (indexOutOfRange(eventNumber, "setInputs")) return

    vcd.valuesAtTime(events(eventNumber)).foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        val fullName = wire.fullName
        if (inputs.contains(fullName)) {
          val inputSymbol = engine.symbolTable(fullName)
          if (tester.clockStepper.clockAssigners.contains(inputSymbol)) {
            tester.clockStepper.bumpClock(inputSymbol, change.value)
          } else {
            setValue(wire, change.value)
          }
          tester.engine.inputsChanged = true
        }
      }
    }
    tester.wallTime.setTime(events(eventNumber))
    tester.engine.evaluateCircuit()
  }

  /** poke every changed wire with it's new value
    * @param eventNumber current time from VCS file.
    */
  def setAllWires(eventNumber: Int): Unit = {
    if (indexOutOfRange(eventNumber, "setAllWires")) return

    if (eventNumber == 0) {
      setInitialValues()
    }

    vcd.valuesAtTime(events(eventNumber)).foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        setValue(wire, change.value)
      }
    }
    tester.engine.inputsChanged = false
  }

  /** Test wires at specified time index, args control just outputs or all non-input wires
    * @param eventNumber    time index to test
    * @param justOutputs  can verify outputs or all wires that are not top-level inputs
    * @param clearResult  repl want this to clear all the time, but can override to accumulate stats
    */
  def testWires(eventNumber: Int, justOutputs: Boolean, clearResult: Boolean = true): Unit = {
    if (clearResult) clearTestResults()

    if (indexOutOfRange(eventNumber, "testWires")) return

    var (tested, failed, ok) = (0, 0, 0)

    def testThisWire(name: String): Boolean = {
      if (inputs.contains(name)) {
        false
      } else if (justOutputs && !outputs.contains(name)) {
        false
      } else {
        true
      }
    }

    vcd.valuesAtTime(events(eventNumber)).foreach { change =>
      if (testAliasedWires) {
        vcd.wiresFor(change).foreach { wire =>
          val fullName = change.wire.fullName
          if (testThisWire(fullName)) checkValue(wire, change.value)
        }
      } else {
        val fullName = change.wire.fullName
        if (testThisWire(fullName)) checkValue(change.wire, change.value)
      }
    }
  }

  /** Used to determine whether the next event has a clock up transition
    * @return
    */
  def nextEventHasClockUp: Boolean = {
    vcd.valuesAtTime(events(nextEvent)).exists { change =>
      vcd.wiresFor(change).exists { wire =>
        clockNames.contains(wire.fullName) && change.value != BigInt(0)
      }
    }
  }

  /** Show information about the event at the specified time index
    * @param eventNumber  the event number
    * @return
    */
  def eventSummary(eventNumber: Int): String = {
    if (indexOutOfRange(eventNumber, "eventSummary")) return ""

    var inputsChanged: Int = 0
    var totalChanges:  Int = 0
    var clockInfo:     String = ""

    vcd.valuesAtTime(events(eventNumber)).foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        val fullName = wire.fullName
        if (clockNames.contains(fullName)) {
          clockInfo += s""",  $fullName:${if (change.value != BigInt(0)) "↑" else "↓"}"""
        } else if (inputs.contains(fullName)) {
          inputsChanged += 1
        }

        totalChanges += 1
      }

    }
    f"$eventNumber%4d, time: ${events(eventNumber)}%5d, inputs $inputsChanged%6d total $totalChanges%6d$clockInfo"
  }

  /** Process the next event and increment the event counter
    */
  def executeNextEvent(): Unit = {
    if (nextEvent >= events.length) {
      println(s"No more events to process: at $nextEvent of ${events.length}")
      return
    }

    if (justSetInputs) {
      setInputs(nextEvent)
    } else {
      setAllWires(nextEvent)
    }

    nextEvent += 1
  }

  /** just like it says
    * @param eventNumber event number
    */
  def setNextEvent(eventNumber: Int): Unit = {
    nextEvent = eventNumber
  }

  /** Short cut to set next event number and run
    * @param eventNumber event number
    */
  def executeEvent(eventNumber: Int): Unit = {
    nextEvent = eventNumber
    executeNextEvent()
  }
}
