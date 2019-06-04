// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.experimental.{DataMirror, MultiIOModule}
import chisel3.stage.{ChiselStage, NoRunFirrtlCompilerAnnotation}
import chisel3.tester.internal._
import firrtl.transforms.{CheckCombLoops, CombinationalPath}
import treadle.{TreadleCircuitStateAnnotation, TreadleTester, TreadleTesterAnnotation}
import treadle.stage.TreadleTesterPhase

import scala.collection.mutable

// TODO: is Seq[CombinationalPath] the right API here? It's unclear where name -> Data resolution should go
class TreadleBackend[T <: MultiIOModule](dut: T,
    val dataNames: Map[Data, String], val combinationalPaths: Map[Data, Set[Data]],
    tester: TreadleTester)
    extends BackendInstance[T] with ThreadedBackend {

  type CycleTracker = mutable.Map[Clock, Int]

  // State for deadlock detection timeout
  val idleCycles: CycleTracker = mutable.Map.empty
  val idleLimits: CycleTracker = mutable.Map(dut.clock -> 1000)

  override def setTimeout(signal: Clock, cycles: Int): Unit = {
    require(signal == dut.clock, "timeout currently only supports master clock")
    if (cycles == 0) {
      idleLimits.remove(signal)
    } else {
      idleLimits.put(signal, cycles)
    }
    idleCycles.remove(signal)
  }

  //
  // Debug utility functions
  //
  val verbose: Boolean = false  // hard-coded debug flag
  def debugLog(str: => String) {
    if (verbose) println(str)
  }

  protected def resolveName(signal: Data): String = {  // TODO: unify w/ dataNames?
    dataNames.getOrElse(signal, signal.toString)
  }

  //
  // Circuit introspection functionality
  //
  override def getSourceClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  override def getSinkClocks(signal: Data): Set[Clock] = {
    throw new ClockResolutionException("ICR not available on chisel-testers2 / firrtl master")
  }

  //
  // Everything else
  //

  def getModule: T = dut

  override def pokeClock(signal: Clock, value: Boolean): Unit = {
    // TODO: check thread ordering
    val intValue = if (value) 1 else 0
    tester.poke(dataNames(signal), intValue)
    debugLog(s"${resolveName(signal)} <- $intValue")
  }

  override def peekClock(signal: Clock): Boolean = {
    doPeek(signal, new Throwable)
    val a = tester.peek(dataNames(signal))
    debugLog(s"${resolveName(signal)} -> $a")
    a > 0
  }

  override def pokeBits(signal: Bits, value: BigInt): Unit = {
    doPoke(signal, value, new Throwable)
    if (tester.peek(dataNames(signal)) != value) {
      idleCycles.clear()
    }
    tester.poke(dataNames(signal), value)
    debugLog(s"${resolveName(signal)} <- $value")
  }

  override def peekBits(signal: Bits, stale: Boolean): BigInt = {
    require(!stale, "Stale peek not yet implemented")

    doPeek(signal, new Throwable)
    val a = tester.peek(dataNames(signal))
    debugLog(s"${resolveName(signal)} -> $a")
    a
  }

  override def expectBits(signal: Bits, value: BigInt, message: Option[String], stale: Boolean): Unit = {
    require(!stale, "Stale peek not yet implemented")

    debugLog(s"${resolveName(signal)} ?> $value")
    Context().env.testerExpect(value, peekBits(signal, stale), resolveName(signal), message)
  }

  protected val clockCounter : mutable.HashMap[Clock, Int] = mutable.HashMap()
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
          debugLog(s"${resolveName(data)} <- (revert) $value")
        case None =>
          idleCycles.clear()
          tester.poke(dataNames(data), 0)  // TODO: randomize or 4-state sim
          debugLog(s"${resolveName(data)} <- (revert) DC")
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
    val mainThread = new TesterThread( () => {
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

        debugLog(s"clock step")

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
        Context().env.checkpoint()

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

      tester.report()  // needed to dump VCDs
    }
  }
}

object TreadleExecutive {
  import chisel3.experimental.BaseModule
  import chisel3.internal.firrtl.Circuit
  import firrtl._

  def getTopModule(circuit: Circuit): BaseModule = {
    (circuit.components find (_.name == circuit.name)).get.id
  }

  /** Returns a Seq of (data reference, fully qualified element names) for the input.
    * name is the name of data
    */
  def getDataNames(name: String, data: Data): Seq[(Data, String)] = Seq(data -> name) ++ (data match {
    case _: Element => Seq()
    case b: Record => b.elements.toSeq flatMap {case (n, e) => getDataNames(s"${name}_$n", e)}
    case v: Vec[_] => v.zipWithIndex flatMap {case (e, i) => getDataNames(s"${name}_$i", e)}
  })

  // TODO: better name
  def combinationalPathsToData(
    dut: BaseModule,
    paths: Seq[CombinationalPath],
    dataNames: Map[Data, String]
  ): Map[Data, Set[Data]] = {

    val nameToData = dataNames.map { case (port, name) => name -> port }  // TODO: check for aliasing
    paths.filter { p =>  // only keep paths involving top-level IOs
      p.sink.module.name == dut.name && p.sources.exists(_.module.name == dut.name)
    } .map { p =>  // discard module names
      p.sink.name -> p.sources.filter(_.module.name == dut.name).map(_.name)
    } .map { case (sink, sources) =>  // convert to Data
      // TODO graceful error message if there is an unexpected combinational path element?
      nameToData(sink) -> sources.map(nameToData(_)).toSet
    }.toMap
  }

  def start[T <: MultiIOModule](dutGen: () => T, annotationSeq: AnnotationSeq): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    try {
      val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)

      val chiseledAnnotations = (new ChiselStage).run(
        annotationSeq ++ Seq(generatorAnnotation, NoRunFirrtlCompilerAnnotation)
//        annotationSeq ++ Seq(generatorAnnotation)
      )

      val circuit = generatorAnnotation.elaborate.circuit

      val dut = getTopModule(circuit).asInstanceOf[T]

      val portNames = DataMirror.modulePorts(dut).flatMap { case (name, data) =>
        getDataNames(name, data).toList
      }.toMap

      val treadledAnnotations = TreadleTesterPhase.transform(chiseledAnnotations)

      val treadleTester = treadledAnnotations.collectFirst { case TreadleTesterAnnotation(t) => t }.getOrElse(
        throw new Exception(s"TreadleTesterPhase did not build a treadle tester")
      )

      val circuitState = treadledAnnotations.collectFirst { case TreadleCircuitStateAnnotation(s) => s }.get

      val pathAnnotations = (new CheckCombLoops).execute(circuitState).annotations

      val paths = pathAnnotations.collect {
        case c: CombinationalPath => c
      }

      val pathsAsData = combinationalPathsToData(dut, paths, portNames)

      new TreadleBackend(dut, portNames, pathsAsData, treadleTester)
    }
    catch {
      case t: Throwable =>
        throw new Exception(s"Problem with compilation: ${t.getMessage}")
    }
  }
}
