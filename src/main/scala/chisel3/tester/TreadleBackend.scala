// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.experimental.{DataMirror, MultiIOModule}
import chisel3.HasChiselExecutionOptions
import firrtl.transforms.CombinationalPath
import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}
import treadle.{HasTreadleSuite, TreadleTester}

import scala.collection.mutable

// TODO: is Seq[CombinationalPath] the right API here? It's unclear where name -> Data resolution should go
class TreadleBackend[T <: MultiIOModule](dut: T,
    val dataNames: Map[Data, String], val combinationalPaths: Map[Data, Set[Data]],
    tester: TreadleTester)
    extends BackendInstance[T] with ThreadedBackend {

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
  // Everything else
  //

  def getModule: T = dut

  override def pokeBits(signal: Bits, value: BigInt): Unit = {
    doPoke(signal, value, new Throwable)
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
        case Some(value) => tester.poke(dataNames(data), value)
           debugLog(s"${resolveName(data)} <- (revert) $value")
        case None => tester.poke(dataNames(data), 0)  // TODO: randomize or 4-state sim
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
      schedulerState.blockedThreads.getOrElseUpdate(signal, mutable.ListBuffer[TesterThread]()) += thisThread
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
      }, 0, rootTimescope.get, 0)
    mainThread.thread.start()

    val blockedThreads = mutable.HashMap[Clock, mutable.ListBuffer[TesterThread]](
        (dut.clock, mutable.ListBuffer(mainThread)))

    while (!mainThread.done) {  // iterate timesteps
      val unblockedThreads = new mutable.ListBuffer[TesterThread]()

      // Unblock threads waiting on main clock
      unblockedThreads ++= blockedThreads.getOrElse(dut.clock, Seq())
      blockedThreads.remove(dut.clock)
      clockCounter.put(dut.clock, getClockCycle(dut.clock) + 1)
      currentTimestep += 1

      debugLog(s"clock step")

      // TODO: allow dependent clocks to step based on test stimulus generator
      // Unblock threads waiting on dependent clock
      lastClockValue foreach { case (clock, lastValue) =>
        val currentValue = getClock(clock)
        if (currentValue != lastValue) {
          lastClockValue.put(clock, currentValue)
          if (currentValue) {  // rising edge
            unblockedThreads ++= blockedThreads.getOrElse(clock, Seq())
            blockedThreads.remove(clock)
            clockCounter.put(clock, getClockCycle(clock) + 1)
          }
        }
      }

      // Actually run things
      val newBlockedThreads = runThreads(unblockedThreads)
      newBlockedThreads.foreach{ case (clock, threads) =>
        blockedThreads.getOrElseUpdate(clock, mutable.ListBuffer[TesterThread]()) ++= threads
      }

      timestep()
      Context().env.checkpoint()
      tester.step(1)
    }

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

object TreadleExecutive {
  import chisel3.internal.firrtl.Circuit
  import chisel3.experimental.BaseModule
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
  def combinationalPathsToData(dut: BaseModule, paths: Seq[CombinationalPath], dataNames: Map[Data, String]) = {
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

  def start[T <: MultiIOModule](
      dutGen: () => T,
      userOptions: Option[ExecutionOptionsManager] = None): BackendInstance[T] = {
    // Create the base options manager that has all the components we care about, and initialize defaults
    val optionsManager = new ExecutionOptionsManager("chisel3")
        with HasChiselExecutionOptions with HasFirrtlOptions with HasTreadleSuite

    optionsManager.treadleOptions = optionsManager.treadleOptions.copy(writeVCD = true)

    // If the user specified options, override the default fields.
    // Note: commonOptions and firrtlOptions are part of every ExecutionOptionsManager, so will always be defined
    // whether the user intended to or not. In those cases testers2 forces an override to the testers2 defaults.
    userOptions foreach {
      case userOptions: HasChiselExecutionOptions => optionsManager.chiselOptions = userOptions.chiselOptions
      case _ =>
    }
    optionsManager.commonOptions = optionsManager.commonOptions.copy(
        targetDirName = s"test_run_dir/${dutGen.getClass.getName}")

    userOptions foreach {
      case userOptions: HasFirrtlOptions => optionsManager.firrtlOptions = userOptions.firrtlOptions
      case _ =>
    }
    optionsManager.firrtlOptions = optionsManager.firrtlOptions.copy(compilerName = "low")

    userOptions foreach {
      case userOptions: HasTreadleSuite => optionsManager.treadleOptions = userOptions.treadleOptions
      case _ =>
    }

    chisel3.Driver.execute(optionsManager, dutGen) match {
      case ChiselExecutionSuccess(Some(circuit), _, Some(firrtlExecutionResult)) =>
        firrtlExecutionResult match {
          case success: FirrtlExecutionSuccess =>
            val dut = getTopModule(circuit).asInstanceOf[T]
            val interpretiveTester = new TreadleTester(success.emitted, optionsManager)

            val portNames = DataMirror.modulePorts(dut).flatMap { case (name, data) =>
              getDataNames(name, data).toList
            }.toMap
            val paths = success.circuitState.annotations.collect {
              case c: CombinationalPath => c
            }
            val pathsAsData = combinationalPathsToData(dut, paths, portNames)

            new TreadleBackend(dut, portNames, pathsAsData, interpretiveTester)
          case FirrtlExecutionFailure(message) =>
            throw new Exception(s"FirrtlBackend: failed firrtl compile message: $message")
        }
      case _ =>
        throw new Exception("Problem with compilation")
    }
  }
}
