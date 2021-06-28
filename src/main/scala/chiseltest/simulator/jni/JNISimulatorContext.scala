package chiseltest.simulator.jni

import chiseltest.simulator.ipc.TestApplicationException
import chiseltest.simulator.{Simulator, SimulatorContext, SimulatorResults, TopmoduleInfo}
import logger.LazyLogging

import java.io.File
import scala.collection.immutable.ListMap
import scala.collection.mutable

/** This context works with a simulation binary that communicates through shared memory.
 * @param cmd command to launch the simulation binary
 * @param toplevel information about the interface exposed by the module at the top of the RTL hierarchy
 * @param waveformFile optional path to the waveform file, we assume that the file will only be available _after_
 *                     the simulation has been terminated
 * @param loadCoverage may be called _after_ the simulation is finished to retrieve coverage information
 * @param sim simulator that generated the binary
 * */
private [chiseltest] class JNISimulatorContext(cmd: Seq[String], toplevel: TopmoduleInfo,
  override val sim: Simulator) extends SimulatorContext with LazyLogging {
  require(toplevel.clocks.size == 1, "currently this interface only works with exactly one clock")

  def int(x: Int): BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)
  def int(x: Long): BigInt = (BigInt(x >>> 1) << 1) | BigInt(x & 1)

  private var isStale = false
  private val _signalMap = mutable.HashMap[String, Int]()
  private val _logs = mutable.ArrayBuffer[String]()

  // load shared library
  private val so = TesterSharedLib(cmd, _logs)

  private def update(): Unit = {
    so.update()
    isStale = false
  }

  private def takeStep(): Unit = {
    so.step()
  }

  private def getId(path: String): Int = {
    so.getid(path)
  }

  private def getChunk(id: Int): Int = {
    so.getchk(id)
  }

  private def poke(id: Int, v: BigInt): Unit = {
    so.poke(id, v.toInt)
  }

  private def peek(id: Int): BigInt = {
    so.peek(id)
  }

  private def start(): Unit = {
    println(s"""STARTING ${cmd mkString " "}""")
    so.start()
    isRunning = true
  }

  override def poke(signal: String, value: BigInt) {
    val id = _signalMap.getOrElseUpdate(signal, getId(signal))
    if (id >= 0) {
      poke(id, value)
      isStale = true
    } else {
      logger.info(s"Can't find $signal in the emulator...")
    }
  }

  override def peek(signal: String): BigInt = {
    if (isStale) { update() }
    val id = _signalMap getOrElseUpdate (signal, getId(signal))
    val r = if (id >= 0) {
      Some(peek(id))
    } else {
      logger.info(s"Can't find $signal in the emulator...")
      None
    }
    r.getOrElse(throw new RuntimeException(s"Could not find signal $signal"))
  }

  override def step(clock: String, n: Int): Option[SimulatorResults] = {
    require(clock == toplevel.clocks.head)
    try {
      update()
      (0 until n).foreach(_ => takeStep())
      None
    } catch {
      case TestApplicationException(exit, msg) =>
        Some(SimulatorResults(exit))
    }
  }

  private var isRunning = false
  override def finish(): SimulatorResults = {
    so.finish()
    isRunning = false
    // TODO: exit code?
    SimulatorResults(0)
  }

  // Once everything has been prepared, we can start the communications.
  start()

  override def peekMemory(memory: String, index: Long) = {
    throw new NotImplementedError("peekMemory")
  }

  override def pokeMemory(memory: String, index: Long, value: BigInt): Unit = {
    throw new NotImplementedError("pokeMemory")
  }
}

class TesterSharedLib(libPath: String) {
  Predef.printf(s"TesterSharedLib: loading $libPath ")
  try {
    System.load(new File(libPath).getCanonicalPath())
    println(" ok")
  } catch {
    case e: Throwable =>
      println(" failed: " + e.toString)
      throw e
  }

  private val state: Long = 0

  @native private def sim_init(): Unit
  @native def reset(): Unit
  @native def step(): Unit
  @native def update(): Unit
  @native def poke(id: Int, value: Int): Unit
  @native def peek(id: Int): Int
  @native def force(): Unit
  @native def getid(path: String): Int
  @native def getchk(id: Int): Int
  @native def finish(): Unit
  @native def start(): Unit

  println(s"State before: $state")
  sim_init()
  println(s"State after: $state")
}

private[chiseltest] object TesterSharedLib {
  def apply(cmd: Seq[String], logs: mutable.ArrayBuffer[String]): TesterSharedLib = {
    require(os.exists(os.Path(cmd.head)), s"${cmd.head} doesn't exist")
    new TesterSharedLib(cmd.head)
  }
}