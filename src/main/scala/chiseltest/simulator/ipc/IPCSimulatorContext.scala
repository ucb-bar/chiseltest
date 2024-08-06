// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.ipc

import chiseltest.simulator._
import firrtl2.logger.LazyLogging

import java.nio.channels.FileChannel
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{blocking, Await, ExecutionContext, Future}
import scala.sys.process._

/** This context works with a simulation binary that communicates through shared memory.
  * @param cmd
  *   command to launch the simulation binary
  * @param toplevel
  *   information about the interface exposed by the module at the top of the RTL hierarchy
  * @param sim
  *   simulator that generated the binary
  * @param verbose
  *   show verbose messages from simulator
  */
private[chiseltest] class IPCSimulatorContext(
  cmd:              Seq[String],
  toplevel:         TopmoduleInfo,
  override val sim: Simulator,
  verbose:          Boolean)
    extends SimulatorContext
    with LazyLogging {
  toplevel.requireNoMultiClock()

  // Construct maps for the input and output
  private val (inputsNameToChunkSizeMap, outputsNameToChunkSizeMap) = {
    def genChunk(port: PinInfo): (String, Int) = port.name -> ((port.width - 1) / 64 + 1)
    (
      ListMap(toplevel.inputs.map(genChunk):  _*),
      ListMap(toplevel.outputs.map(genChunk): _*)
    )
  }
  private object SIM_CMD extends Enumeration {
    val RESET, STEP, UPDATE, POKE, PEEK, FORCE, GETID, GETCHK, FIN = Value
  }
  private val mask32 = (BigInt(1) << 32) - 1
  private val mask64 = (BigInt(1) << 64) - 1
  private def int(x: Int):  BigInt = BigInt(x) & mask32
  private def int(x: Long): BigInt = BigInt(x) & mask64

  private val allSignals = toplevel.inputs ++ toplevel.outputs
  private val signalMask = allSignals.map(s => s.name -> ((BigInt(1) << s.width) - 1)).toMap
  private val signalWidth = allSignals.map(s => s.name -> s.width).toMap
  private val isSignalSigned = allSignals.filter(_.signed).map(_.name).toSet

  private var isStale = false
  private val _pokeMap = mutable.HashMap[String, BigInt]()
  private val _peekMap = mutable.HashMap[String, BigInt]()
  private val _signalMap = mutable.HashMap[String, Int]()
  private val _chunks = mutable.HashMap[String, Int]()
  private val _logs = mutable.ArrayBuffer[String]()

  private def startProcess(cmd: Seq[String], logs: ArrayBuffer[String], cwd: os.Path): Process = {
    val processBuilder = Process(cmd, cwd = cwd.toIO)
    // This makes everything written to stderr get added as lines to logs
    val processLogger = ProcessLogger(
      { str => if (verbose) println(str) },
      { str =>
        logs.synchronized {
          logs += str
        }
      }
    )
    processBuilder.run(processLogger)
  }

  // initialize simulator process
  private val cwd = os.pwd
  private[chiseltest] val process = startProcess(cmd, _logs, cwd)

  // Set up a Future to wait for (and signal) the test process exit.
  import ExecutionContext.Implicits.global
  private[chiseltest] val exitValue = Future(blocking(process.exitValue()))

  // memory mapped channels
  private val (inChannel, outChannel, cmdChannel) = {
    // Wait for the startup message
    // NOTE: There may be several messages before we see our startup message.
    val simStartupMessageStart = "sim start on "
    while (!_logs.exists(_.startsWith(simStartupMessageStart)) && !exitValue.isCompleted) {
      Thread.sleep(100)
    }
    // Remove the startup message (and any precursors).
    while (_logs.nonEmpty && !_logs.head.startsWith(simStartupMessageStart)) {
      if (verbose) println(_logs.remove(0))
    }
    if (verbose)
      println(if (_logs.nonEmpty) _logs.remove(0) else "<no startup message>")
    else if (_logs.nonEmpty) _logs.remove(0)
    while (_logs.size < 3) {
      // If the test application died, throw a run-time error.
      throwExceptionIfDead(exitValue)
      Thread.sleep(100)
    }
    val in_channel_name = _logs.remove(0)
    val out_channel_name = _logs.remove(0)
    val cmd_channel_name = _logs.remove(0)
    val in_channel = new Channel(cwd, in_channel_name)
    val out_channel = new Channel(cwd, out_channel_name)
    val cmd_channel = new Channel(cwd, cmd_channel_name)
    if (verbose) {
      println(s"inChannelName: $in_channel_name")
      println(s"outChannelName: $out_channel_name")
      println(s"cmdChannelName: $cmd_channel_name")
    }

    in_channel.consume()
    cmd_channel.consume()
    in_channel.release()
    out_channel.release()
    cmd_channel.release()

    (in_channel, out_channel, cmd_channel)
  }

  private def dumpLogs(): Unit = {
    _logs.synchronized {
      _logs.foreach(x => println(x))
      _logs.clear()
    }
  }

  private def throwExceptionIfDead(exitValue: Future[Int]): Unit = {
    //    implicit val logger = new TestErrorLog
    if (exitValue.isCompleted) {
      val exitCode = Await.result(exitValue, Duration(-1, SECONDS))
      // We assume the error string is the last log entry.
      val errorString = if (_logs.nonEmpty) {
        _logs.last
      } else
        {
          "test application exit"
        } + " - exit code %d".format(exitCode)
      dumpLogs()
      isRunning = false
      throw TestApplicationException(exitCode, errorString)
    }
  }

  /** A busy-wait loop that monitors exitValue so we don't loop forever if the test application exits for some reason.
    *
    * @param block
    *   a thunk that determines when complete
    * @param loop
    *   a thunk to keep running until block is true or exitValue says completed.
    */
  private def mwhile(block: => Boolean)(loop: => Unit): Unit = {
    while (!exitValue.isCompleted && block) {
      loop
    }
    // If the test application died, throw a run-time error.
    throwExceptionIfDead(exitValue)
  }

  private def sendCmd(cmd: SIM_CMD.Value): Boolean = sendCmd(cmd.id)

  private def sendCmd(data: Int): Boolean = {
    cmdChannel.acquire()
    val ready = cmdChannel.ready
    if (ready) {
      cmdChannel(0) = data
      cmdChannel.produce()
    }
    cmdChannel.release()
    ready
  }

  private def sendCmd(data: String): Boolean = {
    cmdChannel.acquire()
    val ready = cmdChannel.ready
    if (ready) {
      cmdChannel(0) = data
      cmdChannel.produce()
    }
    cmdChannel.release()
    ready
  }

  private def recvResp = {
    outChannel.acquire()
    val valid = outChannel.valid
    val resp =
      if (!valid) None
      else {
        outChannel.consume()
        Some(outChannel(0).toInt)
      }
    outChannel.release()
    resp
  }

  private def sendValue(value: BigInt, chunk: Int) = {
    inChannel.acquire()
    val ready = inChannel.ready
    if (ready) {
      (0 until chunk).foreach(i => inChannel(i) = (value >> (64 * i)).toLong)
      inChannel.produce()
    }
    inChannel.release()
    ready
  }

  private def recvValue(chunk: Int) = {
    outChannel.acquire()
    val valid = outChannel.valid
    val value =
      if (!valid) None
      else {
        outChannel.consume()
        Some(
          (0 until chunk).foldLeft(BigInt(0))((res, i) => res | (int(outChannel(i)) << (64 * i)))
        )
      }
    outChannel.release()
    value
  }

  private def recvOutputs = {
    _peekMap.clear()
    outChannel.acquire()
    val valid = outChannel.valid
    if (valid) {
      (outputsNameToChunkSizeMap.toList.foldLeft(0)) { case (off, (out, chunk)) =>
        val value = (0 until chunk).foldLeft(BigInt(0))((res, i) => res | (int(outChannel(off + i)) << (64 * i)))
        _peekMap(out) = value
        off + chunk
      }
      outChannel.consume()
    }
    outChannel.release()
    valid
  }

  private def sendInputs = {
    inChannel.acquire()
    val ready = inChannel.ready
    if (ready) {
      (inputsNameToChunkSizeMap.toList.foldLeft(0)) { case (off, (in, chunk)) =>
        val value = _pokeMap.getOrElse(in, BigInt(0))
        (0 until chunk).foreach(i => inChannel(off + i) = (value >> (64 * i)).toLong)
        off + chunk
      }
      inChannel.produce()
    }
    inChannel.release()
    ready
  }

  private def update(): Unit = {
    mwhile(!sendCmd(SIM_CMD.UPDATE)) {}
    mwhile(!sendInputs) {}
    mwhile(!recvOutputs) {}
    isStale = false
  }

  private def takeStep(): Unit = {
    mwhile(!sendCmd(SIM_CMD.STEP)) {}
    mwhile(!sendInputs) {}
    mwhile(!recvOutputs) {}
    dumpLogs()
  }

  private def getId(path: String) = {
    mwhile(!sendCmd(SIM_CMD.GETID)) {}
    mwhile(!sendCmd(path)) {}
    if (exitValue.isCompleted) {
      0
    } else {
      (for {
        _ <- Iterator.from(1)
        data = recvResp
        if data.isDefined
      } yield data.get).next()
    }
  }

  private def getChunk(id: Int) = {
    mwhile(!sendCmd(SIM_CMD.GETCHK)) {}
    mwhile(!sendCmd(id)) {}
    if (exitValue.isCompleted) {
      0
    } else {
      (for {
        _ <- Iterator.from(1)
        data = recvResp
        if data.isDefined
      } yield data.get).next()
    }
  }

  private def poke(id: Int, chunk: Int, v: BigInt, force: Boolean = false): Unit = {
    val cmd = if (!force) SIM_CMD.POKE else SIM_CMD.FORCE
    mwhile(!sendCmd(cmd)) {}
    mwhile(!sendCmd(id)) {}
    mwhile(!sendValue(v, chunk)) {}
  }

  private def peek(id: Int, chunk: Int): BigInt = {
    mwhile(!sendCmd(SIM_CMD.PEEK)) {}
    mwhile(!sendCmd(id)) {}
    if (exitValue.isCompleted) {
      BigInt(0)
    } else {
      (for {
        _ <- Iterator.from(1)
        data = recvValue(chunk)
        if data.isDefined
      } yield data.get).next()
    }
  }

  private def start(): Unit = {
    if (verbose)
      println(s"""STARTING ${Utils.quoteCmdArgs(cmd)}""")
    mwhile(!recvOutputs) {}
    isRunning = true
  }

  override def poke(signal: String, value: BigInt): Unit = {
    if (inputsNameToChunkSizeMap.contains(signal)) {
      _pokeMap(signal) = value
      isStale = true
    } else {
      val id = _signalMap.getOrElseUpdate(signal, getId(signal))
      if (id >= 0) {
        val masked = signalMask(signal) & value
        poke(id, _chunks.getOrElseUpdate(signal, getChunk(id)), masked)
        isStale = true
      } else {
        logger.info(s"Can't find $signal in the emulator...")
      }
    }
  }

  override def peek(signal: String): BigInt = {
    if (isStale) update()
    val r = if (outputsNameToChunkSizeMap.contains(signal)) {
      _peekMap.get(signal)
    } else if (inputsNameToChunkSizeMap.contains(signal)) {
      // Added this in case peek is called on input before poke. Did not seem to be a problem in testers
      if (!_pokeMap.contains(signal)) {
        _pokeMap(signal) = BigInt(0)
        isStale = true
        update()
      }
      _pokeMap.get(signal)
    } else {
      val id = _signalMap.getOrElseUpdate(signal, getId(signal))
      if (id >= 0) {
        Some(peek(id, _chunks.getOrElse(signal, getChunk(id))))
      } else {
        logger.info(s"Can't find $signal in the emulator...")
        None
      }
    }
    val unsigned = r.getOrElse(throw new RuntimeException(s"Could not find signal $signal"))
    if (isSignalSigned(signal)) { toSigned(unsigned, signalWidth(signal)) }
    else { unsigned }
  }

  private def toSigned(v: BigInt, width: Int): BigInt = {
    val isNegative = ((v >> (width - 1)) & 1) == 1
    if (isNegative) {
      val mask = (BigInt(1) << width) - 1
      val twosComp = ((~v) + 1) & mask
      -twosComp
    } else { v }
  }

  private def defaultClock = toplevel.clocks.headOption
  override def step(n: Int): StepResult = {
    defaultClock match {
      case Some(value) =>
      case None        => throw NoClockException(toplevel.name)
    }
    var delta: Int = 0
    try {
      update()
      (0 until n).foreach { _ =>
        delta += 1
        takeStep()
      }
      StepOk
    } catch {
      case TestApplicationException(exit, msg) =>
        StepInterrupted(delta, exit != 0, List())
    }
  }

  private var isRunning = false
  override def finish(): Unit = {
    mwhile(!sendCmd(SIM_CMD.FIN)) {}
    val exit = Await.result(exitValue, Duration.Inf)
    if (verbose)
      println("Exit Code: %d".format(exit))
    inChannel.close()
    outChannel.close()
    cmdChannel.close()
    dumpLogs()
    isRunning = false
  }

  // Once everything has been prepared, we can start the communications.
  start()
}

private object TesterProcess {
  def apply(cmd: Seq[String], logs: ArrayBuffer[String], verbose: Boolean): Process = {
    require(new java.io.File(cmd.head).exists, s"${cmd.head} doesn't exist")
    val processBuilder = Process(Utils.quoteCmdArgs(cmd))
    // This makes everything written to stderr get added as lines to logs
    val processLogger = ProcessLogger(
      { str => if (verbose) println(str) },
      logs += _
    )
    processBuilder.run(processLogger)
  }
}

private class Channel(cwd: os.Path, name: String) {
  private lazy val file = new java.io.RandomAccessFile((cwd / name).toIO, "rw")
  private lazy val channel = file.getChannel
  @volatile private lazy val buffer = {
    val size = channel.size
    assert(size > 16, "channel.size is bogus: %d".format(size))
    channel.map(FileChannel.MapMode.READ_WRITE, 0, size)
  }

  val channel_data_offset_64bw = 4 // Offset from start of channel buffer to actual user data in 64bit words.
  def acquire(): Unit = {
    buffer.put(0, 1.toByte)
    buffer.put(2, 0.toByte)
    while (buffer.get(1) == 1 && buffer.get(2) == 0) {}
  }
  def release(): Unit = { buffer.put(0, 0.toByte) }
  def ready:     Boolean = buffer.get(3) == 0
  def valid:     Boolean = buffer.get(3) == 1
  def produce(): Unit = { buffer.put(3, 1.toByte) }
  def consume(): Unit = { buffer.put(3, 0.toByte) }
  def update(idx: Int, data: Long): Unit = {
    buffer.putLong(8 * idx + channel_data_offset_64bw, data)
  }
  def update(base: Int, data: String): Unit = {
    data.zipWithIndex.foreach { case (c, i) =>
      buffer.put(base + i + channel_data_offset_64bw, c.toByte)
    }
    buffer.put(base + data.length + channel_data_offset_64bw, 0.toByte)
  }
  def apply(idx: Int): Long = buffer.getLong(8 * idx + channel_data_offset_64bw)
  def close(): Unit = { file.close() }
  buffer.order(java.nio.ByteOrder.nativeOrder)
  os.remove(cwd / name)
}

private[chiseltest] case class TestApplicationException(exitVal: Int, lastMessage: String)
    extends RuntimeException(lastMessage)
