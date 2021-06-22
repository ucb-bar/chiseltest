// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters



import chisel3.{ChiselExecutionFailure => _, ChiselExecutionResult => _, ChiselExecutionSuccess => _, _}

import java.io.File
import chiseltest.iotesters.DriverCompatibility._
import chiseltest.simulator.Simulator
import firrtl.annotations.Annotation
import logger.Logger

import scala.util.DynamicVariable

object Driver {
  private val backendVar = new DynamicVariable[Option[Simulator]](None)
  private[iotesters] def backend = backendVar.value

  private val optionsManagerVar = new DynamicVariable[Option[TesterOptionsManager]](None)
  def optionsManager = optionsManagerVar.value.getOrElse(new TesterOptionsManager)

  /**
   * This executes a test harness that extends peek-poke tester upon a device under test
   * with an optionsManager to control all the options of the toolchain components
   *
   * @param dutGenerator    The device under test, a subclass of a Chisel3 module
   * @param optionsManager  Use this to control options like which backend to use
   * @param testerGen       A peek poke tester with tests for the dut
   * @return                Returns true if all tests in testerGen pass
   */
  def execute[T <: Module](
    dutGenerator: () => T,
    optionsManager: TesterOptionsManager,
    firrtlSourceOverride: Option[String] = None
  )
    (
      testerGen: T => PeekPokeTester[T]
    ): Boolean = {
    optionsManagerVar.withValue(Some(optionsManager)) {
      Logger.makeScope(optionsManager) {
        if (optionsManager.topName.isEmpty) {
          if (optionsManager.targetDirName == ".") {
            optionsManager.setTargetDirName("test_run_dir")
          }
          val genClassName = testerGen.getClass.getName
          val testerName = genClassName.split("""\$\$""").headOption.getOrElse("") + genClassName.hashCode.abs
          optionsManager.setTargetDirName(s"${optionsManager.targetDirName}/$testerName")
        }
        val testerOptions = optionsManager.testerOptions

        val (dut, backend) = testerOptions.backendName match {
          case "firrtl" =>
            setupFirrtlTerpBackend(dutGenerator, optionsManager, firrtlSourceOverride)
          case "treadle" =>
            setupTreadleBackend(dutGenerator, optionsManager)
          case "verilator" =>
            setupVerilatorBackend(dutGenerator, optionsManager, firrtlSourceOverride)
          case "ivl" =>
            setupIVLBackend(dutGenerator, optionsManager)
          case "vcs" =>
            setupVCSBackend(dutGenerator, optionsManager)
          case "vsim" =>
            setupVSIMBackend(dutGenerator, optionsManager)
          case _ =>
            throw new Exception(s"Unrecognized backend name ${testerOptions.backendName}")
        }

        backendVar.withValue(Some(backend)) {
          try {
            testerGen(dut).finish
          } catch {
            case e: Throwable =>
              e.printStackTrace()
              backend match {
                case b: IVLBackend => TesterProcess.kill(b)
                case b: VCSBackend => TesterProcess.kill(b)
                case b: VSIMBackend => TesterProcess.kill(b)
                case b: VerilatorBackend => TesterProcess.kill(b)
                case _ =>
              }
              throw e
          }
        }
      }
    }
  }

  /**
   * This executes the test with options provide from an array of string -- typically provided from the
   * command line
   *
   * @param args       A *main* style array of string options
   * @param dut        The device to be tested, (device-under-test)
   * @param testerGen  A peek-poke tester with test for the dey
   * @return           Returns true if all tests in testerGen pass
   */
  def execute[T <: Module](args: Array[String], dut: () => T)(
    testerGen: T => PeekPokeTester[T]
  ): Boolean = {
    val optionsManager = new TesterOptionsManager

    optionsManager.parse(args) match {
      case true =>
        execute(dut, optionsManager)(testerGen)
      case _ =>
        false
    }
  }

  /**
   * Start up the interpreter repl with the given circuit
   * To test a `class X extends Module {}`, add the following code to the end
   * of the file that defines
   *
   * @example {{{
   *           object XRepl {
   *             def main(args: Array[String]) {
   *               val optionsManager = new ReplOptionsManager
   *               if(optionsManager.parse(args)) {
   *                 iotesters.Driver.executeFirrtlRepl(() => new X, optionsManager)
   *               }
   *             }
   * }}}
   * running main will place users in the repl with the circuit X loaded into the repl
   * @param dutGenerator   Module to run in interpreter
   * @param optionsManager options
   * @return
   */
  def executeFirrtlRepl[T <: Module](
    dutGenerator: () => T,
    optionsManager: ReplOptionsManager = new ReplOptionsManager): Boolean = {

    if (optionsManager.topName.isEmpty) {
      if (optionsManager.targetDirName == ".") {
        optionsManager.setTargetDirName("test_run_dir")
      }
      val genClassName = dutGenerator.getClass.getName
      val testerName = genClassName.split("""\$\$""").headOption.getOrElse("") + genClassName.hashCode.abs
      optionsManager.setTargetDirName(s"${optionsManager.targetDirName}/$testerName")
    }

    optionsManager.chiselOptions = optionsManager.chiselOptions.copy(runFirrtlCompiler = false)
    optionsManager.firrtlOptions = optionsManager.firrtlOptions.copy(compilerName = "low")

    Logger.makeScope(optionsManager) {
      val chiselResult: ChiselExecutionResult = DriverCompatibility.execute(optionsManager, dutGenerator)
      chiselResult match {
        case ChiselExecutionSuccess(_, emitted, _) =>
          optionsManager.replConfig = optionsManager.replConfig.copy(firrtlSource = emitted)
          FirrtlRepl.execute(optionsManager)
          true
        case ChiselExecutionFailure(message) =>
          println("Failed to compile circuit")
          false
      }
    }
  }
  /**
   * Start up the interpreter repl with the given circuit
   * To test a `class X extends Module {}`, add the following code to the end
   * of the file that defines
   * @example {{{
   *           object XRepl {
   *             def main(args: Array[String]) {
   *               iotesters.Driver.executeFirrtlRepl(args, () => new X)
   *             }
   *           }
   * }}}
   * running main will place users in the repl with the circuit X loaded into the repl
   *
   * @param dutGenerator   Module to run in interpreter
   * @param args           options from the command line
   * @return
   */
  def executeFirrtlRepl[T <: Module](
    args: Array[String],
    dutGenerator: () => T
  ): Boolean = {
    val optionsManager = new ReplOptionsManager

    if(optionsManager.parse(args)) {
      executeFirrtlRepl(dutGenerator, optionsManager)
    }
    else {
      false
    }
  }
  /**
   * This is just here as command line way to see what the options are
   * It will not successfully run
   * TODO: Look into dynamic class loading as way to make this main useful
   *
   * @param args unused args
   */
  def main(args: Array[String]) {
    execute(Array("--help"), null)(null)
  }
  /**
   * Runs the ClassicTester and returns a Boolean indicating test success or failure
   * @@backendType determines whether the ClassicTester uses verilator or the firrtl interpreter to simulate
   * the circuit.
   * Will do intermediate compliation steps to setup the backend specified, including cpp compilation for the
   * verilator backend and firrtl IR compilation for the firrlt backend
   *
   * This apply method is a convenient short form of the [[Driver.execute()]] which has many more options
   *
   * The following tests a chisel CircuitX with a CircuitXTester setting the random number seed to a fixed value and
   * turning on verbose tester output.  The result of the overall test is put in testsPassed
   *
   * @example {{{
   *           val testsPassed = iotesters.Driver(() => new CircuitX, testerSeed = 0L, verbose = true) { circuitX =>
   *             CircuitXTester(circuitX)
   *           }
   * }}}
   *

   * @param dutGen      This is the device under test.
   * @param backendType The default backend is "firrtl" which uses the firrtl interpreter. Other options
   *                    "verilator" will use the verilator c++ simulation generator
   *                    "ivl" will use the Icarus Verilog simulation
   *                    "vcs" will use the VCS simulation
   *                    "vsim" will use the ModelSim/QuestaSim simulation
   * @param verbose     Setting this to true will make the tester display information on peeks,
   *                    pokes, steps, and expects.  By default only failed expects will be printed
   * @param testerSeed  Set the random number generator seed
   * @param testerGen   This is a test harness subclassing PeekPokeTester for dutGen,
   * @return            This will be true if all tests in the testerGen pass
   */
  def apply[T <: Module](
    dutGen: () => T,
    backendType: String = "firrtl",
    verbose: Boolean = false,
    testerSeed: Long = System.currentTimeMillis())(
    testerGen: T => PeekPokeTester[T]): Boolean = {

    val optionsManager = new TesterOptionsManager {
      testerOptions = testerOptions.copy(backendName = backendType, isVerbose = verbose, testerSeed = testerSeed)
    }

    execute(dutGen, optionsManager)(testerGen)
  }

  /**
   * Runs the ClassicTester using the verilator backend without doing Verilator compilation and returns a Boolean indicating success or failure
   * Requires the caller to supply path the already compile Verilator binary
   */
  def run[T <: Module](dutGen: () => T, cmd: Seq[String])
    (testerGen: T => PeekPokeTester[T]): Boolean = {
    val circuit = chisel3.stage.ChiselStage.elaborate(dutGen())
    val dut = getTopModule(circuit).asInstanceOf[T]
    backendVar.withValue(Some(new VerilatorBackend(dut, cmd))) {
      try {
        testerGen(dut).finish
      } catch { case e: Throwable =>
        e.printStackTrace()
        backend match {
          case Some(b: IVLBackend) =>
            TesterProcess kill b
          case Some(b: VCSBackend) =>
            TesterProcess kill b
          case Some(b: VSIMBackend) =>
            TesterProcess kill b
          case Some(b: VerilatorBackend) =>
            TesterProcess kill b
          case _ =>
        }
        throw e
      }
    }
  }

  def run[T <: Module](dutGen: () => T, binary: String, args: String*)
    (testerGen: T => PeekPokeTester[T]): Boolean =
    run(dutGen, binary +: args.toSeq)(testerGen)

  def run[T <: Module](dutGen: () => T, binary: File, waveform: Option[File] = None)
    (testerGen: T => PeekPokeTester[T]): Boolean = {
    val args = waveform match {
      case None => Nil
      case Some(f) => Seq(s"+waveform=$f")
    }
    run(dutGen, binary.toString +: args.toSeq)(testerGen)
  }

  /** Filter a sequence of annotations, ensuring problematic potential duplicates are removed.
   * @param annotations Seq[Annotation] to be filtered
   * @return filtered Seq[Annotation]
   */
  def filterAnnotations(annotations: Seq[Annotation]): Seq[Annotation] = {
    annotations.filterNot {
      case _: firrtl.options.TargetDirAnnotation => true
      case _: logger.LogLevelAnnotation => true
      case _: firrtl.stage.FirrtlCircuitAnnotation => true
      case _: firrtl.stage.InfoModeAnnotation => true
      case _ => false
    }
  }
}

class ReplOptionsManager
  extends InterpreterOptionsManager
    with HasChiselExecutionOptions
    with HasReplConfig

