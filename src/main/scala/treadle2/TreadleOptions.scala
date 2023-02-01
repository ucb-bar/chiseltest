// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.CircuitState
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.ir.Circuit
import firrtl.options.{HasShellOptions, RegisteredLibrary, ShellOption, Unserializable}
import firrtl.stage.{FirrtlFileAnnotation, FirrtlSourceAnnotation}
import treadle2.blackboxes.BuiltInBlackBoxFactory
import treadle2.executable.{ClockInfo, DataStorePlugin, ExecutionEngine, TreadleException}
import treadle2.stage.phases.HandleFormalStatements

sealed trait TreadleOption extends Unserializable { this: Annotation => }

/** Tells treadle to write a vcd file during simulation
  */
case object WriteVcdAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-write-vcd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vcd execution log, filename will be based on top-name"
    )
  )
}

/** Tells treadle to include _T_* and _GEN_* wires in VCD output
  */
case object VcdShowUnderScoredAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-vcd-show-underscored-vars",
      toAnnotationSeq = _ => Seq(VcdShowUnderScoredAnnotation),
      helpText = "vcd output by default does not show var that start with underscore, this overrides that"
    )
  )
}

/** Tells treadle to write coverage report in CSV format after simulation
  */
case object WriteCoverageCSVAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-write-coverage-csv",
      toAnnotationSeq = _ => Seq(WriteCoverageCSVAnnotation),
      helpText = "writes coverage report in CSV format after simulation, filename will be based on top-name"
    )
  )
}

/**  Tells treadle to execute verbosely
  */
case object VerboseAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-verbose",
      toAnnotationSeq = _ => Seq(VerboseAnnotation),
      helpText = "makes the treadle very verbose"
    )
  )
}

/**  Tells treadle to allow cycles
  */
case object AllowCyclesAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-allow-cycle",
      toAnnotationSeq = _ => Seq(AllowCyclesAnnotation),
      helpText = "will try to run when firrtl contains combinational loops"
    )
  )
}

/**  Sets the seed for treadle's private random number generator
  */
case class RandomSeedAnnotation(seed: Long = 0L) extends NoTargetAnnotation with TreadleOption

object RandomSeedAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Long](
      longOption = "tr-random-seed",
      toAnnotationSeq = (seed: Long) => Seq(RandomSeedAnnotation(seed)),
      helpText = "sets the seed for Treadle's random number generator"
    )
  )
}

/**  Tells treadle to show the low firrtl it is starting out with
  */
case object ShowFirrtlAtLoadAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-show-firrtl-at-load",
      toAnnotationSeq = _ => Seq(ShowFirrtlAtLoadAnnotation),
      helpText = "show the low firrtl source treadle is using to build simulator"
    )
  )
}

/**  Tells treadle to show the low firrtl it is starting out with
  */
case object SaveFirrtlAtLoadAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-save-firrtl-at-load",
      toAnnotationSeq = _ => Seq(SaveFirrtlAtLoadAnnotation),
      helpText = "save the low firrtl source treadle is using to build simulator"
    )
  )
}

/**  Tells treadle to not run its own lowering pass on firrtl input (not recommended)
  */
case object DontRunLoweringCompilerLoadAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-dont-run-lower-compiler-on-load",
      toAnnotationSeq = _ => Seq(),
      helpText = "Deprecated: This option has no effect and will be removed in treadle 1.4"
    )
  )
}

/**  Tells treadle to present random value when validIf's condition is off
  */
case object ValidIfIsRandomAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-validif-random",
      toAnnotationSeq = _ => Seq(ValidIfIsRandomAnnotation),
      helpText = "validIf returns random value when condition is false"
    )
  )
}

/**  Sets verilog plus args that will be passed to black boxes
  */
case class PlusArgsAnnotation(plusArgs: Seq[String]) extends NoTargetAnnotation with TreadleOption

case object PlusArgsAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "tr-plus-args",
      toAnnotationSeq = (args: Seq[String]) => Seq(PlusArgsAnnotation(args)),
      helpText = s"a comma separated list of plusArgs"
    )
  )
}

/**  Controls whether changes to memory locations are written to vcd output
  *  @param specifier controls which memories and which locations of those memories are logged to vcd output
  *                   When not present not memories are logged
  *                   "all"             log all values at all locations of all memories
  *                   "mem1:all"        log all values at all locations for memory mem1
  *                   "mem1:0-4"        log values at locations 0-4 for memory mem1
  *                   "mem1:b0-b100"    log values at locations 0-4 but show addresses in binary for memory mem1
  *                   "mem1:h0-hff"     log values at locations 0-255 but show addresses in hex for memory mem1
  *                   "mem1:o0-o377"    log values at locations 0-255 but show addresses in octal for memory mem1
  *
  * This annotation may occur more than once in order to specify multiple memories
  */
case class MemoryToVCD(specifier: String) extends NoTargetAnnotation with TreadleOption

case object MemoryToVCD extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-mem-to-vcd",
      toAnnotationSeq = (specifier: String) => Seq(MemoryToVCD(specifier)),
      helpText = s"""log specified memory/indices to vcd, format "all" or "memoryName:1,2,5-10" """
    )
  )
}

/** Controls whether coverage information will be gathered or not during the execution of a test.
  */
case object EnableCoverageAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-enable-coverage",
      toAnnotationSeq = _ => Seq(EnableCoverageAnnotation),
      helpText = s"""Enables automatic line coverage on tests"""
    )
  )
}

/**  Sets one or more clocks including their frequencies and phase
  */
case class ClockInfoAnnotation(clockInfoSeq: Seq[ClockInfo] = Seq(ClockInfo()))
    extends NoTargetAnnotation
    with TreadleOption

case object ClockInfoAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "tr-clock-info",
      toAnnotationSeq = (s: Seq[String]) => Seq(ClockInfoAnnotation(s.map(parseClockInfo))),
      helpText = "comma separated list of clock-name[:period[:initial-offset]]"
    )
  )

  def parseClockInfo(input: String): ClockInfo = {
    input.split(":").map(_.trim).toList match {
      case name :: Nil =>
        ClockInfo(name)
      case name :: period :: Nil =>
        ClockInfo(name, period.toLong)
      case name :: period :: offset :: Nil =>
        ClockInfo(name, period.toLong, offset.toLong)
      case _ =>
        throw TreadleException(s"Bad clock info string $input, should be name[:period[:offset]]")
    }
  }
}

/**  Sets a list of symbols that will be rendered during execution
  */
case class SymbolsToWatchAnnotation(symbolNames: Seq[String] = Seq.empty) extends NoTargetAnnotation with TreadleOption

case object SymbolsToWatchAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "tr-symbols-to-watch",
      toAnnotationSeq = (names: Seq[String]) => Seq(SymbolsToWatchAnnotation(names)),
      helpText = "symbol[,symbol[...]"
    )
  )
}

/**  used with treadle's default reset operations
  */
case class ResetNameAnnotation(symbolNames: String = "") extends NoTargetAnnotation with TreadleOption

case object ResetNameAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-reset-name",
      toAnnotationSeq = (resetName: String) => Seq(ResetNameAnnotation(resetName)),
      helpText = "name of the default reset signal"
    )
  )
}

/**  Tells treadle to Randomize circuit at startup
  */
case object RandomizeAtStartupAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-randomize-at-startup",
      toAnnotationSeq = _ => Seq(RandomizeAtStartupAnnotation),
      helpText = "makes treadle do it's own randomization of circuit at startup"
    )
  )
}

/**  Tell treadle to call it's own internal reset at startup. This is typically handled by the
  *  unit test framework and not needed for users
  */
case object CallResetAtStartupAnnotation extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-call-reset-at-startup",
      toAnnotationSeq = _ => Seq(CallResetAtStartupAnnotation),
      helpText = "makes treadle do it's own reset at startup, usually for internal use only"
    )
  )
}

/**  Tells treadle to prefix printf strings with a wall time
  */
case object PrefixPrintfWithWallTime extends NoTargetAnnotation with TreadleOption with HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-prefix-printf-with-walltime",
      toAnnotationSeq = _ => Seq(PrefixPrintfWithWallTime),
      helpText = """Adds a string "[<wall-time>]" to the front of printf lines, helps match to vcd"""
    )
  )
}

/** The circuit used to build a [[TreadleTester]]
  * @param circuit a firrtl ast
  */
case class TreadleCircuitAnnotation(circuit: Circuit) extends NoTargetAnnotation with TreadleOption

/** used to pass parsed firrtl to the TreadleTester
  * @param state the state to be passed along
  */
case class TreadleCircuitStateAnnotation(state: CircuitState) extends NoTargetAnnotation

/** Provides an input form hint to treadle to know how to best handle the input it receives
  *
  * @param form the input form
  */
@deprecated("Remove references, this has no effect", since = "1.3.x")
case class TreadleFirrtlFormHint(form: Any) extends NoTargetAnnotation

@deprecated("Remove references, this has no effect", since = "1.3.x")
object TreadleFirrtlFormHint extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-firrtl-input-form",
      toAnnotationSeq = (firrtl: String) => {
        Seq()
      },
      helpText = "Deprecated: This option has no effect and will be removed in treadle 1.4"
    )
  )
}

/** Adds the treadle blackboxes for rocket black box factory
  */
object TreadleRocketBlackBoxes extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Unit](
      longOption = "tr-add-rocket-black-boxes",
      toAnnotationSeq = _ => Seq(BlackBoxFactoriesAnnotation(Seq(new BuiltInBlackBoxFactory))),
      helpText = "add in the black boxes needed to simulate rocket"
    )
  )
}

/** Used to pass a tester on to a test harness
  *
  * @param tester The [[TreadleTester]] to be passed on
  */
case class TreadleTesterAnnotation(tester: TreadleTester) extends NoTargetAnnotation with TreadleOption

/** Factory for FirrtlSourceAnnotation, this is an alias for FirrtlCli
  */
object TreadleFirrtlString extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-firrtl-source-string",
      toAnnotationSeq = (firrtl: String) => Seq(FirrtlSourceAnnotation(firrtl)),
      helpText = "a serialized firrtl circuit, mostly used internally"
    )
  )
}

/** Factory for FirrtlFileAnnotation annotation, this is an alias for Firrtl Cli
  */
object TreadleFirrtlFile extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "tr-firrtl-source-file",
      shortOption = Some("tfsf"),
      toAnnotationSeq = (firrtl: String) => Seq(FirrtlFileAnnotation(firrtl)),
      helpText = "specify treadle repl source file"
    )
  )
}

case class BlackBoxFactoriesAnnotation(blackBoxFactories: Seq[ScalaBlackBoxFactory])
    extends NoTargetAnnotation
    with TreadleOption

/** Using this annotation allows external users of a TreadleTester to supply their own custom
  * [[treadle2.executable.DataStorePlugin]]s. See that code for examples of use.
  *
  * @note this annotation cannot be generated from the command line
  * @param name      A unique name for this plugin
  * @param getPlugin A function that returns a DataStorePlugin subclass
  */
case class DataStorePlugInAnnotation(
  name:      String,
  getPlugin: ExecutionEngine => DataStorePlugin)
    extends NoTargetAnnotation
    with TreadleOption

/** Constructs this as a registered library that will be auto-detected by
  * projects who have a dependency on Treadle
  */
class TreadleLibrary extends RegisteredLibrary {
  val name: String = "treadle2"

  val options: Seq[ShellOption[_]] = Seq(
    WriteVcdAnnotation,
    VcdShowUnderScoredAnnotation,
    VerboseAnnotation,
    AllowCyclesAnnotation,
    RandomSeedAnnotation,
    ShowFirrtlAtLoadAnnotation,
    SaveFirrtlAtLoadAnnotation,
    DontRunLoweringCompilerLoadAnnotation,
    ValidIfIsRandomAnnotation,
    MemoryToVCD,
    ClockInfoAnnotation,
    SymbolsToWatchAnnotation,
    ResetNameAnnotation,
    RandomizeAtStartupAnnotation,
    CallResetAtStartupAnnotation,
    TreadleRocketBlackBoxes,
    PrefixPrintfWithWallTime,
    TreadleFirrtlString,
    TreadleFirrtlFile,
    new HandleFormalStatements,
    EnableCoverageAnnotation
  ).flatMap(_.options)
}

object TreadleDefaults {
  val RollbackBuffers = 0
}
