// SPDX-License-Identifier: Apache-2.0

package treadle

import java.io.{File, PrintWriter}

import treadle.utils.NumberHelpers._

import firrtl.AnnotationSeq
import firrtl.graph.{CyclicException, DiGraph}
import firrtl.options.Viewer.view
import firrtl.options.{OptionsException, StageOptions, StageUtils, TargetDirAnnotation}
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlSourceAnnotation, OutputFileAnnotation}
import org.json4s.native.JsonMethods._
import treadle.chronometry.UTC
import treadle.executable.{ExecutionEngine, RenderComputations, Symbol, SymbolTable, TreadleException, WaveformValues}
import treadle.repl._
import treadle.stage.TreadleTesterPhase
import treadle.vcd.VCD

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.completer._
import scala.tools.jline.console.history.FileHistory
import scala.tools.jline.{Terminal, TerminalFactory}
import scala.util.matching.Regex

abstract class Command(val name: String) {
  def run(args: Array[String]): Unit
  def usage: (String, String)
  def completer: Option[ArgumentCompleter] = {
    Some(
      new ArgumentCompleter(
        new StringsCompleter({ name })
      )
    )
  }
}

/** Considered by many to be the world's best Treadle Repl
  * @param initialAnnotations initial settings.
  */
class TreadleRepl(initialAnnotations: AnnotationSeq) {
  var annotationSeq: AnnotationSeq = initialAnnotations
  var stageOptions:  StageOptions = view[StageOptions](annotationSeq)

  val terminal: Terminal = TerminalFactory.create()
  val console: ConsoleReader = annotationSeq.collectFirst { case OverrideOutputStream(s) => s } match {
    case Some(stream) => new ConsoleReader(System.in, stream)
    case _            => new ConsoleReader
  }

  // creates a treadle repl history in the current directory
  // makes it nicer when switching between projects
  private val historyPath = ".treadle_repl_history"
  val historyFile = new File(historyPath)
  val history = new FileHistory(historyFile)

  try {
    if (!historyFile.exists()) {
      println(s"creating ${historyFile.getName}")
      historyFile.createNewFile()
    }
    history.load(historyFile)
    console.setHistory(history)
  } catch {
    case e: Exception =>
      // ignore problems with history file, better to run than freak out over this.
      println(s"Error creating history file: message is ${e.getMessage}. History of commands will not be saved")
  }

  var currentTreadleTesterOpt: Option[TreadleTester] = None
  def currentTreadleTester:    TreadleTester = currentTreadleTesterOpt.get

  def engine: ExecutionEngine = currentTreadleTesterOpt match {
    case Some(tester) => tester.engine
    case _ =>
      throw TreadleException(s"No file currently loaded")
  }

  var args = Array.empty[String]
  var done = false

  var inScript = false
  val scriptFactory: ScriptFactory = ScriptFactory(this)
  var currentScript: Option[Script] = None
  val IntPattern:    Regex = """(-?\d+)""".r

  var currentSymbols: String = ""

  var currentVcdScript:  Option[VCD] = None
  var replVcdController: Option[ReplVcdController] = None

  var outputFormat: String = annotationSeq.collectFirst { case TreadleReplDisplayFormat(format) => format }
    .getOrElse("d")

  def formatOutput(value: BigInt): String = {
    outputFormat match {
      case "d"       => value.toString
      case "h" | "x" => f"0x$value%x"
      case "b"       => s"b${value.toString(2)}"
    }
  }

  def showNameAndValue(symbolName: String, offset: Int = 0): String = {
    if (engine.inputsChanged) {
      engine.evaluateCircuit()
    }
    engine.symbolTable.get(symbolName) match {
      case Some(symbol) if symbol.forcedValue.isDefined =>
        s"$symbolName ${formatOutput(symbol.forcedValue.get)} [forced]"
      case Some(_) =>
        s"$symbolName ${formatOutput(engine.getValue(symbolName, offset))}"
      case _ =>
        s"Could not find symbol $symbolName"

    }
  }

  def loadSource(): Unit = {

    currentTreadleTesterOpt = annotationSeq.collectFirst { case TreadleTesterAnnotation(t) => t }
    buildCompletions()
  }

  def loadFile(fileName: String): Unit = {
    var file = new File(fileName)
    if (!file.exists()) {
      file = new File(fileName + ".fir")
      if (!file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    val sourceReader = io.Source.fromFile(file)
    val input = sourceReader.mkString
    sourceReader.close()

    annotationSeq = (new TreadleTesterPhase).transform(annotationSeq.filter {
      case _: OutputFileAnnotation          => false
      case _: FirrtlCircuitAnnotation       => false
      case _: FirrtlSourceAnnotation        => false
      case _: TreadleTesterAnnotation       => false
      case _: TreadleCircuitStateAnnotation => false
      case _ => true
    } :+ FirrtlSourceAnnotation(input))

    currentTreadleTesterOpt = None
    loadSource()
  }

  /** First try and read script file in the target dir, if that fails try opening it in place
    * @param fileName name of script file
    */
  def loadScript(fileName: String): Unit = {
    annotationSeq.collectFirst { case TargetDirAnnotation(t) => t } match {
      case Some(targetDir) =>
        if (!fileName.startsWith(targetDir)) {
          currentScript = scriptFactory(targetDir + File.separator + fileName)
        }
        if (currentScript.isEmpty) {
          currentScript = scriptFactory(fileName)
        }
      case _ =>
    }

    currentScript match {
      case Some(script) =>
        console.println(s"loaded script file ${script.fileName} with ${script.length} lines")
      case _ =>
        console.println(s"unable to open script file '$fileName")
    }
  }

  def loadVcdScript(fileName: String): Unit = {
    val dutName = currentTreadleTesterOpt match {
      case Some(tester) => tester.engine.ast.main
      case None         => ""
    }
    try {
      currentVcdScript = Some(VCD.read(fileName, dutName))
      replVcdController = Some(new ReplVcdController(this, this.engine, currentVcdScript.get))
      println(s"vcd script $fileName loaded")
    } catch {
      case e: Exception =>
        console.println(s"Failed to load vcd script $fileName, error: ${e.getMessage}")
    }
  }

  val wallTime: UTC = UTC()
  wallTime.onTimeChange = () => {
    engine.vcdOption.foreach { vcd =>
      vcd.setTime(wallTime.currentTime)
    }
  }

  val combinationalDelay: Long = 10

  def reset(timeRaised: Long): Unit = {
    engine.setValue(currentTreadleTester.resetName, 1)
    engine.inputsChanged = true

    wallTime.addOneTimeTask(wallTime.currentTime + timeRaised, "reset-task") { () =>
      engine.setValue(currentTreadleTester.resetName, 0)
      if (engine.verbose) {
        println(s"reset dropped at ${wallTime.currentTime}")
      }
      engine.inputsChanged = true
    }
  }

  var cycleCount: Long = 0L

  /** Cycles the circuit n steps (with a default of one)
    * At each step registers and memories are advanced and all other elements recomputed
    *
    * @param n cycles to perform
    */
  def step(n: Int = 1): Unit = {
    currentTreadleTester.step(n)
  }

  // scalastyle:off number.of.methods
  object Commands {
    def getOneArg(failureMessage: String, argOption: Option[String] = None): Option[String] = {
      if (args.length == 2) {
        Some(args(1))
      } else if (args.length == 1 && argOption.isDefined) {
        Some(argOption.get)
      } else {
        if (failureMessage.nonEmpty) {
          error(failureMessage)
        }
        None
      }
    }
    def getTwoArgs(
      failureMessage: String,
      arg1Option:     Option[String] = None,
      arg2Option:     Option[String] = None
    ): (Option[String], Option[String]) = {
      if (args.length == 3) {
        (Some(args(1)), Some(args(2)))
      } else if (args.length == 2) {
        (Some(args(1)), arg2Option)
      } else if (args.length == 1) {
        (arg1Option, arg2Option)
      } else {
        error(failureMessage)
        (None, None)
      }
    }
    //scalastyle:off magic.number
    def getThreeArgs(
      failureMessage: String,
      arg1Option:     Option[String] = None,
      arg2Option:     Option[String] = None,
      arg3Option:     Option[String] = None
    ): (Option[String], Option[String], Option[String]) = {
      if (args.length == 4) {
        (Some(args(1)), Some(args(2)), Some(args(3)))
      } else if (args.length == 3) {
        (Some(args(1)), Some(args(2)), arg3Option)
      } else if (args.length == 2) {
        (Some(args(1)), arg2Option, arg3Option)
      } else if (args.length == 1) {
        (arg1Option, arg2Option, arg3Option)
      } else {
        error(failureMessage)
        (None, None, None)
      }
    }

    //scalastyle:off magic.number
    def getManyArgs(defaults: Option[String]*): List[Option[String]] = {
      val combined: Seq[(Option[String], Option[String])] = args.tail.map(Some(_)).zipAll(defaults, None, None)

      val result = combined.map { case (command, default) => if (command.isDefined) command else default }

      result.toList
    }

    val commands: ArrayBuffer[Command] = ArrayBuffer.empty[Command]
    commands ++= Seq(
      new Command("load") {
        def usage: (String, String) = ("load fileName", "load/replace the current firrtl file")
        override def completer: Option[ArgumentCompleter] = {
          Some(
            new ArgumentCompleter(
              new StringsCompleter({ "load" }),
              new FileNameCompleter
            )
          )
        }
        def run(args: Array[String]): Unit = {
          getOneArg("load filename") match {
            case Some(fileName) => loadFile(fileName)
            case _              =>
          }
        }
      },
      new Command("script") {
        def usage: (String, String) = ("script fileName", "load a script from a text file")
        override def completer: Option[ArgumentCompleter] = {
          Some(
            new ArgumentCompleter(
              new StringsCompleter({ "script" }),
              new FileNameCompleter
            )
          )
        }
        def run(args: Array[String]): Unit = {
          getOneArg("script fileName") match {
            case Some(fileName) => loadScript(fileName)

            case _ =>
          }
        }
      },
      new Command("run") {
        def usage: (String, String) = ("run [linesToRun|all|list|reset]", "run loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(
            new ArgumentCompleter(
              new StringsCompleter({ "run" }),
              new StringsCompleter(jlist(Seq("all", "reset", "list")))
            )
          )
        }
        def handleList(script: Script, listArg: Option[String]): Unit = {
          val (min, max) = listArg match {
            case Some(IntPattern(intString)) =>
              val windowHalfSize = intString.toInt
              (script.currentLine + 1 - windowHalfSize, script.currentLine + 2 + windowHalfSize)
            case Some(other) =>
              console.println(s"run list parameter=$other, parameter must be an positive integer")
              (0, 0)
            case _ =>
              (0, script.length)
          }
          console.println(
            script.lines.zipWithIndex.flatMap { case (line, index) =>
              if (index >= min && index < max) {
                if (index == script.currentLine + 1) {
                  Some(Console.GREEN + f"$index%3d $line" + Console.RESET)
                } else {
                  Some(f"$index%3d $line")
                }
              } else {
                None
              }
            }.mkString("\n")
          )
        }
        // scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          currentScript match {
            case Some(script) =>
              getTwoArgs(
                "run [lines|skip [n]|set n|all|reset|list [n]], default is 1 => run 1 line",
                arg1Option = Some("1"),
                arg2Option = None
              ) match {
                case (Some("all"), _) =>
                  console.println("run all")
                  if (script.atEnd) { script.reset() }
                  else { script.runRemaining() }
                case (Some("reset"), _) =>
                  script.reset()
                  handleList(script, Some("2"))
                case (Some("list"), listArg) =>
                  handleList(script, listArg)
                case (Some("skip"), listArg) =>
                  val skip = listArg match {
                    case Some(IntPattern(intString)) => intString.toInt
                    case _                           => 1
                  }
                  script.setSkipLines(skip)
                case (Some("set"), listArg) =>
                  listArg match {
                    case Some(IntPattern(intString)) =>
                      script.setLine(intString.toInt)
                      handleList(script, Some("2"))
                    case _ =>
                      console.println("must specify set line number")
                  }
                case (Some(IntPattern(intString)), _) =>
                  val linesToRun = intString.toInt
                  script.setLinesToRun(linesToRun)
                case (None, None) =>
                  script.runRemaining()
                case (Some(arg), _) =>
                  error(s"unrecognized run_argument $arg")
              }
            case _ =>
              error(s"No current script")
          }
        }
        // scalastyle:on cyclomatic.complexity

      },
      new Command("vcd") {
        def usage: (String, String) = ("vcd [load|run|list|test|help]", "control vcd input file")
        override def completer: Option[ArgumentCompleter] = {
          Some(
            new ArgumentCompleter(
              new StringsCompleter({ "vcd" }),
              new AggregateCompleter(
                new StringsCompleter(jlist(Seq("run", "inputs", "list", "test"))),
                new ArgumentCompleter(
                  new StringsCompleter({ "load" }),
                  new FileNameCompleter
                )
              )
            )
          )
        }
        def run(args: Array[String]): Unit = {
          args.toList match {
            case "load" :: fileName :: _ =>
              loadVcdScript(fileName)
            case _ =>
              replVcdController match {
                case Some(controller) => controller.processListCommand(args)
                case _                => error(s"No current script")
              }
          }
        }
      },
      new Command("record-vcd") {
        def usage: (String, String) = ("record-vcd [<fileName>]|[done]", "treadle.vcd loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(
            new ArgumentCompleter(
              new StringsCompleter({ "record-vcd" }),
              new FileNameCompleter
            )
          )
        }
        def run(args: Array[String]): Unit = {
          getOneArg("treadle.vcd [fileName|done]", argOption = Some("out.treadle.vcd")) match {
            case Some("done") =>
              engine.disableVCD()
            case Some(fileName) =>
              engine.makeVCDLogger(fileName, showUnderscored = currentTreadleTester.vcdShowUnderscored)
            case _ =>
              engine.disableVCD()
          }
        }
      },
      new Command("symbol") {
        private def peekableThings = engine.validNames.toSeq
        def usage: (String, String) = ("symbol regex", "show symbol information")

        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "symbol" }),
                new StringsCompleter(jlist(peekableThings))
              )
            )
          }
        }

        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          var linesShown = 0
          getOneArg("symbol regex") match {
            case Some(peekRegex) =>
              try {
                val portRegex = peekRegex.r
                val numberOfThingsPeeked = peekableThings.sorted.count { signal =>
                  portRegex.findFirstIn(signal) match {
                    case Some(_) =>
                      try {
                        val value = engine.getValue(signal)
                        val symbol = engine.symbolTable(signal)
                        if (linesShown % 50 == 0) {
                          console.println(s"${Symbol.renderHeader}")
                        }
                        linesShown += 1
                        console.println(s"${symbol.render} ${formatOutput(value)}")
                        true
                      } catch { case _: Exception => false }
                    case _ =>
                      false
                  }
                }
                if (numberOfThingsPeeked == 0) {
                  console.println(s"Sorry no settable ports matched regex $peekRegex")
                }
              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("watch") {
        private def peekableThings: Seq[String] = engine.validNames.toSeq
        def usage:                  (String, String) = ("watch [+|-] regex [regex ...]", "watch (+) or unwatch (-) signals")

        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "watch" }),
                new StringsCompleter(jlist(peekableThings))
              )
            )
          }
        }

        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          var isAdding = true

          engine.dataStore.plugins.get("show-computation") match {
            case Some(plugin: RenderComputations) =>
              args.foreach {
                case "-" => isAdding = false
                case "+" => isAdding = true
                case symbolPattern =>
                  try {
                    val portRegex = symbolPattern.r
                    peekableThings.foreach { signalName =>
                      portRegex.findFirstIn(signalName) match {
                        case Some(_) =>
                          try {
                            val value = engine.getValue(signalName)
                            val symbol = engine.symbolTable(signalName)
                            if (isAdding) {
                              plugin.symbolsToWatch += symbol
                              console.println(s"add watch for ${symbol.render} ${formatOutput(value)}")
                            } else {
                              plugin.symbolsToWatch -= symbol
                              console.println(s"removing watch for ${symbol.render} ${formatOutput(value)}")
                            }
                          } catch {
                            case _: Exception => false
                          }
                        case _ =>
                          false
                      }
                    }
                  } catch {
                    case e: Exception =>
                      error(s"exception ${e.getMessage} $e")
                    case a: AssertionError =>
                      error(s"exception ${a.getMessage}")
                  }
              }
              plugin.setEnabled(plugin.symbolsToWatch.nonEmpty)
              engine.setLeanMode()

            case _ =>
              console.println(s"Can't find watch plugin-in")
          }
        }
      },
      new Command("poke") {
        def usage: (String, String) = ("poke inputSymbol value", "set an input port to the given integer value")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "poke"
                }),
                new StringsCompleter(
                  jlist(engine.getInputPorts ++ engine.getRegisterNames)
                )
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("poke inputSymbol value") match {
            case (Some(portName), Some(valueString)) =>
              try {
                val numberValue = parseBigInt(valueString)
                engine.setValue(portName, numberValue)
              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("force") {
        def usage: (String, String) =
          ("force symbol value", "hold a wire to value (use value clear to clear forced wire)")

        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "force"
                }),
                new StringsCompleter(
                  jlist(engine.getInputPorts ++ engine.getRegisterNames)
                )
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("force symbol value (use value clear to clear forced wire)") match {
            case (Some(portName), Some(valueString)) =>
              try {
                if (valueString.toLowerCase == "clear") {
                  currentTreadleTester.clearForceValue(portName)
                } else {
                  val numberValue = parseBigInt(valueString)
                  currentTreadleTester.forceValue(portName, numberValue)
                }
              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case (None, None) =>
              val forcedList = engine.symbolTable.nameToSymbol.keys.toSeq.sorted.flatMap { key =>
                val symbol = engine.symbolTable(key)
                symbol.forcedValue.map { _ =>
                  showNameAndValue(symbol.name)
                }
              }
              if (forcedList.nonEmpty) {
                console.println(forcedList.mkString("\n"))
              } else {
                console.println("Currently no wires are forced")
              }
            case _ =>
          }
        }
      },
      new Command("rpoke") {
        private def settableThings = {
          engine.getInputPorts ++ engine.getRegisterNames
        }
        def usage: (String, String) = ("rpoke regex value", "poke value into portSymbols that match regex")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "rpoke"
                }),
                new StringsCompleter(jlist(settableThings))
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("rpoke regex value") match {
            case (Some(pokeRegex), Some(valueString)) =>
              try {
                val pokeValue = parseBigInt(valueString)
                val portRegex = pokeRegex.r
                val setThings = settableThings.flatMap { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      engine.setValue(settableThing, pokeValue)
                      Some(settableThing)
                    case _ => None
                  }
                }
                if (setThings.nonEmpty) {
                  console.println(s"poking value $pokeValue into ${setThings.toList.sorted.mkString(", ")}")
                } else {
                  console.println(s"Sorry no inputSymbols matched regex $pokeRegex")
                }

              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("peek") {
        def usage: (String, String) =
          ("peek symbol [offset]", "show the current value of the signal")

        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "peek"
                }),
                new StringsCompleter(jlist(engine.validNames.toSeq))
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("peek symbol", arg2Option = Some("0")) match {
            case (Some(symbol), Some(offsetString)) =>
              try {
                val offset = offsetString.headOption match {
                  case Some('b') => BigInt(offsetString.tail, 2).toInt
                  case Some('d') => BigInt(offsetString.tail, 10).toInt
                  case Some('x') => BigInt(offsetString.tail, 16).toInt
                  case Some('h') => BigInt(offsetString.tail, 16).toInt
                  case _         => offsetString.toInt
                }
                console.println(s"peek ${showNameAndValue(symbol, offset)}")
              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage}")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
              println(s"You must specify a signal name")
          }
        }
      },
      new Command("rpeek") {
        private def peekableThings = engine.validNames.toSeq
        def usage: (String, String) = ("rpeek regex", "show the current value of symbols matching the regex")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "rpeek"
                }),
                new StringsCompleter(jlist(peekableThings))
              )
            )
          }
        }
        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          getOneArg("rpeek regex") match {
            case Some(peekRegex) =>
              try {
                val portRegex = peekRegex.r
                val numberOfThingsPeeked = peekableThings.sorted.count { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      try {
                        console.println(showNameAndValue(settableThing))
                        true
                      } catch { case _: Exception => false }
                    case _ =>
                      false
                  }
                }
                if (numberOfThingsPeeked == 0) {
                  console.println(s"Sorry no symbols matched regex $peekRegex")
                }
              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("randomize") {
        def usage: (String, String) = ("randomize", "randomize all registers and memory values)")
        def run(args: Array[String]): Unit = {
          getOneArg("randomize [seed]", Some("0")) match {
            case Some(additionalSeedString) =>
              val additionalSeed = additionalSeedString.toLong
              engine.randomize(additionalSeed)
            case _ =>
          }
        }
      },
      new Command("reset") {
        def usage: (String, String) =
          ("reset [numberOfSteps]", "assert reset (if present) for numberOfSteps (default 1)")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "reset"
                })
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("reset [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                currentTreadleTester.clockInfoList.headOption match {
                  case Some(clockInfo) =>
                    val extraTime = clockInfo.period * numberOfStepsString.toInt
                    reset(clockInfo.initialOffset + extraTime)
                    wallTime.runToTask("reset-task")
                  case _ =>
                    engine.setValue("reset", 1)
                    engine.advanceTime(combinationalDelay)
                    engine.setValue("reset", 0)
                }
              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("radix") {
        def usage: (String, String) = ("reset [b|d|x|h]", "Set the output radix to binary, decimal, or hex")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "radix"
                }),
                new StringsCompleter(jlist(Seq("b", "d", "h", "x")))
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("radix [b|d|h|x]", Some("d")) match {
            case Some(radix) =>
              if (Seq("b", "d", "h", "x").contains(radix.toLowerCase())) {
                outputFormat = radix
              } else {
                console.println(s"Unknown output radix $radix")
              }

            case _ =>
          }
        }
      },
      new Command("step") {
        def usage: (String, String) =
          ("step [numberOfSteps]", "cycle the clock numberOfSteps (default 1) times, and show state")
        def run(args: Array[String]): Unit = {
          getOneArg("step [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                val numberOfSteps = numberOfStepsString.toInt
                engine.timer("steps") {
                  for (_ <- 0 until numberOfSteps) {
                    engine.timer("step") {
                      step()
                    }
                  }
                }
                if (!scriptRunning) {
                  // console.println(engine.circuitState.prettyString())
                  console.println(s"step $numberOfSteps in ${engine.timer.prettyLastTime("steps")}")
                }
              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("waitfor") {
        def usage: (String, String) =
          (
            "waitfor symbol value [maxNumberOfSteps]",
            "wait for particular value (default 1) of symbol, up to maxNumberOfSteps (default 100)"
          )

        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "waitfor"
                }),
                new StringsCompleter(jlist(engine.validNames.toSeq))
              )
            )
          }
        }

        def run(args: Array[String]): Unit = {
          getThreeArgs(
            "waitfor symbol [value] [maxNumberOfSteps]",
            arg2Option = Some("1"),
            arg3Option = Some("100")
          ) match {
            case (Some(symbol), Some(valueString), Some(maxNumberOfStepsString)) =>
              try {
                val maxNumberOfSteps = maxNumberOfStepsString.toInt
                val value = valueString.toInt

                var tries = 0
                while (tries < maxNumberOfSteps && engine.getValue(symbol) != BigInt(value)) {
                  step()
                  tries += 1
                }
                if (engine.getValue(symbol) != BigInt(value)) {
                  console.println(
                    s"waitfor exhausted $symbol did not take on" +
                      s" value ${formatOutput(value)} in $maxNumberOfSteps cycles"
                  )
                } else {
                  console.println(s"$symbol == value ${formatOutput(value)} in $tries cycles")
                }
              } catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("depend") {
        def usage: (String, String) =
          (
            "depend [childrenOf|parentsOf] signal [depth] | depend compare symbol1 symbol2",
            "show dependency relationship to symbol or between two symbols"
          )

        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            def peekableThings = engine.validNames.toSeq

            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "depend" }),
                new StringsCompleter(jlist(Seq("childrenOf", "parentsOf", "compare"))),
                new StringsCompleter(jlist(peekableThings)),
                new StringsCompleter(jlist(peekableThings))
              )
            )
          }
        }

        def showRelated(direction: String, digraph: DiGraph[Symbol], symbolName: String, maxDepth: Int): Unit = {
          val table = engine.symbolTable
          val symbol = engine.symbolTable(symbolName)
          val symbolsAtDepth = Array.fill(maxDepth + 1) {
            new mutable.HashSet[Symbol]
          }

          walkGraph(symbol, depth = 0)

          def walkGraph(symbol: Symbol, depth: Int): Unit = {
            symbolsAtDepth(depth) += symbol

            if (depth < maxDepth) {
              digraph.getEdges(symbol).toSeq.sortBy(_.name).foreach { childSymbol =>
                walkGraph(childSymbol, depth + 1)
              }
              if (table.isRegister(symbol.name)) {
                walkGraph(table(SymbolTable.makeRegisterInputName(symbol)), depth + 1)
              }
            }
          }

          val showDepth = symbolsAtDepth.count(_.nonEmpty)
          for (depth <- 0 until showDepth) {
            println(s"$direction symbols at distance $depth")
            println(symbolsAtDepth(depth).toSeq.map(_.name).sorted.mkString("\n"))
          }
        }

        def run(args: Array[String]): Unit = {
          val table = engine.symbolTable
          val parsedArgs = getManyArgs(Some("parentsOf"), None, Some("4"))
          parsedArgs match {
            case Some("parentsOf") :: Some(symbol1) :: Some(depth) :: _ =>
              showRelated("Parents", table.parentsOf, symbol1, maxDepth = depth.toInt)
            case Some("parentsOf") :: _ =>
              console.println(s"""You must specify a symbol with command "depend parentsOf" """)
            case Some("childrenOf") :: Some(symbol1) :: Some(depth) :: _ =>
              showRelated("Children", table.childrenOf, symbol1, maxDepth = depth.toInt)
            case Some("childrenOf") :: _ =>
              console.println(s"""You must specify a symbol with command "depend childrenOf" """)
            case Some("compare") :: Some(symbolName1) :: Some(symbolName2) :: _ =>
              val (symbol1, symbol2) = (table(symbolName1), table(symbolName2))
              def showPath(direction: String, digraph: DiGraph[Symbol]): Unit = {
                try {
                  val path = digraph.path(symbol1, symbol2)
                  console.println(s"$symbol1 is a $direction of $symbol2 via")
                  path.foreach { symbol =>
                    console.println(f"${symbol.name}")
                  }
                } catch {
                  case _: firrtl.graph.PathNotFoundException =>
                    console.println(s"$symbol1 is not a $direction of $symbol2")
                }
              }
              showPath("parent", table.parentsOf)
              showPath("child", table.childrenOf)
            case _ =>
              println(usage)

          }
        }
      },
      new Command("show") {
        def usage: (String, String) = ("show [state|inputs|outputs|clocks|firrtl|lofirrtl]", "show useful things")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "show" }),
                new StringsCompleter(jlist(Seq("state", "inputs", "outputs", "clocks", "firrtl", "lofirrtl")))
              )
            )
          }
        }

        def run(args: Array[String]): Unit = {
          def showValue(symbolName: String): String = {
            showNameAndValue(symbolName)
          }

          def getSource: String = {
            annotationSeq.collectFirst { case FirrtlSourceAnnotation(firrtl) =>
              firrtl
            }.getOrElse {
              annotationSeq.collectFirst { case FirrtlCircuitAnnotation(circuit) =>
                circuit.serialize
              }.getOrElse {
                annotationSeq.collectFirst { case TreadleCircuitStateAnnotation(state) =>
                  state.circuit.serialize
                }.getOrElse {
                  "Can't find initial firrtl. Working low firrtl is\n" + engine.ast.serialize
                }
              }
            }
          }

          getOneArg("", None) match {
            case Some("lofirrtl") =>
              console.println(engine.ast.serialize)
            case Some("input") | Some("firrtl") =>
              console.println(getSource)
            case Some("inputs") =>
              console.println(engine.symbolTable.inputPortsNames.toSeq.sorted.map(showValue).mkString("\n"))
            case Some("outputs") =>
              console.println(engine.symbolTable.outputPortsNames.toSeq.sorted.map(showValue).mkString("\n"))
            case Some("clocks") =>
              console.println(currentTreadleTester.clockInfoList.map(_.prettyString).mkString("\n"))
            case Some("state") =>
              console.println(engine.symbolTable.keys.toSeq.sorted.map(showValue).mkString("\n"))
            case _ =>
              console.println(engine.getPrettyString)
          }
        }
      },
      new Command("display") {
        def usage: (String, String) = ("display symbol[, symbol, ...]", "show computation of symbols")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "display" }),
                new StringsCompleter(jlist(engine.symbolTable.keys.toSeq))
              )
            )
          }
        }

        def run(args: Array[String]): Unit = {
          getOneArg("", Some("state")) match {
            case Some(symbolList) =>
              if (currentTreadleTesterOpt.isDefined) {
                console.println(engine.renderComputation(symbolList, outputFormat))
              }
            case _ =>
              console.println(engine.getPrettyString)
          }
        }
      },
      new Command("info") {
        def usage: (String, String) = ("info", "show information about the circuit")
        def run(args: Array[String]): Unit = {
          console.println(engine.getInfoString)
        }
      },
      new Command("walltime") {
        def usage: (String, String) = ("walltime [advance]", "show current wall time, or advance it")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "walltime" })
              )
            )
          }
        }
        // scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          currentTreadleTesterOpt match {
            case Some(tester) =>
              getOneArg("") match {
                case Some(numberString) =>
                  val advance = numberString.toLong
                  if (advance < 1) {
                    console.println("walltime advance must be > 0")
                  } else {
                    currentTreadleTester.advanceTime(advance)
                    console.println(
                      s"Current Wall Time is ${currentTreadleTester.wallTime.currentTime}" +
                        s" incremented by $advance"
                    )
                  }

                case _ =>
                  console.println(s"Current Wall Time is ${currentTreadleTester.wallTime.currentTime}")
              }
            case _ =>
              console.println(s"You must have a firrtl file loaded to use walltime")
          }
        }
      },
      new Command("verbose") {
        def usage: (String, String) =
          ("verbose [true|false|toggle]", "set evaluator verbose mode (default toggle) during dependency evaluation")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "verbose" }),
                new StringsCompleter(jlist(Seq("true", "false", "toggle")))
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("verbose must be followed by true false or toggle", Some("toggle")) match {
            case Some("toggle") => engine.setVerbose(!engine.verbose)
            case Some("true")   => engine.setVerbose()
            case Some("false")  => engine.setVerbose(false)
            case _              =>
          }
          console.println(s"evaluator verbosity is now ${engine.verbose}")
        }
      },
      new Command("snapshot") {
        def usage: (String, String) = ("snapshot", "save state of engine")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "snapshot" }),
                new FileNameCompleter
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("snapshot requires a file name") match {
            case Some(fileName) =>
              val writer = new PrintWriter(new File(fileName))
              writer.write(engine.dataStore.serialize)
              writer.close()
            case _ =>
          }
          console.println(engine.dataStore.serialize)
        }
      },
      new Command("restore") {
        def usage: (String, String) = ("restore", "restore state of engine from snapshot file")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({ "restore" }),
                new FileNameCompleter
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("snapshot requires a file name") match {
            case Some(fileName) =>
              val ioSource = io.Source.fromFile(fileName)
              val jsonSource = ioSource.mkString
              ioSource.close
              engine.dataStore.deserialize(jsonSource)
            case _ =>
          }
        }
      },
//      new Command("allow-cycles") {
//        def usage: (String, String) = ("allow-cycles [true|false|toggle]",
//          "set evaluator allow combinational loops (could cause correctness problems")
//        override def completer: Option[ArgumentCompleter] = {
//          if(currentTreadleTesterOpt.isEmpty) {
//            None
//          }
//          else {
//            Some(new ArgumentCompleter(
//              new StringsCompleter({ "allow-cycles"}),
//              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
//            ))
//          }
//        }
//        def run(args: Array[String]): Unit = {
//          getOneArg("allow-cycles must be followed by true false or toggle", Some("toggle")) match {
//            case Some("toggle") =>
//              engine.evaluator.allowCombinationalLoops = ! engine.evaluator.allowCombinationalLoops
//            case Some("true")   => engine.evaluator.allowCombinationalLoops = true
//            case Some("false")  => engine.evaluator.allowCombinationalLoops = false
//            case _ =>
//          }
//          console.println(s"evaluator allow combinational loops is now ${engine.evaluator.evaluateAll}")
//        }
//      },
      new Command("waves") {
        def usage: (String, String) =
          ("waves symbolName ...", "generate wavedrom json for viewing waveforms")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "waves"
                }),
                new StringsCompleter(jlist(engine.validNames.toSeq))
              )
            )
          }
        }
        def run(args: Array[String]): Unit = {
          if (args.length < 3) {
            //            error("at least one symbol needed")
            None
          } else {
            val cycleTime = args(1).toInt
            val symbolNames = args.tail.tail
            val numSymbols = symbolNames.length
            val symbols: Array[Symbol] = new Array[Symbol](numSymbols)
            symbolNames.zipWithIndex.foreach { case (symbolName, counter) =>
              assert(
                engine.symbolTable.contains(symbolName),
                s""""$symbolName" : argument is not an element of this circuit"""
              )
              symbols.update(counter, engine.symbolTable(symbolName))
            }

            val waveformValues: WaveformValues =
              engine.dataStore.getWaveformValues(symbols, cycleTime, 4)

            console.println(waveformValues.toString)
            console.println(pretty(render(waveformValues.toJson)))
          }
        }
      },
      new Command("history") {
        def usage: (String, String) = ("history [pattern]", "show command history, with optional regex pattern")
        override def completer: Option[ArgumentCompleter] = {
          if (currentTreadleTesterOpt.isEmpty) {
            None
          } else {
            Some(
              new ArgumentCompleter(
                new StringsCompleter({
                  "history"
                })
              )
            )
          }
        }
        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          def showHistory(regex: String): Unit = {
            try {
              val historyRegex = regex.r
              var matches = 0
              val iterator = console.getHistory.entries()

              while (iterator.hasNext) {
                val command = iterator.next()
                if (historyRegex.findFirstIn(command.value()).isDefined) {
                  matches += 1
                  println(s"${command.index() + 1} ${command.value()}")
                }
              }
              if (matches == 0) {
                console.println(s"Sorry no wires matched regex $regex")
              }
            } catch {
              case e: Exception =>
                error(s"exception ${e.getMessage} $e")
              case a: AssertionError =>
                error(s"exception ${a.getMessage}")
            }
          }

          getOneArg("history pattern", Some(".")) match {
            case Some(regex) => showHistory(regex)
            case _           => showHistory(".")
          }
        }
      },
      new Command("help") {
        def usage: (String, String) = ("help [markdown]", "show repl commands (in markdown format if desired)")
        def run(args: Array[String]): Unit = {
          getOneArg("") match {
            case Some("markdown") =>
              console.println("| command | description |")
              console.println("| ------- | ----------- |")
              val helpText = Commands.commands.map { command =>
                val (column1, column2) = command.usage
                val escapeColumn1 = column1
                  .replaceAll(raw"\|", "&#124;")
                  .replaceAll("<", raw"\<")
                  .replaceAll(">", raw"\<")

                val escapeColumn2 = column2
                  .replaceAll(raw"\|", "&#124;")
                  .replaceAll("<", raw"\<")
                  .replaceAll(">", raw"\<")

                s"| $escapeColumn1 | $escapeColumn2 |"
              }.sorted.mkString("\n")
              console.println(helpText)
            case _ =>
              val maxColumn1Width = Commands.commands.map(_.usage._1.length).max + 2
              val helpText = Commands.commands.map { command =>
                val (column1, column2) = command.usage
                terminal.getWidth

                s"$column1${" " * (maxColumn1Width - column1.length)} $column2"
              }.sorted.mkString("\n")

              console.println(helpText)
          }
        }
      },
      new Command("quit") {
        def usage: (String, String) = ("quit", "exit the engine")
        def run(args: Array[String]): Unit = {
          if (!history.isEmpty) {
            history.removeLast()
          }
          done = true
        }
      }
    )
    val commandMap: Map[String, Command] = commands.map(command => command.name -> command).toMap
  }
  //scalastyle:on

  def buildCompletions(): Unit = {
    console.setCompletionHandler(new CandidateListCompletionHandler {})
    Commands.commands.flatMap { command =>
      command.completer
    }.foreach { completer =>
      console.addCompleter(completer)
    }
  }

  /** gets the next line from either the current executing script or from the console.
    * Strips comments from the line, may result in empty string, command parser is ok with that
    *
    * @return
    */
  def getNextLine: String = {
    val rawLine = currentScript match {
      case Some(script) =>
        script.getNextLineOption match {
          case Some(line) =>
            console.println(s"[${script.currentLine}:${script.fileName}] $line")
            line
          case _ =>
            console.readLine()
        }
      case _ =>
        console.setPrompt(s"${console.getHistory.index() + 1} treadle>> ")

        console.readLine()
    }
    if (rawLine == null) {
      history.add("quit")
      "quit"
    } else {
      rawLine.split("#").head
    }
  }

  def scriptRunning: Boolean = {
    currentScript match {
      case Some(script) => script.hasNext
      case _            => false
    }
  }

  //scalastyle:off method.length
  def run(): Unit = {
    console.setPrompt("treadle>> ")

    try {
      loadSource()

      annotationSeq.collectFirst { case TreadleScriptFile(name) => name }.foreach { scriptName =>
        loadScript(scriptName)
      }
      if (annotationSeq.contains(TreadleReplUseVcd)) {
        annotationSeq.collectFirst { case TreadleVcdScriptFileOverride(vcdName) => vcdName } match {
          case Some(vcdName) =>
            loadVcdScript(vcdName)
          case _ =>
            currentTreadleTesterOpt.foreach { tester =>
              val vcdName = tester.engine.ast.main + ".vcd"
              loadVcdScript(vcdName)
            }
        }
      }
    } catch {
      case t: TreadleException =>
        console.println(s"Startup: Treadle Exception ${t.getMessage}")
      case _: CyclicException =>
      case e: Throwable =>
        throw e
    }
    buildCompletions()

    if (annotationSeq.contains(TreadleReplRunScriptAtStartup)) {
      currentScript match {
        case Some(script) =>
          script.reset()
          script.runRemaining()
        case None =>
          console.println(s"Error: fr-run-script-at-startup set, with no script file")
      }
    }

    while (!done) {
      try {
        val line = getNextLine

        line.split(""";""").foreach { subLine =>
          args = subLine.trim.split(" +")

          if (args.length > 0) {
            if (Commands.commandMap.contains(args.head)) {
              Commands.commandMap(args.head).run(args.tail)
            } else {
              if (subLine.nonEmpty) error(s"unknown command $subLine, try help")
            }
          } else {
            error(s"unknown command: $subLine")
          }
        }
      } catch {
        case ie: TreadleException =>
          console.println(s"Treadle Exception occurred: ${ie.getMessage}")
          ie.printStackTrace()
        case _: CyclicException =>
        case e: NullPointerException =>
          error(s"Null pointer exception, please file an issue\n ${e.getMessage}")
          e.printStackTrace()
        case e: Exception =>
          console.println(s"Exception occurred: ${e.getMessage}")
          e.printStackTrace()
      }
    }

    try {
      console.println(s"saving history ${history.size()}")
      console.flush()
      history.flush()
      console.shutdown()
      TerminalFactory.get.restore()
    } catch {
      case e: Exception =>
        println(s"Error on quit, message is ${e.getMessage}")
    }
    terminal.restore()
  }

  def error(message: String): Unit = {
    console.println(s"Error: $message")
  }

  def jlist(list: Seq[String]): java.util.List[String] = {
    val array = ArrayBuffer.empty[String]
    array ++= list
    array.asJava
  }
}

object TreadleRepl {
  def apply(annotationSeq: AnnotationSeq): TreadleRepl = {
    val newAnnos = (new TreadleTesterPhase).transform(annotationSeq)
    new TreadleRepl(newAnnos)
  }

  def main(args: Array[String]): Unit = {
    try {
      (new TreadleReplStage).execute(args, Seq.empty)
    } catch {
      case a: OptionsException =>
        StageUtils.dramaticUsageError(a.message)
        System.exit(1)
    }
  }
}
