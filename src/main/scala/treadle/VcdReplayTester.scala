// SPDX-License-Identifier: Apache-2.0

//
package treadle

import java.io.File

import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, Shell, ShellOption, Stage, StageMain, Unserializable}
import firrtl.stage.{FirrtlCli, FirrtlSourceAnnotation}
import logger.LazyLogging
import treadle.executable.TreadleException
import treadle.utils.VcdRunner
import treadle.vcd.VCD

/** This tester runs a VCD file against a circuit expressed in a firrtl file.  The VCD file should
  * have been produced by running a test harness against the circuit.  This test can be used to
  * generate circuit behavior while running symbolic or concolic testing.
  * It can also be used to determine if later changes to a circuit have changed since some original
  * correct **golden** run of the circuit
  * For example use the main below to run the VcdAdder files contained in the src/test/resources directory
  * {{{
  * sbt 'runMain treadle.VcdReplayTester -fs src/test/resources/VcdAdder.fir -vcd src/test/resources/VcdAdder.vcd'
  * }}}
  *
  * @param annotationSeq Used to set various options
  */
class VcdReplayTester(annotationSeq: AnnotationSeq) extends LazyLogging {

  private def getInput(fileName: String): String = {
    var file = new File(fileName)
    if (!file.exists()) {
      file = new File(fileName + ".fir")
      if (!file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    // this is a bit of boiler plate to not leave source hanging open
    val source = io.Source.fromFile(file)
    val text = source.mkString
    source.close()
    text
  }

  val vcdFileName: String = annotationSeq.collectFirst { case VcdReplayVcdFile(fileName) => fileName }.getOrElse(
    throw TreadleException(s"You must specify VcdFile to compare to treadle execution")
  )

  val tester: TreadleTester = TreadleTester(annotationSeq)

  val vcd: VCD = VCD.read(vcdFileName, tester.engine.ast.main)

  val vcdRunner: VcdRunner = new VcdRunner(tester, vcd)

  def testSuccesses: Long = vcdRunner.testSuccesses
  def testFailures:  Long = vcdRunner.testFailures

  def run(): Unit = {
    vcdRunner.setInitialValues()

    val start = annotationSeq.collectFirst { case VcdReplaySkipEvents(n) => n }.getOrElse(0)
    val end = annotationSeq.collectFirst { case VcdReplaySkipEvents(n) => start + n }.getOrElse(vcdRunner.events.length)

    vcdRunner.setNextEvent(start)

    val startTime = System.currentTimeMillis()
    while (vcdRunner.nextEvent < end) {
      println(vcdRunner.eventSummary(vcdRunner.nextEvent))

      vcdRunner.executeNextEvent()

      vcdRunner.testWires(
        vcdRunner.previousEvent,
        justOutputs = false,
        clearResult = false
      )
    }
    val endTime = System.currentTimeMillis()

    tester.finish

    println(f"events run:       ${vcdRunner.eventsRun}%10d")
    println(f"input values set: ${vcdRunner.inputValuesSet}%10d")
    println(f"values tested:    ${vcdRunner.valuesTested}%10d")
    println(f"test successes:   ${vcdRunner.testSuccesses}%10d")
    println(f"test failures:    ${vcdRunner.testFailures}%10d")
    println(f"clock cycles:     ${tester.cycleCount}%10d")
    println(f"                  ${tester.cycleCount / ((endTime - startTime) / 1000.0)}%10.2f Hz")
    println(f"run time:         ${(endTime - startTime) / 1000.0}%10.2f seconds")
  }
}

trait VcdReplayTesterCli { this: Shell =>
  parser.note("VcdReplayTester Front End Options")

}

class VcdReplayTesterStage extends Stage {
  val shell = new Shell("vcd_replay_tester") with VcdReplayTesterCli with FirrtlCli

  def apply(annotationSeq: AnnotationSeq): VcdReplayTester = {
    new VcdReplayTester(annotationSeq)
  }

  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    new VcdReplayTester(annotations)
    annotations
  }
}

object VcdReplayTester extends StageMain(new VcdReplayTesterStage)

sealed trait VcdReplayTesterOptions extends Unserializable { this: Annotation => }

case class VcdReplayVcdFile(fileName: String) extends NoTargetAnnotation with VcdReplayTesterOptions

case class VcdReplaySkipEvents(count: Int) extends NoTargetAnnotation with VcdReplayTesterOptions

case class VcdReplayEventsToRun(count: Int) extends NoTargetAnnotation with VcdReplayTesterOptions

/** makes a more convenient way of specifying firrtl source
  */
case object VcdReplayFirrtlSource extends NoTargetAnnotation with VcdReplayTesterOptions with HasShellOptions {
  val options = Seq(
    new ShellOption[String](
      longOption = "firrtl-source",
      shortOption = Some("fs"),
      toAnnotationSeq = (s: String) => Seq(FirrtlSourceAnnotation(s)),
      helpText = "firrtl source file to load on startup"
    )
  )
}

case object VcdReplayVcdFile extends HasShellOptions {
  val options = Seq(
    new ShellOption[String](
      longOption = "vcd-file",
      shortOption = Some("vcd"),
      toAnnotationSeq = (s: String) => Seq(VcdReplayVcdFile(s)),
      helpText = "firrtl source file to load on startup"
    )
  )
}

case object VcdReplaySkipEvents extends HasShellOptions {
  val options = Seq(
    new ShellOption[Int](
      longOption = "skip-events",
      shortOption = Some("se"),
      toAnnotationSeq = (n: Int) => Seq(VcdReplaySkipEvents(n)),
      helpText = "number of events to skip before starting"
    )
  )
}

case object VcdReplayEventsToRun extends HasShellOptions {
  val options = Seq(
    new ShellOption[Int](
      longOption = "events-to-run",
      shortOption = Some("etr"),
      toAnnotationSeq = (n: Int) => Seq(VcdReplayEventsToRun(n)),
      helpText = "number of events to run"
    )
  )
}

case object VcdReplayTestAliasedWires extends NoTargetAnnotation with VcdReplayTesterOptions with HasShellOptions {
  val options = Seq(
    new ShellOption[Int](
      longOption = "test-aliased-wires",
      shortOption = Some("taw"),
      toAnnotationSeq = _ => Seq(VcdReplayTestAliasedWires),
      helpText = "test values of internal intermediate wires against vcd"
    )
  )
}
