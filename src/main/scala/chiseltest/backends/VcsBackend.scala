// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.ir._
import firrtl.options.Viewer

import java.io.{File, FileWriter}

/** [[SimulatorBackend]] for Synopsys VCS backend.
  * Interface should be [[VPIInterface]].
  */
object VcsBackend extends SimulatorBackend {

  import scala.sys.process._

  /** VCS use Verilog as harness, this function generate this harness based on [[Circuit]] */
  private def generateVcsVerilogHarness(
    targetDir: String,
    waveForm:  Option[String],
    circuit:   Circuit,
    topPorts:  Seq[Port]
  ): String = {
    def generateHarnessIO(port: Port) = port match {
      case Port(_, name, direction, tpe) =>
        s"  ${direction match {
          case Input  => "reg"
          case Output => "wire"
        }}[${tpe.asInstanceOf[GroundType].width.asInstanceOf[IntWidth].width - 1}: 0] $name = 0;"
    }

    def generateDutIO(port: Port) = port match {
      case Port(_, name, _, _) =>
        s"    .$name($name)"
    }

    def waveDump(waveForm: Option[String]) = waveForm match {
      case Some("vcd") => s"""$$vcdplusfile("${targetDir + File.separator + s"${circuit.main}.vcd"}");"""
      case _           => ""
    }

    val (input, output) = topPorts.partition(_.direction == Input)
    val dutName = circuit.main
    val emittedStuff =
      s"""module test;
         |${topPorts.map(generateHarnessIO).mkString("\n")}
         |  always #1 clock = ~clock;
         |  reg vcdon = 0;
         |  reg [1023:0] vcdfile = 0;
         |
         |  $dutName $dutName(
         |${topPorts.map(generateDutIO).mkString(", \n")}
         |  );
         |
         |  initial begin
         |    $$init_rsts(reset);
         |    $$init_ins(${input.map(_.name).mkString(", ")});
         |    $$init_outs(${output.map(_.name).mkString(", ")});
         |    $$init_sigs($dutName);
         |    ${waveDump(waveForm)}
         |    $$vcdplusautoflushon;
         |  end
         |
         |  always @(negedge clock) begin
         |    $$tick();
         |    $$vcdplusflush;
         |  end
         |endmodule
         |""".stripMargin
    val verilogHarnessFileName = s"${circuit.main}-harness.v"
    val verilogHarnessFile:   File = new File(targetDir, verilogHarnessFileName)
    val verilogHarnessWriter: FileWriter = new FileWriter(verilogHarnessFile)
    verilogHarnessWriter.append(emittedStuff)
    verilogHarnessWriter.close()
    verilogHarnessFile.getAbsoluteFile.toString
  }

  /** Compile Verilog to executable linked to VCS own library. */
  private def compileVcsDut(
    targetDir:           String,
    circuit:             Circuit,
    userSimulatorFlags:  Option[Seq[String]],
    userSimulatorCFlags: Option[Seq[String]],
    coverageAnnotations: Set[CoverageAnnotations],
    verilogHarnessFile:  String
  ): Unit = {
    val topName = circuit.main
    val blackBoxVerilogListFile: File = new File(targetDir, firrtl.transforms.BlackBoxSourceHelper.defaultFileListName)
    val blackBoxVerilogListFlag = if (blackBoxVerilogListFile.exists()) {
      Seq("-f", blackBoxVerilogListFile.getAbsolutePath)
    } else {
      Seq.empty[String]
    }
    val coverageFlags: Seq[String] = coverageAnnotations.collect {
      case LineCoverageAnnotation        => Set("line")
      case ToggleCoverageAnnotation      => Set("tgl")
      case BranchCoverageAnnotation      => Set("branch")
      case ConditionalCoverageAnnotation => Set("cond")
      case UserCoverageAnnotation        => Set("assert")
      case StructuralCoverageAnnotation  => Set("line", "tgl", "branch", "cond")
    }.flatten.toSeq match {
      case Nil   => Seq.empty
      case flags => Seq("-cm " + flags.mkString("+"))
    }
    val vcsCFlags = Seq(
      "-I$VCS_HOME/include",
      s"-I${getClass.getResource("/vpi/").getPath}",
      "-fPIC",
      "-std=c++11"
    ) ++ userSimulatorCFlags.getOrElse(Seq.empty)
    val vcsFlags = Seq(
      "-full64",
      "-quiet",
      "-timescale=1ns/1ps",
      "-debug_pp",
      s"-Mdir=$topName.csrc",
      "+v2k",
      "+vpi",
      "+vcs+lic+wait",
      "+vcs+initreg+random",
      "-P",
      getClass.getResource("/vpi/vpi.tab").getPath,
      "-cpp",
      "g++",
      "-O2",
      "-LDFLAGS",
      "-lstdc++"
    ) ++ blackBoxVerilogListFlag ++ coverageFlags ++ userSimulatorFlags.getOrElse(Seq.empty) ++ Seq(
      "-o",
      topName,
      s"$topName.v",
      verilogHarnessFile,
      getClass.getResource("/vpi/vpi.cpp").getPath
    )

    val generatedCommand = s"""cd $targetDir && vcs ${vcsFlags.mkString(" ")} -CFLAGS "${vcsCFlags.mkString(" ")}" """
    assert(
      Seq("bash", "-c", generatedCommand).! == 0,
      s"vcs failed on circuit $topName in work dir $targetDir: \n$generatedCommand"
    )
  }

  def compileDut(annotations: AnnotationSeq): AnnotationSeq = {
    val options = Viewer[ChiselTestOptions].view(annotations)
    compileVcsDut(
      options.targetDir.get,
      options.circuit.get,
      options.simulatorFlags,
      options.simulatorCFlags,
      options.coverageAnnotations,
      generateVcsVerilogHarness(
        options.targetDir.get,
        options.waveForm,
        options.circuit.get,
        options.topPorts.get
      )
    )
    annotations :+ SimulatorInterfaceAnnotation(
      new VPIInterface(
        options.topPorts.get,
        options.topName.get,
        Seq(
          new File(options.targetDir.get, options.topName.get).toString,
          "-k",
          options.targetDir.get + File.separator + "ucli.key"
        )
      )
    )
  }
}
