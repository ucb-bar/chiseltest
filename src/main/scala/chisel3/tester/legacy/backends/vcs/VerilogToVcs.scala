package chisel3.tester.legacy.backends.vcs

import chisel3.tester.legacy.backends.verilator.EditableBuildCSimulatorCommand
import scala.sys.process._

object VerilogToVcs extends EditableBuildCSimulatorCommand {
  val prefix = "vcs-command-edit"
  override def composeCommand(topModule: String,
                              dir: java.io.File,
                              flags: Seq[String],
                              cFlags: Seq[String]): String = {
    Seq("cd", dir.toString, "&&", "vcs") ++ flags mkString " "

  }

  def composeFlags(
    topModule: String,
    dir: java.io.File,
    moreVcsFlags: Seq[String] = Seq.empty[String],
    moreVcsCFlags: Seq[String] = Seq.empty[String]
  ): (Seq[String], Seq[String]) = {

    val ccFlags = Seq("-I$VCS_HOME/include", "-I$dir", "-fPIC", "-std=c++11") ++ moreVcsCFlags

    val vcsFlags = Seq(
      "-full64",
      "-quiet",
      "-timescale=1ns/1ps",
      "-debug_pp",
      s"-Mdir=$topModule.csrc",
      "+v2k",
      "+vpi",
      "+vcs+lic+wait",
      "+vcs+initreg+random",
      "+define+CLOCK_PERIOD=1",
      "-P",
      "vpi.tab",
      "-cpp",
      "g++",
      "-O2",
      "-LDFLAGS",
      "-lstdc++",
      "-CFLAGS",
      "\"%s\"".format(ccFlags mkString " ")
    ) ++
      moreVcsFlags

    (vcsFlags, ccFlags)
  }

  def constructCSimulatorCommand(
    topModule: String,
    dir: java.io.File,
    harness: java.io.File,
    iFlags: Seq[String] = Seq.empty[String],
    iCFlags: Seq[String] = Seq.empty[String]
  ): String = {

    val (cFlags, cCFlags) = composeFlags(
      topModule,
      dir,
      iFlags ++ blackBoxVerilogList(dir) ++ Seq(
        "-o",
        topModule,
        s"$topModule.v",
        harness.toString,
        "vpi.cpp"
      ),
      iCFlags
    )

    composeCommand(topModule, dir, cFlags, cCFlags)
  }

  def apply(topModule: String,
            dir: java.io.File,
            vcsHarness: java.io.File,
            moreVcsFlags: Seq[String] = Seq.empty[String],
            moreVcsCFlags: Seq[String] = Seq.empty[String],
            editCommands: String = ""): ProcessBuilder = {

    val finalCommand = editCSimulatorCommand(
      constructCSimulatorCommand(
        topModule,
        dir,
        vcsHarness,
        moreVcsFlags,
        moreVcsCFlags
      ),
      editCommands
    )
    println(s"$finalCommand")

    Seq("bash", "-c", finalCommand)
  }
}
