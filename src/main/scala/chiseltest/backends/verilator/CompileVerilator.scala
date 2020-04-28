package chiseltest.backends.verilator

import java.io.{File, FileOutputStream, PrintWriter}
import java.security.MessageDigest

import chiseltest.internal.WriteVcdAnnotation
import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir.Circuit
import firrtl.options.{Phase, PreservesAll, TargetDirAnnotation}
import firrtl.stage.FirrtlCircuitAnnotation

import scala.sys.process._

case class EnableCache() extends NoTargetAnnotation

case class VerilatorFlags(flags: Seq[String]) extends NoTargetAnnotation

case class VerilatorCFlags(flags: Seq[String]) extends NoTargetAnnotation

case class CommandAnnotation(value: Seq[String]) extends NoTargetAnnotation

class CompileVerilator extends Phase with PreservesAll[Phase] {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    // if user define his own Command to run
    val customCommand = a.exists { case _: CommandAnnotation => true; case _ => false }
    val enableCache = a.exists { case _: EnableCache => true; case _ => false }
    if (customCommand) return a
    val targetDir: String = a.collectFirst { case TargetDirAnnotation(t) => t }.get
    val circuit: Circuit = a.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    val topName = circuit.main
    // if user enable cache, we will compare the hash of circuit
    // but notice only circuit will be calculated.
    // thus if user change there flags, compiling won't be triggered
    val hash = MessageDigest.getInstance("MD5").digest(circuit.serialize.getBytes).map(0xFF & _).map("%02x".format(_)).mkString
    val hashFile = new File(targetDir, s"${circuit.main}-hash.md5")
    val oldHash = if (hashFile.canRead) {
      val f = scala.io.Source.fromFile(hashFile)
      val r = f.mkString
      r
    } else ""
    // write new to file.
    val hashWriter = new PrintWriter(new FileOutputStream(hashFile, false))
    hashWriter.append(hash)
    hashWriter.close()
    if (oldHash == hash & enableCache) {
      println("use cache circuit")
    } else {
      println("run compile.")
      val verilatorBinary = a.collectFirst { case SimulatorBinaryPath(p) => p }.get
      val writeVcdFlag: Seq[String] = if (a.contains(WriteVcdAnnotation)) Seq("--trace") else Seq.empty
      val userVerilatorFlags: Seq[String] = a.collectFirst { case VerilatorFlags(f) => f }.getOrElse(Seq.empty)
      val moreVerilatorCFlags: Seq[String] = a.collectFirst { case VerilatorCFlags(f) => f }.getOrElse(Seq.empty)
      val cppHarnessFile: String = a.collectFirst { case CppHarnessFile(f) => f }.get
      val simulatorHFileDirectory: String = a.collectFirst { case SimulatorHFileDictionary(f) => f }.get

      val blackBoxVerilogListFile = new File(targetDir, firrtl.transforms.BlackBoxSourceHelper.defaultFileListName)
      val blackBoxVerilogListFlag = if (blackBoxVerilogListFile.exists()) {
        Seq("-f", blackBoxVerilogListFile.getAbsolutePath)
      } else {
        Seq.empty[String]
      }
      val verilatorCFlags = Seq(
        "-Wno-undefined-bool-conversion", "-O1", s"-DTOP_TYPE=V$topName", "-DVL_USER_FINISH",
        s"-include V$topName.h", s"-I${simulatorHFileDirectory}"
      ) ++ moreVerilatorCFlags
      val verilatorFlags = Seq(
        "--assert", "-Wno-fatal", "-Wno-WIDTH", "-Wno-STMTDLY", "-O1",
        "--top-module", topName,
        s"+define+TOP_TYPE=V$topName", s"+define+PRINTF_COND=!$topName.reset", s"+define+STOP_COND=!$topName.reset",
        "-CFLAGS", s""""${verilatorCFlags mkString " "}"""",
        "-Mdir", targetDir,
        "--exe", cppHarnessFile
      ) ++ blackBoxVerilogListFlag ++ writeVcdFlag ++ userVerilatorFlags
      val generatedCommand = s"cd $targetDir && $verilatorBinary --cc $topName.v ${verilatorFlags.mkString(" ")}"
      assert(
        Seq("bash", "-c", generatedCommand).! == 0,
        s"verilator command failed on circuit $topName in work dir $targetDir: \n$generatedCommand"
      )
      assert(
        Seq("make", "-C", targetDir, "-j", "-f", s"V$topName.mk", s"V$topName").! == 0,
        s"Compilation of verilator generated code failed for circuit $topName in work dir $targetDir"
      )
    }

    val command = Seq(new File(targetDir, s"V$topName").toString)
    a ++ Seq(CommandAnnotation(command))
  }
}