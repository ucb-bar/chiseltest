package chiseltest.stage

import chisel3.MultiIOModule
import firrtl.annotations.{Annotation, NoTargetAnnotation}
import firrtl.options.{HasShellOptions, ShellOption, Unserializable}

sealed trait ChiselTestOption extends Unserializable {
  this: Annotation =>
}

case class TestFunctionAnnotation[T <: MultiIOModule](func: T => Unit) extends NoTargetAnnotation with ChiselTestOption

private[chiseltest] object TestFunctionAnnotation extends HasShellOptions {
  override val options = Seq(
    new ShellOption[String](
      longOption = "test-function",

      /** @todo find a way to point to a test function. */
      toAnnotationSeq = a => Seq(),
      helpText = "<test function>",
      shortOption = None
    )
  )
}

@deprecated
case class WriteVcdAnnotation() extends NoTargetAnnotation with ChiselTestOption

case class WaveFormAnnotation(waveForm: String) extends NoTargetAnnotation with ChiselTestOption

private[chiseltest] object WaveFormAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "write-vcd",
      toAnnotationSeq = {
        case "vcd" => Seq(WaveFormAnnotation("vcd"))
        case "fsdb" => Seq(WaveFormAnnotation("fsdb"))
      },
      helpText = "writes waveform, could be vcd or fsdb(vcs only)."
    )
  )
}

@deprecated
case class TreadleBackendAnnotation() extends NoTargetAnnotation with ChiselTestOption

@deprecated
case class VerilatorBackendAnnotation() extends NoTargetAnnotation with ChiselTestOption

@deprecated
case class VcsBackendAnnotation() extends NoTargetAnnotation with ChiselTestOption

case class SimulatorBackendAnnotation(simulator: String) extends NoTargetAnnotation with ChiselTestOption

private[chiseltest] object SimulatorBackendAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "simulator-backend",
      toAnnotationSeq = {
        case a@"treadle" => Seq(SimulatorBackendAnnotation(a))
        case a@"verilator" => Seq(SimulatorBackendAnnotation(a))
        case a@"vcs" => Seq(SimulatorBackendAnnotation(a))
        case _ => Seq.empty
      },
      helpText = "choose simulator backend, `verilator`, `treadle`, `vcs`"
    )
  )
}


case class SimulatorBinary(file: String) extends NoTargetAnnotation

private[chiseltest] object SimulatorBinary extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "simulator-binary",
      toAnnotationSeq = a => Seq(SimulatorBinary(a)),
      helpText = "choose simulator binary, default to read from $PATH."
    )
  )
}


case class LicenseAnnotation(license: String) extends NoTargetAnnotation

private[chiseltest] object LicenseAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[String](
      longOption = "license-annotation",
      toAnnotationSeq = a => Seq(LicenseAnnotation(a)),
      helpText = "set license for vcs or verdi. String after LM_LICENSE_FILE."
    )
  )
}

case class EnableCache(enable: Boolean) extends NoTargetAnnotation

private[chiseltest] object EnableCache extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Boolean](
      longOption = "enable-cache",
      toAnnotationSeq = b => Seq(EnableCache(b)),
      helpText = "enable verilog cache between multiple running. Default to be true."
    )
  )
}

case class SimulatorFlagsAnnotation(flags: Seq[String]) extends NoTargetAnnotation

private[chiseltest] object SimulatorFlagsAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "verilator-flags",
      toAnnotationSeq = a => Seq(SimulatorFlagsAnnotation(a)),
      helpText = "add your own custom Verilator flags."
    )
  )
}

case class SimulatorCFlagsAnnotation(flags: Seq[String]) extends NoTargetAnnotation

private[chiseltest] object SimulatorCFlagsAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "verilator-cflags",
      toAnnotationSeq = a => Seq(SimulatorCFlagsAnnotation(a)),
      helpText = "add your own custom Verilator Cflags."
    )
  )
}

/** optional */
case class CommandAnnotation(value: Seq[String]) extends NoTargetAnnotation

private[chiseltest] object CommandAnnotation extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "custom-command",
      toAnnotationSeq = a => Seq(CommandAnnotation(a)),
      helpText = "override the simulator running command."
    )
  )
}

case class CppHarnessFileAnnotaion(file: String) extends NoTargetAnnotation

private[chiseltest] object CppHarnessFileAnnotaion extends HasShellOptions {
  val options: Seq[ShellOption[_]] = Seq(
    new ShellOption[Seq[String]](
      longOption = "custom-cpp-harness",
      toAnnotationSeq = a => Seq(CommandAnnotation(a)),
      helpText = "custom cpp harness for verilator."
    )
  )
}