// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental

import chisel3.stage.ChiselCli
import chiseltest.simulator._
import firrtl.options.{HasShellOptions, Shell, ShellOption}
import firrtl.stage.FirrtlCli

object ChiselTestFlags extends HasShellOptions {
  override val options: Seq[ShellOption[_]] = Seq(
    // backends
    new ShellOption[Unit](
      longOption = "t-use-treadle",
      toAnnotationSeq = _ => Seq(TreadleBackendAnnotation),
      helpText = "direct tester to use the treadle simulator"
    ),
    new ShellOption[Unit](
      longOption = "t-use-verilator",
      toAnnotationSeq = _ => Seq(VerilatorBackendAnnotation),
      helpText = "direct tester to use the verilator simulator"
    ),
    new ShellOption[Unit](
      longOption = "t-use-iverilog",
      toAnnotationSeq = _ => Seq(IcarusBackendAnnotation),
      helpText = "direct tester to use the Icarus Verilog simulator"
    ),
    new ShellOption[Unit](
      longOption = "t-use-vcs",
      toAnnotationSeq = _ => Seq(VcsBackendAnnotation),
      helpText = "direct tester to use the VCS simulator"
    ),
    // waveform dumps
    new ShellOption[Unit](
      longOption = "t-write-vcd",
      toAnnotationSeq = _ => Seq(WriteVcdAnnotation),
      helpText = "writes vcd waveform dump"
    ),
    new ShellOption[Unit](
      longOption = "t-write-vpd",
      toAnnotationSeq = _ => Seq(WriteVpdAnnotation),
      helpText = "writes vpd waveform dump"
    ),
    new ShellOption[Unit](
      longOption = "t-write-lxt",
      toAnnotationSeq = _ => Seq(WriteLxtAnnotation(version = 1)),
      helpText = "writes lxt waveform dump"
    ),
    new ShellOption[Unit](
      longOption = "t-write-lxt2",
      toAnnotationSeq = _ => Seq(WriteLxtAnnotation(version = 2)),
      helpText = "writes lxt2 waveform dump"
    ),
    new ShellOption[Unit](
      longOption = "t-write-fst",
      toAnnotationSeq = _ => Seq(WriteFstAnnotation),
      helpText = "writes fst waveform dump"
    )
  )
}

trait ChiselTestCli extends ChiselCli with FirrtlCli {
  this: Shell =>
  parser.note("ChiselTest Options")
  ChiselTestFlags.addOptions(parser)
}

class ChiselTestShell extends Shell("ChiselTest") with ChiselTestCli
