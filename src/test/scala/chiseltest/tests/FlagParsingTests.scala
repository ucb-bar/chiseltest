// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chiseltest._
import chiseltest.experimental.ChiselTestShell
import org.scalatest.flatspec.AnyFlatSpec

class FlagParsingTests extends AnyFlatSpec {
  behavior of "ChiselTestShell"

  val shell = new ChiselTestShell

  it should "parse all backend options" in {
    assert(shell.parse(Array("--t-use-verilator")).toSeq == Seq(VerilatorBackendAnnotation))
    assert(shell.parse(Array("--t-use-treadle")).toSeq == Seq(TreadleBackendAnnotation))
    assert(shell.parse(Array("--t-use-vcs")).toSeq == Seq(VcsBackendAnnotation))
    assert(shell.parse(Array("--t-use-iverilog")).toSeq == Seq(IcarusBackendAnnotation))
  }

  it should "parse all wavedump options" in {
    assert(shell.parse(Array("--t-write-vcd")).toSeq == Seq(WriteVcdAnnotation))
    assert(shell.parse(Array("--t-write-vpd")).toSeq == Seq(WriteVpdAnnotation))
    assert(shell.parse(Array("--t-write-fst")).toSeq == Seq(WriteFstAnnotation))
    assert(shell.parse(Array("--t-write-lxt")).toSeq == Seq(WriteLxtAnnotation(1)))
    assert(shell.parse(Array("--t-write-lxt2")).toSeq == Seq(WriteLxtAnnotation(2)))
  }
}
