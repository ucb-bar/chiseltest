// See LICENSE for license details.

package chisel3.tests.internal

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.VerilogCompiler
import firrtl.options.TargetDirAnnotation
import firrtl.stage.CompilerAnnotation
import org.scalatest.{FreeSpec, Matchers}

class VerilogCompilerSpec extends FreeSpec with Matchers {
  class PassThru extends Module {
    val io = IO(new Bundle { val in = Input(Bool()) ; val out = Output(Bool()) })
    io.out := io.in
  }
  "how to get from chisel dut to verilog" in {
     val annos = Seq(
       TargetDirAnnotation("test_run_dir/VerilogCompilerSpec"),
       ChiselGeneratorAnnotation(() => new PassThru),
       CompilerAnnotation(new VerilogCompiler())
     )

    val resultAnnos = (new ChiselStage).run(annos)
    resultAnnos.nonEmpty should be (true)
  }

}
