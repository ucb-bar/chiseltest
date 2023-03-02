// SPDX-License-Identifier: Apache-2.0

package treadle2.blackboxes

import firrtl.options.TargetDirAnnotation
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.asyncreset.AsyncResetBlackBoxFactory
import treadle2.{BlackBoxFactoriesAnnotation, TreadleTestHarness, WriteVcdAnnotation}

// scalastyle:off magic.number
class AsyncResetRegSpec extends AnyFreeSpec with Matchers {
  "an async reset reg should behave like a normal reg using clock" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesAsyncResetReg :
        |  extmodule AsyncResetReg :
        |    output io : {flip d : UInt<1>, q : UInt<1>, flip en : UInt<1>}
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 1
        |
        |  extmodule AsyncResetReg_1 :
        |    output io : {flip d : UInt<1>, q : UInt<1>, flip en : UInt<1>}
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 1
        |
        |  extmodule AsyncResetReg_2 :
        |    output io : {flip d : UInt<1>, q : UInt<1>, flip en : UInt<1>}
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 0
        |
        |  module AsyncResetRegVec_w3_i3 :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip d : UInt<3>, q : UInt<3>, flip en : UInt<1>}
        |
        |    inst reg_0 of AsyncResetReg @[AsyncResetRegTest.scala 103:11]
        |    reg_0.io is invalid
        |    reg_0.clk is invalid
        |    reg_0.rst is invalid
        |    inst reg_1 of AsyncResetReg_1 @[AsyncResetRegTest.scala 103:11]
        |    reg_1.io is invalid
        |    reg_1.clk is invalid
        |    reg_1.rst is invalid
        |    inst reg_2 of AsyncResetReg_2 @[AsyncResetRegTest.scala 103:11]
        |    reg_2.io is invalid
        |    reg_2.clk is invalid
        |    reg_2.rst is invalid
        |    reg_0.clk <= clock @[AsyncResetRegTest.scala 107:13]
        |    reg_0.rst <= reset @[AsyncResetRegTest.scala 108:13]
        |    node _T = bits(io.d, 0, 0) @[AsyncResetRegTest.scala 109:23]
        |    reg_0.io.d <= _T @[AsyncResetRegTest.scala 109:16]
        |    reg_0.io.en <= io.en @[AsyncResetRegTest.scala 110:16]
        |    reg_1.clk <= clock @[AsyncResetRegTest.scala 107:13]
        |    reg_1.rst <= reset @[AsyncResetRegTest.scala 108:13]
        |    node _T_1 = bits(io.d, 1, 1) @[AsyncResetRegTest.scala 109:23]
        |    reg_1.io.d <= _T_1 @[AsyncResetRegTest.scala 109:16]
        |    reg_1.io.en <= io.en @[AsyncResetRegTest.scala 110:16]
        |    reg_2.clk <= clock @[AsyncResetRegTest.scala 107:13]
        |    reg_2.rst <= reset @[AsyncResetRegTest.scala 108:13]
        |    node _T_2 = bits(io.d, 2, 2) @[AsyncResetRegTest.scala 109:23]
        |    reg_2.io.d <= _T_2 @[AsyncResetRegTest.scala 109:16]
        |    reg_2.io.en <= io.en @[AsyncResetRegTest.scala 110:16]
        |    node _T_3 = cat(reg_2.io.q, reg_1.io.q) @[Cat.scala 30:58]
        |    node _T_4 = cat(_T_3, reg_0.io.q) @[Cat.scala 30:58]
        |    io.q <= _T_4 @[AsyncResetRegTest.scala 116:8]
        |
        |  module UsesAsyncResetReg :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<3>, out : UInt<3>}
        |
        |    inst reg of AsyncResetRegVec_w3_i3 @[AsyncResetRegTest.scala 128:19]
        |    reg.clock <= clock
        |    reg.reset <= reset
        |    reg.io.d <= io.in @[AsyncResetRegTest.scala 130:12]
        |    reg.clock <= clock @[AsyncResetRegTest.scala 131:13]
        |    reg.reset <= reset @[AsyncResetRegTest.scala 132:13]
        |    reg.io.en <= UInt<1>("h01") @[AsyncResetRegTest.scala 133:13]
        |    io.out <= reg.io.q @[AsyncResetRegTest.scala 135:10]
        |
        |
      """.stripMargin

    val options = Seq(
      BlackBoxFactoriesAnnotation(Seq(new AsyncResetBlackBoxFactory))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      // poke a value and it should not appear as reg output until after step
      tester.poke("io_in", 7)
      tester.expect("io_out", 0)

      tester.step()

      tester.expect("io_out", 7)

      // reset should make immediate change to register
      tester.poke("reset", 1)
      tester.expect("io_out", 3)

      tester.step()

      tester.expect("io_out", 3)
    }
  }

  "single bit example of an async reset reg should behave like a normal reg using clock" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesAsyncResetReg :
        |  extmodule AsyncResetReg :
        |    input io_d : UInt<1>
        |    output io_q : UInt<1>
        |    input io_en : UInt<1>
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 1
        |
        |  module UsesAsyncResetReg :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip in : UInt<1>, flip en : UInt<1>, out : UInt<1>}
        |
        |    inst reg_0 of AsyncResetReg @[AsyncResetRegTest.scala 45:11]
        |    reg_0.rst is invalid
        |    reg_0.clk is invalid
        |    reg_0.io_en is invalid
        |    reg_0.io_q is invalid
        |    reg_0.io_d is invalid
        |
        |    reg_0.clk <= clock
        |    reg_0.rst <= reset
        |    reg_0.io_d <= io.in  @[AsyncResetRegTest.scala 51:16]
        |    reg_0.io_en <= io.en @[AsyncResetRegTest.scala 52:16]
        |
        |    io.out <= reg_0.io_q
        |
        |
    """.stripMargin

    val options = Seq(
      WriteVcdAnnotation,
      TargetDirAnnotation("test_run_dir/async_reset_1"),
      OutputFileAnnotation("async_reset_1"),
      BlackBoxFactoriesAnnotation(Seq(new AsyncResetBlackBoxFactory))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      // test that setting the input is only seen after step
      tester.poke("io_in", 1)
      tester.poke("io_en", 1)
      tester.expect("io_out", 0)

      tester.step()

      tester.expect("io_out", 1)

      // test that enable defeats register update on step
      tester.poke("io_in", 0)
      tester.poke("io_en", 0)
      tester.expect("io_out", 1)

      tester.step()

      tester.expect("io_out", 1)

      // setting enable lets register update on step again
      tester.poke("io_en", 1)
      tester.expect("io_out", 1)

      tester.step()

      tester.expect("io_out", 0)

      // setting reset immediately updates register
      tester.poke("reset", 1)
      tester.expect("io_out", 1)

      tester.step()

      tester.expect("io_out", 1)
    }
  }

  "mutually connected async registers should work" in {
    val input =
      """
        |circuit AsyncResetFeedbackModule :
        |  extmodule AsyncResetReg :
        |    input io_d : UInt<1>
        |    output io_q : UInt<1>
        |    input io_en : UInt<1>
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 0
        |
        |  extmodule AsyncResetReg_1 :
        |    input io_d : UInt<1>
        |    output io_q : UInt<1>
        |    input io_en : UInt<1>
        |    input clk : Clock
        |    input rst : UInt<1>
        |
        |    defname = AsyncResetReg
        |    parameter RESET_VALUE = 1
        |
        |  module AsyncResetFeedbackModule :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io_out_0 : UInt<1>
        |    output io_out_1 : UInt<1>
        |
        |    inst reg0 of AsyncResetReg @[AsyncResetRegTest.scala 39:20]
        |    inst reg1 of AsyncResetReg_1 @[AsyncResetRegTest.scala 40:20]
        |    io_out_0 <= reg0.io_q @[AsyncResetRegTest.scala 45:13]
        |    io_out_1 <= reg1.io_q @[AsyncResetRegTest.scala 46:13]
        |    reg0.io_d <= reg1.io_q @[AsyncResetRegTest.scala 42:10]
        |    reg0.io_en <= UInt<1>("h1") @[AsyncResetRegTest.scala 52:11]
        |    reg0.clk <= clock @[AsyncResetRegTest.scala 48:12]
        |    reg0.rst <= reset @[AsyncResetRegTest.scala 50:12]
        |    reg1.io_d <= reg0.io_q @[AsyncResetRegTest.scala 43:10]
        |    reg1.io_en <= UInt<1>("h1") @[AsyncResetRegTest.scala 53:11]
        |    reg1.clk <= clock @[AsyncResetRegTest.scala 49:12]
        |    reg1.rst <= reset @[AsyncResetRegTest.scala 51:12]
    """.stripMargin

    val options = Seq(
      WriteVcdAnnotation,
      TargetDirAnnotation("test_run_dir/async_reset_2"),
      OutputFileAnnotation("async_reset_2"),
      BlackBoxFactoriesAnnotation(Seq(new AsyncResetBlackBoxFactory))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      tester.expect("io_out_0", 0)
      tester.expect("io_out_1", 0)
      tester.step()
      tester.expect("io_out_0", 0)
      tester.expect("io_out_1", 0)

      tester.poke("reset", 1)
      tester.expect("io_out_0", 0)
      tester.expect("io_out_1", 1)

      tester.step()
      tester.poke("reset", 0)
      tester.expect("io_out_0", 0)
      tester.expect("io_out_1", 1)

      tester.step()
      tester.expect("io_out_0", 1)
      tester.expect("io_out_1", 0)

      tester.step()
      tester.expect("io_out_0", 0)
      tester.expect("io_out_1", 1)
    }
  }
}
