// SPDX-License-Identifier: Apache-2.0

package treadle2.blackboxes

import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle2.{BlackBoxFactoriesAnnotation, PlusArgsAnnotation, TreadleTestHarness, WriteVcdAnnotation}

// scalastyle:off magic.number
class PlusArgReaderSpec extends AnyFreeSpec {
  "plus-args allow changes to values inside of compiled modules" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesPlusArgReader :
        |  extmodule PlusArgReader :
        |    output out : UInt<128>
        |
        |    defname = plusarg_reader
        |
        |    parameter DEFAULT = 1
        |    parameter WIDTH = 32
        |    parameter FORMAT = "value=%d"
        |
        |  module UsesPlusArgReader :
        |    input clock : Clock
        |    output out : UInt<128>
        |
        |    inst plusArgReader of PlusArgReader
        |
        |    out <= plusArgReader.out
        |
      """.stripMargin

    val options = Seq(
      WriteVcdAnnotation,
      BlackBoxFactoriesAnnotation(Seq(new BuiltInBlackBoxFactory)),
      PlusArgsAnnotation(Seq("+value=11"))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      tester.expect("out", BigInt(11))
    }
  }

  "make sure we can handle more than one plus-args from command line" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.2-SNAPSHOT, scalaVersion: 2.12.6, sbtVersion: 1.2.6
        |circuit UsesPlusArgReader :
        |  extmodule PlusArgReader :
        |    output out : UInt<128>
        |
        |    defname = plusarg_reader
        |
        |    parameter DEFAULT = 1
        |    parameter WIDTH = 32
        |    parameter FORMAT = "value=%d"
        |
        |  extmodule PlusArgReader_2 :
        |    output out : UInt<128>
        |
        |    defname = plusarg_reader
        |
        |    parameter DEFAULT = 1
        |    parameter WIDTH = 32
        |    parameter FORMAT = "second_value=%d"
        |
        |  module UsesPlusArgReader :
        |    input clock : Clock
        |    output out : UInt<128>
        |    output out2 : UInt<128>
        |
        |    inst plusArgReader of PlusArgReader
        |    inst plusArgReader2 of PlusArgReader_2
        |
        |    out <= plusArgReader.out
        |    out2 <= plusArgReader2.out
        |
      """.stripMargin

    val options = Seq(
      WriteVcdAnnotation,
      BlackBoxFactoriesAnnotation(Seq(new BuiltInBlackBoxFactory)),
      PlusArgsAnnotation(Seq("+value=11", "+second_value=22"))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      tester.expect("out", BigInt(11))
      tester.expect("out2", BigInt(22))
    }
  }
}
