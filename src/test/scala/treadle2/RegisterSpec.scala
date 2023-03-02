// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.annotations.{PresetAnnotation, ReferenceTarget, Target}
import firrtl.stage.FirrtlSourceAnnotation
import firrtl.transforms.{DontTouchAnnotation, NoDCEAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class RegisterSpec extends AnyFreeSpec with Matchers {
  "Register reset behavior" - {

    "it should reset registers when their condition is true" in {
      val input =
        """
          |circuit RegInc :
          |  module RegInc :
          |    input clock : Clock
          |    input reset1 : UInt<1>
          |    output out1 : UInt<16>
          |
          |    reg reg1 : UInt<16>, clock with : (reset => (reset1, UInt(3)))
          |
          |    reg1 <= mux(reset1, UInt<16>("h03"), add(reg1, UInt(1)))
          |    out1 <= reg1
          |
      """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation)) { tester =>
        tester.poke("reset1", 1)
        tester.step()
        tester.peek("reg1") should be(3)

        tester.poke("reset1", 0)
        tester.step()
        tester.peek("reg1") should be(4)

        tester.peek("reg1") should be(4)

        tester.poke("reset1", 0)
        tester.step()
        tester.peek("reg1") should be(5)

        tester.poke("reset1", 1)
        tester.step()
        tester.peek("reg1") should be(3)

        tester.poke("reset1", 0)
        tester.step()
        tester.peek("reg1") should be(4)
      }
    }

    "it should be able to initialize registers from other places" in {
      val input =
        """
          |circuit Stop0 :
          |  module Stop0 :
          |    input clk : Clock
          |    input reset1 : UInt<1>
          |    input reset2 : UInt<1>
          |    output out1 : UInt<16>
          |    output out2 : UInt<16>
          |
          |
          |    reg reg1 : UInt<16>, clk with : (reset => (reset1, UInt<16>(0)))
          |    reg reg2 : UInt<16>, clk with : (reset => (reset2, reg1))
          |
          |    reg1 <= add(reg1, UInt(1))
          |    reg2 <= add(reg2, UInt(3))
          |
          |    out1 <= reg1
          |    out2 <= reg2
          |
          |
      """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation)) { tester =>
        tester.poke("reset1", 1)
        tester.poke("reset2", 0)
        tester.step()
        tester.peek("reg1") should be(0)

        tester.poke("reset1", 0)
        tester.poke("reset2", 1)
        tester.step()
        tester.peek("reg1") should be(1)
        tester.peek("reg2") should be(0)

        tester.poke("reset1", 1)
        tester.poke("reset2", 1)
        tester.step()
        tester.peek("reg1") should be(0)
        tester.peek("reg2") should be(1)

        tester.poke("reset1", 0)
        tester.poke("reset2", 0)
        tester.step()
        tester.peek("reg1") should be(1)
        tester.peek("reg2") should be(4)

        tester.poke("reset1", 0)
        tester.poke("reset2", 0)
        tester.step()
        tester.peek("reg1") should be(2)
        tester.peek("reg2") should be(7)

        tester.poke("reset1", 1)
        tester.poke("reset2", 0)
        tester.step()
        tester.peek("reg1") should be(0)
        tester.peek("reg2") should be(10)

        tester.poke("reset1", 0)
        tester.poke("reset2", 0)
        tester.step()
        tester.peek("reg1") should be(1)
        tester.peek("reg2") should be(13)

        tester.poke("reset1", 0)
        tester.poke("reset2", 1)
        tester.step()
        tester.peek("reg1") should be(2)
        tester.peek("reg2") should be(1)
      }
    }
  }

  "reset support behavior" - {

    "it should have register decrement as reset lowers" in {
      val input =
        """
          |circuit RegInc :
          |  module RegInc :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output out1 : UInt<16>
          |
          |
          |    reg reg1 : UInt<16>, clock with : (reset => (reset, UInt(3)))  @[RegisterSpec.scala 131:20]
          |
          |    reg1 <= add(reg1, UInt(1))
          |    out1 <= reg1
          |
      """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
        tester.poke("reset", 1)
        tester.step()
        tester.poke("reset", 0)
        tester.step()
      }
    }
  }

  "reset support behavior, 2nd example" - {

    "it should reset takes precedence over next value" in {
      val input =
        """
          |circuit RegInc :
          |  module RegInc :
          |    input clock : Clock
          |    input reset1 : UInt<1>
          |    output out1 : UInt<16>
          |
          |    reg reg1 : UInt<16>, clock with : (reset => (reset1, UInt(3)))  @[RegisterSpec.scala 131:20]
          |
          |    reg1 <= add(reg1, UInt(1))
          |    out1 <= reg1
          |
          |
      """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation)) { tester =>
        tester.poke("reset1", 1)
        tester.step()
        tester.peek("reg1") should be(3)
        tester.step()
        tester.peek("reg1") should be(3)
        tester.step()
        tester.peek("reg1") should be(3)

        tester.poke("reset1", 0)
        tester.step()
        tester.peek("reg1") should be(4)

        tester.peek("reg1") should be(4)

        tester.poke("reset1", 0)
        tester.step()
        tester.peek("reg1") should be(5)

        tester.poke("reset1", 1)
        tester.peek("reg1") should be(5)
        tester.step()
        tester.peek("reg1") should be(3)
        tester.step()
        tester.peek("reg1") should be(3)

        tester.poke("reset1", 0)
        tester.peek("reg1") should be(3)
        tester.step()
        tester.peek("reg1") should be(4)
      }
    }
  }

  "poking registers behavior" - {

    "it should poke a register" in {
      val input =
        """
          |circuit Stop0 :
          |  module Stop0 :
          |    input clk : Clock
          |    input in : UInt<16>
          |    output out : UInt<16>
          |
          |    reg reg1 : UInt<16>, clk
          |    reg reg2 : UInt<16>, clk
          |    wire T_1 : UInt<16>
          |    wire T_2 : UInt<16>
          |
          |    reg1 <= in
          |    T_1 <= reg1
          |    reg2 <= T_1
          |    T_2 <= reg2
          |    out <= T_2
          |
      """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation)) { tester =>
        tester.poke("in", 7)
        tester.step()
        tester.peek("reg1") should be(7)
        tester.poke("in", 3)
        tester.step()
        tester.peek("reg1") should be(3)

        tester.poke("in", 8)
        tester.poke("reg1", 42)
        tester.peek("reg1") should be(42)
        tester.step()
        tester.peek("reg2") should be(42)
        tester.peek("reg1") should be(8)
      }
    }
  }

  "multi-clock registers behavior" - {

    "it should get the timing right" in {
      val input =
        """
          |circuit RegisterDependencies : @[:@2.0]
          |  module RegisterDependencies : @[:@3.2]
          |    input clock : Clock @[:@4.4]
          |    input reset : UInt<1> @[:@5.4]
          |    input io_in : UInt<16> @[:@6.4]
          |    input io_en : UInt<1> @[:@6.4]
          |    output io_o1 : UInt<16> @[:@6.4]
          |    output io_o2 : UInt<16> @[:@6.4]
          |
          |    reg reg1 : UInt<16>, clock with :
          |      reset => (UInt<1>("h0"), reg1)
          |    reg clockToggle : UInt<1>, clock with :
          |      reset => (UInt<1>("h0"), clockToggle)
          |    reg reg2 : UInt<16>, clock with :
          |      reset => (UInt<1>("h0"), reg2)
          |
          |    node _T_8 = add(reg1, UInt<1>("h1")) @[RegisterDependencies.scala 17:16:@9.4]
          |    node _T_9 = tail(_T_8, 1) @[RegisterDependencies.scala 17:16:@10.4]
          |
          |    node _T_13 = eq(clockToggle, UInt<1>("h0")) @[RegisterDependencies.scala 20:18:@13.4]
          |    node _T_14 = and(io_en, clockToggle) @[RegisterDependencies.scala 22:23:@15.4]
          |    node clock2 = asClock(_T_14) @[RegisterDependencies.scala 22:39:@16.4]
          |
          |    node _T_18 = add(reg2, UInt<1>("h1")) @[RegisterDependencies.scala 26:18:@18.4]
          |    node _T_19 = tail(_T_18, 1) @[RegisterDependencies.scala 26:18:@19.4]
          |
          |    io_o1 <= reg1
          |    io_o2 <= reg2
          |
          |    reg1 <= mux(reset, io_in, _T_9)
          |    clockToggle <= mux(reset, UInt<1>("h1"), _T_13)
          |    reg2 <= mux(reset, UInt<7>("h33"), _T_19)
          |
      """.stripMargin

      TreadleTestHarness(
        Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation)
      ) { tester =>
        tester.poke("io_in", 77)
        tester.poke("io_en", 0)
        tester.poke("reset", 1)
        tester.step()
        tester.expect("reg1/in", 77)
        tester.expect("reg2/in", 51)

        tester.poke("reset", 0)
        tester.step()
        tester.expect("reg1", 78)
        tester.expect("reg2", 52)

        tester.step()
        tester.expect("reg1", 79)
        tester.expect("reg2", 53)

        tester.poke("io_en", 1)
        tester.step()
        tester.expect("reg1", 80)
        tester.expect("reg2", 54)
      }
    }
  }

  "mutually connected registers behavior" - {

    "it should alternate values" in {
      val input =
        """
          |circuit AsyncResetRegTestanon6 :
          |  module AsyncResetRegTestanon6 :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io_out_0 : UInt<1>
          |    output io_out_1 : UInt<1>
          |
          |    reg reg0 : UInt<1>, clock with :
          |      reset => (UInt<1>("h0"), reg0)
          |    reg reg1 : UInt<1>, clock with :
          |      reset => (UInt<1>("h0"), reg1)
          |
          |    io_out_0 <= reg0 @[AsyncResetRegTest.scala 162:16]
          |    io_out_1 <= reg1 @[AsyncResetRegTest.scala 163:16]
          |    reg0 <= mux(reset, UInt<1>("h0"), reg1) @[AsyncResetRegTest.scala 159:12]
          |    reg1 <= mux(reset, UInt<1>("h1"), reg0) @[AsyncResetRegTest.scala 160:12]
      """.stripMargin

      TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), NoDCEAnnotation)) { tester =>
        tester.expect("io_out_0", 0)
        tester.expect("io_out_1", 0)
        tester.step()
        tester.expect("io_out_0", 0)
        tester.expect("io_out_1", 0)

        tester.poke("reset", 1)
        tester.expect("io_out_0", 0)
        tester.expect("io_out_1", 0)

        tester.step()
        tester.expect("io_out_0", 0)
        tester.expect("io_out_1", 1)

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

  "registers preset values behavior" - {

    "example one register in top module" - {
      val input =
        """
          |circuit Foo :
          |  module Foo :
          |    input clock : Clock
          |    input reset : AsyncReset
          |    output out : UInt<8>
          |
          |    reg r : UInt<8>, clock with :
          |      reset => (reset, UInt<8>("h7b")) @[RegisterPresetTest.scala 20:18]
          |    out <= r @[RegisterPresetTest.scala 23:7]
      """.stripMargin

      "Without preset annotations register starts at zero" in {
        TreadleTestHarness(
          Seq(
            FirrtlSourceAnnotation(input),
            DontTouchAnnotation(Target.deserialize("~Foo|Foo>r").asInstanceOf[ReferenceTarget])
          )
        ) { tester =>
          tester.expect("out", 0)
          tester.poke("reset", 1)
          tester.step()
          tester.expect("out", 0x7b)
        }
      }

      "With preset annotations register starts at init value" in {
        val presetAnnotation = PresetAnnotation(
          ReferenceTarget(
            circuit = "Foo",
            module = "Foo",
            path = Seq(),
            ref = "reset",
            component = Seq()
          )
        )

        TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), presetAnnotation)) { tester =>
          tester.expect("out", 123)
          tester.poke("reset", 1)
          tester.step()
          tester.expect("out", 0x7b)
        }

      }
    }

    "it should do presets correctly with submodules" - {
      val input =
        """
          |circuit Foo :
          |  module Baz :
          |    input clock : Clock
          |    input reset : AsyncReset
          |    output out : UInt<8>
          |
          |    reg r : UInt<8>, clock with :
          |      reset => (reset, UInt<32>("h0700")) @[RegisterPresetTest.scala 22:18]
          |    out <= r @[RegisterPresetTest.scala 25:7]
          |
          |  module Bar :
          |    input clock : Clock
          |    input reset : AsyncReset
          |    output out : UInt<32>
          |
          |    inst sub3 of Baz @[RegisterPresetTest.scala 34:20]
          |    sub3.clock <= clock
          |    sub3.reset <= reset
          |    inst sub4 of Baz @[RegisterPresetTest.scala 36:20]
          |    sub4.clock <= clock
          |    sub4.reset <= reset
          |    reg r : UInt<32>, clock with :
          |      reset => (reset, UInt<32>("hb0")) @[RegisterPresetTest.scala 40:18]
          |    node _out_T = add(r, sub3.out) @[RegisterPresetTest.scala 43:12]
          |    node _out_T_1 = tail(_out_T, 1) @[RegisterPresetTest.scala 43:12]
          |    node _out_T_2 = add(_out_T_1, sub4.out) @[RegisterPresetTest.scala 43:24]
          |    node _out_T_3 = tail(_out_T_2, 1) @[RegisterPresetTest.scala 43:24]
          |    out <= _out_T_3 @[RegisterPresetTest.scala 43:7]
          |
          |  module Foo :
          |    input clock : Clock
          |    input reset : AsyncReset
          |    output out1 : UInt<32>
          |    output out2 : UInt<32>
          |    output out3 : UInt<32>
          |
          |    reg r : UInt<32>, clock with :
          |      reset => (reset, UInt<32>("h0a")) @[RegisterPresetTest.scala 55:18]
          |    inst sub1 of Bar @[RegisterPresetTest.scala 59:22]
          |    sub1.clock <= clock
          |    sub1.reset <= reset
          |    out2 <= sub1.out @[RegisterPresetTest.scala 60:10]
          |    inst sub2 of Bar @[RegisterPresetTest.scala 62:22]
          |    sub2.clock <= clock
          |    sub2.reset <= reset
          |    out3 <= sub2.out @[RegisterPresetTest.scala 63:10]
          |    out1 <= r @[RegisterPresetTest.scala 69:8]
          |
    """.stripMargin

      val dontTouchAnnotation1 = DontTouchAnnotation(Target.deserialize("~Foo|Foo>r").asInstanceOf[ReferenceTarget])
      val dontTouchAnnotation21 =
        DontTouchAnnotation(Target.deserialize("~Foo|Foo/sub1:Bar>r").asInstanceOf[ReferenceTarget])
      val dontTouchAnnotation22 =
        DontTouchAnnotation(Target.deserialize("~Foo|Foo/sub2:Bar>r").asInstanceOf[ReferenceTarget])
      val dontTouchAnnotation313 =
        DontTouchAnnotation(Target.deserialize("~Foo|Foo/sub1:Bar/sub3:Baz>r").asInstanceOf[ReferenceTarget])
      val dontTouchAnnotation314 =
        DontTouchAnnotation(Target.deserialize("~Foo|Foo/sub1:Bar/sub4:Baz>r").asInstanceOf[ReferenceTarget])
      val dontTouchAnnotation323 =
        DontTouchAnnotation(Target.deserialize("~Foo|Foo/sub2:Bar/sub3:Baz>r").asInstanceOf[ReferenceTarget])
      val dontTouchAnnotation324 =
        DontTouchAnnotation(Target.deserialize("~Foo|Foo/sub2:Bar/sub4:Baz>r").asInstanceOf[ReferenceTarget])
      val dontTouches = Seq(
        dontTouchAnnotation1,
        dontTouchAnnotation21,
        dontTouchAnnotation22,
        dontTouchAnnotation313,
        dontTouchAnnotation314,
        dontTouchAnnotation323,
        dontTouchAnnotation324
      )

      //TODO: Reintroduce this test when register presets are implemented
      "With not preset annotations register starts at zero" ignore {
        TreadleTestHarness(
          Seq(FirrtlSourceAnnotation(input)) ++ dontTouches
        ) { tester =>
          tester.expect("out1", 0x0)
          tester.expect("out2", 0x0)
          tester.expect("out3", 0x0)

          tester.step()

          tester.expect("out1", 0x0)
          tester.expect("out2", 0x0)
          tester.expect("out3", 0x0)

          tester.poke("reset", 1)
          tester.step()

          tester.expect("out1", 0x0a)
          tester.expect("out2", 0xeb0)
          tester.expect("out3", 0xeb0)
        }
      }

      //TODO: Reintroduce this test when register presets are implemented
      "With preset annotations register starts at init value" ignore {
        val presetAnnotation1 = PresetAnnotation(Target.deserialize("~Foo|Foo>reset").asInstanceOf[ReferenceTarget])
        val presetAnnotation2 =
          PresetAnnotation(Target.deserialize("~Foo|Foo/sub2:Bar>reset").asInstanceOf[ReferenceTarget])
        val presetAnnotation3 =
          PresetAnnotation(Target.deserialize("~Foo|Foo/sub1:Bar/sub4:Baz>reset").asInstanceOf[ReferenceTarget])
        val presetAnnotation4 =
          PresetAnnotation(Target.deserialize("~Foo|Foo/sub2:Bar/sub4:Baz>reset").asInstanceOf[ReferenceTarget])

        val presetAnnotations = Seq(
          presetAnnotation1,
          presetAnnotation2,
          presetAnnotation3,
          presetAnnotation4
        )

        TreadleTestHarness(
          Seq(FirrtlSourceAnnotation(input)) ++ presetAnnotations ++ dontTouches
        ) { tester =>
          tester.expect("out1", 0x0a)
          tester.expect("out2", 0x700)
          tester.expect("out3", 0x7b0)

          tester.step()

          tester.expect("out1", 0x0a)
          tester.expect("out2", 0x700)
          tester.expect("out3", 0x7b0)

          tester.step()

          tester.expect("out1", 0x0a)
          tester.expect("out2", 0x700)
          tester.expect("out3", 0x7b0)

          tester.poke("reset", 1)
          tester.step()
          tester.poke("reset", 0)
          tester.step()

          tester.expect("out1", 0x0a)
          tester.expect("out2", 0xeb0)
          tester.expect("out3", 0xeb0)
        }
      }
    }
  }
}
