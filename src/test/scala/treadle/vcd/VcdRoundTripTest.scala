// SPDX-License-Identifier: Apache-2.0

package treadle.vcd

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle.{TreadleTestHarness, WriteVcdAnnotation}

class VcdRoundTripTest extends AnyFreeSpec with Matchers with LazyLogging {
  "create a vcd through treadle and then read it back in" in {
    val input =
      """
        |;buildInfoPackage: chisel3, version: 3.3-SNAPSHOT, scalaVersion: 2.12.10, sbtVersion: 1.3.2
        |circuit TopModule :
        |  module ModuleA :
        |    input clock : Clock
        |    input reset : Reset
        |    input a : UInt<16>
        |    input b : UInt<16>
        |    output c : UInt<16>
        |
        |    node _T = add(a, b) @[HierarchicalNaming.scala 23:23]
        |    node _T_1 = tail(_T, 1) @[HierarchicalNaming.scala 23:23]
        |    reg _T_2 : UInt, clock @[HierarchicalNaming.scala 23:20]
        |    _T_2 <= _T_1 @[HierarchicalNaming.scala 23:20]
        |    c <= _T_2 @[HierarchicalNaming.scala 24:7]
        |
        |  module ModuleA_1 :
        |    input clock : Clock
        |    input reset : Reset
        |    input a : UInt<16>
        |    input b : UInt<16>
        |    output c : UInt<16>
        |
        |    inst ModuleA of ModuleA @[HierarchicalNaming.scala 18:19]
        |    ModuleA.clock <= clock
        |    ModuleA.reset <= reset
        |    ModuleA.a <= a @[HierarchicalNaming.scala 19:9]
        |    ModuleA.b <= b @[HierarchicalNaming.scala 20:9]
        |    c <= ModuleA.c @[HierarchicalNaming.scala 21:7]
        |
        |  module ModuleA_2 :
        |    input clock : Clock
        |    input reset : Reset
        |    input a : UInt<16>
        |    input b : UInt<16>
        |    output c : UInt<16>
        |
        |    inst ModuleA of ModuleA_1 @[HierarchicalNaming.scala 18:19]
        |    ModuleA.clock <= clock
        |    ModuleA.reset <= reset
        |    ModuleA.a <= a @[HierarchicalNaming.scala 19:9]
        |    ModuleA.b <= b @[HierarchicalNaming.scala 20:9]
        |    c <= ModuleA.c @[HierarchicalNaming.scala 21:7]
        |
        |  module ModuleA_3 :
        |    input clock : Clock
        |    input reset : Reset
        |    input a : UInt<16>
        |    input b : UInt<16>
        |    output c : UInt<16>
        |
        |    node _T = add(a, b) @[HierarchicalNaming.scala 23:23]
        |    node _T_1 = tail(_T, 1) @[HierarchicalNaming.scala 23:23]
        |    reg _T_2 : UInt, clock @[HierarchicalNaming.scala 23:20]
        |    _T_2 <= _T_1 @[HierarchicalNaming.scala 23:20]
        |    c <= _T_2 @[HierarchicalNaming.scala 24:7]
        |
        |  module TopModule :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input topA : UInt<16>
        |    input topB : UInt<16>
        |    output topC : UInt<16>
        |
        |    inst mpath2 of ModuleA_2 @[HierarchicalNaming.scala 33:22]
        |    mpath2.clock <= clock
        |    mpath2.reset <= reset
        |    inst mpath0 of ModuleA_3 @[HierarchicalNaming.scala 34:22]
        |    mpath0.clock <= clock
        |    mpath0.reset <= reset
        |    mpath2.a <= topA @[HierarchicalNaming.scala 36:12]
        |    mpath2.b <= topB @[HierarchicalNaming.scala 37:12]
        |    mpath0.a <= topA @[HierarchicalNaming.scala 38:12]
        |    mpath0.b <= topB @[HierarchicalNaming.scala 39:12]
        |    node _T = add(mpath2.c, mpath0.c) @[HierarchicalNaming.scala 41:20]
        |    node _T_1 = tail(_T, 1) @[HierarchicalNaming.scala 41:20]
        |    topC <= _T_1 @[HierarchicalNaming.scala 41:8]
        |
        |
        |""".stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input), WriteVcdAnnotation)) { tester =>
      for {
        a <- (3 until 10)
        b <- (7 until 22)
      } {
        tester.poke("topA", a)
        tester.poke("topB", b)
        tester.step()

        logger.debug(s"f(a = $a,  b = $b) => ${tester.peek("topC")}")
      }

      tester.engine.writeVCD() // This flushes vcd file

      val readVcd = VCD.read("test_run_dir/TopModule/TopModule.vcd")
      val madeVcd = tester.engine.vcdOption.get

      val madeKeys = madeVcd.wires.keys.toSeq.sorted
      val readKeys = readVcd.wires.keys.toSeq.sorted

      madeKeys.zip(readKeys).foreach { case (k1, k2) =>
        k1 should be(k2)
      }

      madeKeys.foreach { key =>
        readVcd.wires(key).fullName should be(madeVcd.wires(key).fullName)
        logger.debug(f"${readVcd.wires(key).fullName}%60s  ===  ${madeVcd.wires(key).fullName}")
      }
    }
  }
}
