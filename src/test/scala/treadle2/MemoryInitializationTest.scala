// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.annotations._
import firrtl.stage.{FirrtlFileAnnotation, FirrtlSourceAnnotation}
import logger.{LazyLogging, LogLevel, Logger}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must._

class MemoryInitializationTest extends AnyFreeSpec with Matchers with LazyLogging {
  "Memories should be loadable using annotations" in {
    val firrtlText =
      """
        |;buildInfoPackage: chisel3, version: 3.4-SNAPSHOT, scalaVersion: 2.12.11, sbtVersion: 1.3.10
        |circuit HasMemories :
        |  module HasMemories :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input address : UInt<4>
        |    output out1 : UInt<4>
        |    output out2 : UInt<4>
        |    output out3 : UInt<4>
        |
        |    cmem memory1 : UInt<4>[16] @[MemoryLoaderTest.scala 28:20]
        |    cmem memory2 : UInt<4>[16] @[MemoryLoaderTest.scala 29:20]
        |    cmem memory3 : UInt<4>[16] @[MemoryLoaderTest.scala 30:20]
        |    infer mport _T = memory1[address], clock @[MemoryLoaderTest.scala 36:18]
        |    out1 <= _T @[MemoryLoaderTest.scala 36:8]
        |    infer mport _T_1 = memory2[address], clock @[MemoryLoaderTest.scala 37:18]
        |    out2 <= _T_1 @[MemoryLoaderTest.scala 37:8]
        |    infer mport _T_2 = memory3[address], clock @[MemoryLoaderTest.scala 38:18]
        |    out3 <= _T_2 @[MemoryLoaderTest.scala 38:8]
        |
        |    """.stripMargin

    def getMemoryReference(memoryNumber: Int): ReferenceTarget = {
      CircuitTarget("HasMemories").module("HasMemories").ref(s"memory$memoryNumber")
    }

    TreadleTestHarness(
      Seq(
        FirrtlSourceAnnotation(firrtlText),
        MemoryRandomInitAnnotation(getMemoryReference(1)),
        MemoryScalarInitAnnotation(getMemoryReference(2), 7),
        MemoryArrayInitAnnotation(
          getMemoryReference(3),
          Seq.tabulate(16) { i =>
            i
          }
        )
      )
    ) { tester =>
      Logger.setLevel(classOf[MemoryInitializationTest], LogLevel.None)
      for (i <- 0 until 16) {
        tester.poke("address", i)
        tester.step()
        logger.debug(s"${tester.peek("out1")} : ${tester.peek("out2")} : ${tester.peek("out3")}")
      }
    }
  }

  "Memories should be loadable even in submodules" in {
    val firrtlText =
      """
        |;buildInfoPackage: chisel3, version: 3.4-SNAPSHOT, scalaVersion: 2.12.11, sbtVersion: 1.3.10
        |circuit Topper :
        |  module Bottomer :
        |    input clock : Clock
        |    input reset : Reset
        |    input address : UInt<4>
        |    output out : UInt<4>
        |
        |    cmem memory1 : UInt<4>[16] @[MemoryLoaderTest.scala 46:20]
        |    infer mport _T = memory1[address], clock @[MemoryLoaderTest.scala 49:17]
        |    out <= _T @[MemoryLoaderTest.scala 49:7]
        |
        |  module Middler :
        |    input clock : Clock
        |    input reset : Reset
        |    input address : UInt<4>
        |    output out1 : UInt<4>
        |    output out2 : UInt<4>
        |    output out3 : UInt<4>
        |
        |    cmem memory1 : UInt<4>[16] @[MemoryLoaderTest.scala 54:20]
        |
        |    inst bottomModule of Bottomer @[MemoryLoaderTest.scala 56:28]
        |    bottomModule.clock <= clock
        |    bottomModule.reset <= reset
        |    bottomModule.address <= address @[MemoryLoaderTest.scala 57:24]
        |
        |    inst bottomModule2 of Bottomer @[MemoryLoaderTest.scala 56:28]
        |    bottomModule2.clock <= clock
        |    bottomModule2.reset <= reset
        |    bottomModule2.address <= address @[MemoryLoaderTest.scala 57:24]
        |
        |    infer mport _T = memory1[address], clock @[MemoryLoaderTest.scala 61:18]
        |    out1 <= _T @[MemoryLoaderTest.scala 61:8]
        |    out2 <= bottomModule.out @[MemoryLoaderTest.scala 62:8]
        |    out3 <= bottomModule2.out @[MemoryLoaderTest.scala 62:8]
        |
        |  module Topper :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input address : UInt<4>
        |    output out1 : UInt<4>
        |    output out2 : UInt<4>
        |    output out3 : UInt<4>
        |    output out4 : UInt<4>
        |    output out5 : UInt<4>
        |    output out6 : UInt<4>
        |    output out7 : UInt<4>
        |    output out8 : UInt<4>
        |
        |    cmem memory1 : UInt<4>[16] @[MemoryLoaderTest.scala 68:20]
        |    cmem memory2 : UInt<4>[16] @[MemoryLoaderTest.scala 68:20]
        |
        |    inst middleModule of Middler @[MemoryLoaderTest.scala 70:28]
        |    middleModule.clock <= clock
        |    middleModule.reset <= reset
        |    middleModule.address <= address @[MemoryLoaderTest.scala 71:24]
        |    infer mport _T = memory1[address], clock @[MemoryLoaderTest.scala 77:18]
        |    out1 <= _T @[MemoryLoaderTest.scala 77:8]
        |    out2 <= middleModule.out1 @[MemoryLoaderTest.scala 78:8]
        |    out3 <= middleModule.out2 @[MemoryLoaderTest.scala 79:8]
        |    out4 <= middleModule.out3 @[MemoryLoaderTest.scala 79:8]
        |
        |    inst middleModule2 of Middler @[MemoryLoaderTest.scala 70:28]
        |    middleModule2.clock <= clock
        |    middleModule2.reset <= reset
        |    middleModule2.address <= address @[MemoryLoaderTest.scala 71:24]
        |    infer mport _T_2 = memory2[address], clock @[MemoryLoaderTest.scala 77:18]
        |    out5 <= _T_2 @[MemoryLoaderTest.scala 77:8]
        |    out6 <= middleModule2.out1 @[MemoryLoaderTest.scala 78:8]
        |    out7 <= middleModule2.out2 @[MemoryLoaderTest.scala 79:8]
        |    out8 <= middleModule2.out3 @[MemoryLoaderTest.scala 79:8]
        |
        |""".stripMargin

    /*
    The following is an example of a partial path
    i.e. everything below a particular module
     */
    val allBottom2memory1s = ReferenceTarget(
      "Topper",
      "Bottomer",
      Seq(
        (Instance("bottomModule2"), OfModule("Bottomer"))
      ),
      "memory1",
      Seq.empty
    )

    val middleModule2bottomModule = ReferenceTarget(
      "Topper",
      "Bottomer",
      Seq(
        (Instance("middleModule2"), OfModule("Middler")),
        (Instance("bottomModule"), OfModule("Bottomer"))
      ),
      "memory1",
      Seq.empty
    )

    util.Random.setSeed(0L)

    TreadleTestHarness(
      Seq(
        FirrtlSourceAnnotation(firrtlText),
        SaveFirrtlAtLoadAnnotation,
        //out1
        MemoryScalarInitAnnotation(CircuitTarget("Topper").module("Topper").ref("memory1"), 7),
        // out5
        MemoryRandomInitAnnotation(CircuitTarget("Topper").module("Topper").ref("memory2")),
        // out2 and out 6
        MemoryArrayInitAnnotation(
          CircuitTarget("Topper").module("Middler").ref("memory1"),
          Seq.tabulate(16) { i =>
            (i * 2) % 16
          }
        ),
        // out4 and out8
        MemoryArrayInitAnnotation(
          allBottom2memory1s,
          Seq.tabulate(16) { i =>
            15 - i
          }
        ),
        // out7
        MemoryArrayInitAnnotation(
          middleModule2bottomModule,
          Seq.tabulate(16) { i =>
            (i + 7) % 16
          }
        )
      )
    ) { tester =>
      Logger.setLevel(classOf[MemoryInitializationTest], LogLevel.None)

      logger.debug {
        val headers = Seq(
          ("out1", "t.m1"),
          ("out2", "t.mm.m1"),
          ("out3", "t.mm.bm.m1"),
          ("out4", "t.mm.bm2.m1"),
          ("out5", "t.m2"),
          ("out6", "t.mm2.m1"),
          ("out7", "t.mm2.bm.m1"),
          ("out8", "t.mm2.bm2.m1")
        )

        headers.map { a =>
          f"${a._1}%12s"
        }.mkString("") + "\n" +
          headers.map { a =>
            f"${a._2}%12s"
          }.mkString("") + "\n"
      }

      var out5Sum = 0

      for (i <- 0 until 16) {
        tester.poke("address", i)
        tester.step()

        logger.debug(
          (1 to 8).map { outNum =>
            f"""${tester.peek(s"out$outNum")}%12d"""
          }.mkString("")
        )

        tester.expect("out1", 7)
        tester.expect("out2", (i * 2) % 16)
        tester.expect("out3", 0)
        tester.expect("out4", 15 - i)
        // out5 is random
        out5Sum = out5Sum + tester.peek("out5").toInt
        tester.expect("out6", (i * 2) % 16)
        tester.expect("out7", (i + 7) % 16)
        tester.expect("out8", 15 - i)
      }

      logger.debug(f"Average of out5 (random) is ${out5Sum / 16.0}%6.2f")
      out5Sum must be > 0
    }
  }

  private def testMemFileInit(filename: String, expected: Seq[Int], tpe: MemoryLoadFileType): Unit = {
    val annos = Seq(
      FirrtlFileAnnotation("src/test/resources/treadle/ReadMem.fir"),
      MemoryFileInlineAnnotation(
        CircuitTarget("ReadMem").module("ReadMem").ref("m"),
        s"src/test/resources/treadle/$filename",
        tpe
      )
    )
    TreadleTestHarness(annos) { tester =>
      expected.zipWithIndex.foreach { case (expect, addr) =>
        tester.poke("addr", addr)
        tester.step()
        assert(tester.peek("value") == expect)
      }
      tester.step()
      tester.finish
    }
  }

  "Initialize memories from file with hex values" in {
    testMemFileInit("ReadMem.hex", Seq(0xab, 0xcd, 0xef), MemoryLoadFileType.Hex)
  }

  "Initialize memories from file with binary values" in {
    testMemFileInit("ReadMem.bin", Seq(2, 0, 1), MemoryLoadFileType.Binary)
  }
}
