// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.options.TargetDirAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import firrtl.{AnnotationSeq, FileUtils}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import treadle2.executable.TreadleException

class MemoryVcdSpec extends AnyFreeSpec with Matchers {
  private val HasTwoMems =
    s"""
       |circuit a :
       |  module a :
       |    input clock : Clock
       |    input reset : UInt<1>
       |
       |    input  data1_in    : UInt<8>
       |    output data1_out   : UInt<3>
       |    input  data2_in    : UInt<8>
       |    output data2_out   : UInt<3>
       |
       |    reg cnt : UInt<8>, clock with : (reset => (reset, UInt<256>("h00")))
       |    cnt <= add(cnt, UInt<1>("h01"))
       |
       |    mem memory1 :
       |      data-type => UInt<8>
       |      depth => 256
       |      read-latency => 0
       |      write-latency => 1
       |      reader => r
       |      writer => w
       |
       |    mem memory2 :
       |      data-type => UInt<8>
       |      depth => 256
       |      read-latency => 0
       |      write-latency => 1
       |      reader => r
       |      writer => w
       |
       |    data1_out         <= memory1.r.data
       |    memory1.r.addr    <= cnt
       |    memory1.r.en      <= UInt<1>("h01")
       |    memory1.r.clk     <= clock
       |
       |    memory1.w.addr <= cnt
       |    memory1.w.en   <= UInt<1>("h01")
       |    memory1.w.mask <= UInt<1>("h01")
       |    memory1.w.clk  <= clock
       |    memory1.w.data <= data1_in
       |
       |    data2_out         <= memory2.r.data
       |    memory2.r.addr    <= cnt
       |    memory2.r.en      <= UInt<1>("h01")
       |    memory2.r.clk     <= clock
       |
       |    memory2.w.addr <= cnt
       |    memory2.w.en   <= UInt<1>("h01")
       |    memory2.w.mask <= UInt<1>("h01")
       |    memory2.w.clk  <= clock
       |    memory2.w.data <= data1_in
       |
       |""".stripMargin

  "memory vcd writing tests" - {
    def runTest(dir: String, annotationSeq: AnnotationSeq): Unit = {
      TreadleTestHarness(
        Seq(
          FirrtlSourceAnnotation(HasTwoMems),
          TargetDirAnnotation(dir),
          WriteVcdAnnotation,
          VcdShowUnderScoredAnnotation
        ) ++
          annotationSeq
      ) { tester =>
        for (i <- 0 to 20) {
          tester.poke("data1_in", i)
          tester.poke("data2_in", i)
          tester.step()
        }
      }
    }

    def checkResults(dir: String, shouldAppear: Seq[String], shouldNotAppear: Seq[String]): Boolean = {
      val fileName = s"$dir/a.vcd"
      val vcdText = FileUtils.getLines(fileName)

      for (text <- shouldAppear) {
        assert(
          vcdText.exists { line =>
            line.contains(text)
          },
          s""""$text" should appear in $fileName"""
        )
      }

      for (text <- shouldNotAppear) {
        assert(
          !vcdText.exists { line =>
            line.contains(text)
          },
          s""""$text" should NOT appear in $fileName"""
        )
      }

      true
    }

    "memories are not include in vcd output by default" in {
      val dir = "test_run_dir/memory_vcd_test_no_logging"
      runTest(dir, Seq.empty)
      checkResults(dir, Seq.empty, Seq("memory1(", "memory2("))
    }

    "all memories can be included with the command all" in {
      val dir = "test_run_dir/memory_vcd_test_all"
      runTest(dir, Seq(MemoryToVCD("all")))
      checkResults(
        dir,
        Seq.tabulate(20) { i =>
          s"memory1($i)"
        } ++ Seq.tabulate(20) { i =>
          s"memory2($i)"
        },
        Seq.empty
      )
    }

    "all of just one memory can be included with the command memoryName:all" in {
      val dir = "test_run_dir/memory_vcd_test_all_memory1"
      runTest(dir, Seq(MemoryToVCD("memory1:all")))
      checkResults(
        dir,
        Seq.tabulate(20) { i =>
          s"memory1($i)"
        },
        Seq.tabulate(20) { i =>
          s"memory2($i)"
        }
      )
    }

    "specific indices of a given memory can be logged" in {
      val dir = "test_run_dir/memory_vcd_test_selected"
      runTest(dir, Seq(MemoryToVCD("memory1:0-4"), MemoryToVCD("memory2:10-14")))
      checkResults(
        dir,
        Seq.tabulate(5) { i =>
          s"memory1($i)"
        } ++
          Seq.tabulate(5) { i =>
            s"memory2(${i + 10})"
          },
        Seq.tabulate(5) { i =>
          s"memory2($i)"
        } ++
          Seq.tabulate(5) { i =>
            s"memory1(${i + 10})"
          }
      )
    }

    "indices can be specified in hex and octal and binary" in {
      val dir = "test_run_dir/memory_vcd_index_radix"
      runTest(dir, Seq(MemoryToVCD("memory1:b11,b111"), MemoryToVCD("memory2:ha1,0xa2")))
      checkResults(
        dir,
        Seq(
          s"memory1(11)",
          s"memory1(111)",
          s"memory2(a1)",
          s"memory2(a2)"
        ),
        Seq.empty
      )
    }

    "throw a chisel exception if radices are changed within one memory" in {
      val dir = "test_run_dir/memory_vcd_index_radix"
      intercept[TreadleException] {
        runTest(dir, Seq(MemoryToVCD("memory1:b11,x111")))
      }
    }

    "throw a chisel exception memory not found" in {
      val dir = "test_run_dir/memory_vcd_index_radix"
      intercept[TreadleException] {
        runTest(dir, Seq(MemoryToVCD("memory3:b11,b111")))
      }
    }

    "throw a chisel exception if index ranges is bad" in {
      val dir = "test_run_dir/memory_vcd_index_radix"
      intercept[TreadleException] {
        runTest(dir, Seq(MemoryToVCD("memory3:d77-d66")))
      }
    }
  }
}
