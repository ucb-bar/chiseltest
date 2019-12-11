package chiseltest.tests

import java.io.{ByteArrayOutputStream, File, PrintStream}

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.experimental.sanitizeFileName
import org.scalatest._
import treadle.{VerboseAnnotation, WriteVcdAnnotation}

class OptionsPassingTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "write vcd output when passing in a WriteVcdAnnotation" in {
    test(new Module {
      val io = IO(new Bundle {
        val a = Input(UInt(8.W))
        val b = Output(UInt(8.W))
      })
      io.b := io.a
    }).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.a.poke(1.U)
      c.io.b.expect(1.U)
      c.clock.step()
      c.io.a.poke(42.U)
      c.io.b.expect(42.U)
    }

    val testDir = new File("test_run_dir" +
            File.separator +
            sanitizeFileName(s"Testers2 should write vcd output when passing in a WriteVcdAnnotation"))

    val vcdFileOpt = testDir.listFiles.find { f =>
      f.getPath.endsWith(".vcd")
    }

    vcdFileOpt.isDefined should be (true)
    vcdFileOpt.get.delete()
  }

  it should "allow turning on verbose mode" in {
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new MultiIOModule {}).withAnnotations(Seq(VerboseAnnotation)) { c =>
        c.clock.step(1)
      }
    }

    val output = outputStream.toString

    output.contains("Symbol table:") should be (true)
    output.contains("clock/prev") should be (true)
  }
}
