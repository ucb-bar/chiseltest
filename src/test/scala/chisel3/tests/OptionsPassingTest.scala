package chisel3.tests

import java.io.{ByteArrayOutputStream, File, PrintStream}

import chisel3._
import chisel3.tester._
import org.scalatest._
import treadle.{VerboseAnnotation, WriteVcdAnnotation}
import chisel3.tester.experimental.TestOptionBuilder._

class OptionsPassingTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "write vcd output when passing in a WriteVcdAnnotation" in {
    def shiftTest(in: UInt, out: UInt, clk: Clock, value: UInt) {
      timescope {
        in.poke(value)
        clk.step(1)
      }
      clk.step(3)
      out.expect(value)
    }

    test(new ShifterModule(UInt(8.W), 4))
            .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      fork { shiftTest(c.in, c.out, c.clock, 42.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 43.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 44.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 45.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 46.U) }
      c.clock.step(1)
      fork { shiftTest(c.in, c.out, c.clock, 47.U) }.join

      val vcdFileName = "test_run_dir" +
              File.separator +
              sanitizeFileName(s"Testers2 should write vcd output when passing in a WriteVcdAnnotation") +
              File.separator +
              sanitizeFileName("ShifterModule") +
              ".vcd"

      val vcdFile = new File(vcdFileName)
      println(s"vcd file name $vcdFileName ${vcdFile.getAbsolutePath}")
      vcdFile.exists() should be (true)
      vcdFile.delete()
    }
  }

  it should "allow turning on verbose mode" in {
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new ShifterModule(UInt(8.W), 4)).withAnnotations(Seq(VerboseAnnotation)) { c =>
        c.clock.step(1)
      }
    }

    val output = outputStream.toString

    output.contains("Symbol table:") should be (true)
    output.contains("clock/prev") should be (true)
  }
}
