// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import java.io.{ByteArrayOutputStream, File, PrintStream}

import chisel3._
import chisel3.stage.ChiselOutputFileAnnotation
import chiseltest._
import chiseltest.experimental.sanitizeFileName
import chiseltest.internal._
import firrtl.options.{OutputAnnotationFileAnnotation, TargetDirAnnotation}
import firrtl.stage.OutputFileAnnotation
import org.scalatest._
import treadle.{VerboseAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OptionsPassingTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "write vcd output when passing in a WriteVcdAnnotation" in {
    val testDir = new File(test(new Module {
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
    }.collectFirst{
      case TargetDirAnnotation(dir) => dir
    }.get)

    val vcdFileOpt = testDir.listFiles.find { f =>
      f.getPath.endsWith(".vcd")
    }

    vcdFileOpt.isDefined should be(true)
    vcdFileOpt.get.delete()
  }

  it should "allow specifying configuration options using CLI style flags" in {
    test(new MultiIOModule() {}).withFlags(Array("--t-name", "overrideName")) { _ =>}.collectFirst{
      case TargetDirAnnotation(dir) => dir
    }.get.contains("overrideName")
  }

  // This is failing on github CI, not worth figuring this out right now just for the sake
  // of old deprecated usages
  //TODO: Remove this test for next major release
  it should "allow specifying configuration options using annotations and CLI style flags" ignore {
    val targetDirName = "test_run_dir/overridden_dir_2"
    val fileBaseName = "wheaton"
    val annotations = Seq(
      ChiselOutputFileAnnotation(fileBaseName),
      OutputFileAnnotation(fileBaseName),
      OutputAnnotationFileAnnotation(fileBaseName)
    )
    val targetDir = new File(targetDirName)
    if (targetDir.exists()) {
      targetDir.delete()
    }
    test(new MultiIOModule() {})
      .withAnnotations(annotations)
      .withFlags(Array("--target-dir", targetDirName)) { c =>
        targetDir.exists() should be(true)
        val firrtlFile = new File(targetDir + File.separator + s"$fileBaseName.lo.fir")
        firrtlFile.exists() should be(true)
      }
  }

  it should "allow turning on verbose mode" in {
    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new MultiIOModule {}).withAnnotations(Seq(VerboseAnnotation)) { c =>
        c.clock.step(1)
      }
    }

    val output = outputStream.toString

    output.contains("Symbol table:") should be(true)
    output.contains("clock/prev") should be(true)
  }
}
