// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator

import firrtl.AnnotationSeq
import org.scalatest.Tag

import java.nio.ByteBuffer

/** tests the waveform dumping abilities of a simulator */
abstract class WaveformCompliance(sim: Simulator, tag: Tag = DefaultTag) extends ComplianceTest(sim, tag) {

  private val testSrc =
    """circuit Foo :
      |  module Child :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output io : { flip in : UInt<1>, out : UInt<1>}
      |
      |    reg r : UInt<1>, clock with :
      |      reset => (reset, UInt(0))
      |    r <= io.in
      |    io.out <= r
      |
      |  module Foo :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output io : { flip in : UInt<1>, out : UInt<1>}
      |
      |    node _io_out_T = not(io.in) @[main.scala 12:13]
      |    inst c of Child
      |    c.clock <= clock
      |    c.reset <= reset
      |    c.io.in <= _io_out_T @[main.scala 12:10]
      |    io.out <= c.io.out
      |""".stripMargin

  val waveformExtensions = Set("vcd", "txt", "fst", "vpd", "lxt", "lxt2", "fsdb")
  it should "not create a waveform file when no WriteWaveformAnnotation is provided" taggedAs(tag) in {
    performDutTest(Seq())
    val dumps = testDirFiles().filter(f => waveformExtensions.contains(f.last.split('.').last))
    assert(dumps.isEmpty, s"Found waveform dump files $dumps even though no WriteWaveformAnnotation was provided!")
  }

  sim.waveformFormats.foreach { anno =>
    val format = anno.format
    it should s"generate a $format waveform dump at the end of simulation" taggedAs(tag) in {
      performDutTest(Seq(anno))
      val dumpFile = targetDir / ("Foo." + format)
      checkWavedump(dumpFile, format)
    }
  }

  // performs some sanity checks on a given waveform
  private def checkWavedump(file: os.Path, format: String): Unit = {
    assert(os.exists(file) && os.isFile(file), s"Waveform dump $file does not exist!")

    format match {
      case "vcd" =>
        // the VCD format is a text file that will contain some keyword we can search for
        val txt = os.read(file)
        assert(txt.contains("$version"))
        assert(txt.contains("$scope"))
        assert(txt.contains("$enddefinitions"))
      case "lxt" =>
        // the LXT format has a characteristic header ID followed by the version number
        // lt_emit_u16(lt, LT_HDRID);    // #define LT_HDRID (0x0138)
        // lt_emit_u16(lt, LT_VERSION);  // #define LT_VERSION (0x0004)
        val bb = ByteBuffer.wrap(os.read.bytes(file))
        assert(bb.getShort == 0x0138)
        val version = bb.getShort
        assert(version == 4) // TODO: potentially relax this version requirement
      case "lxt2" =>
        // the LXT2 format has a characteristic header ID followed by the version number
        // lxt2_wr_emit_u16(lt, LXT2_WR_HDRID);   // #define LXT2_WR_HDRID (0x1380)
        // lxt2_wr_emit_u16(lt, LXT2_WR_VERSION); // #define LXT2_WR_VERSION (0x0001)
        // lxt2_wr_emit_u8 (lt, LXT2_WR_GRANULE_SIZE);	/* currently 32 or 64 */
        val bb = ByteBuffer.wrap(os.read.bytes(file))
        assert(bb.getShort == 0x1380)
        val version = bb.getShort
        val granule_size = bb.get()
        assert(granule_size == 32 || granule_size == 64)
        assert(version == 1) // TODO: potentially relax this version requirement
      case "fst" =>
        // we look for characteristic header bytes defined in `fstWriterEmitHdrBytes`
        val bb = ByteBuffer.wrap(os.read.bytes(file))
        // fstBlockType: FST_BL_HDR = 0
        assert(bb.get() == 0)
        // section length is hard coded to 329
        assert(bb.getLong() == 329)
      case "vpd" =>
        val txt = os.read(file).toString.take(3)
        assert(txt.startsWith("xV4"))
      case "fsdb" =>
        val txt = os.read(file)
        assert(txt.contains("FSDB Writer"))
        assert(txt.contains("finish"))
      case other => throw new NotImplementedError(s"TODO: add code to check $other")
    }
  }

  // perform some testing with the dut in order to generate interesting waveforms
  private def performDutTest(annos: AnnotationSeq): Unit = {
    val dut = load(testSrc, annos = annos)
    dut.poke("reset", 1)
    dut.step()
    dut.poke("reset", 0)
    assert(dut.peek("io_out") == 0)
    val rand = new scala.util.Random(0)
    (0 until 50).foreach { _ =>
      val in = BigInt(1, rand)
      dut.poke("io_in", in)
      dut.step()
      assert(dut.peek("io_out") == ((~in) & 1))
    }
    dut.step()
    dut.finish()
  }

  private def testDirFiles(): Seq[os.Path] = os.list(targetDir).filter(os.isFile)
}
