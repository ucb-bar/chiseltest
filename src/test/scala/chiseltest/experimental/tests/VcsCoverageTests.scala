// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import java.io.{ByteArrayOutputStream, File, PrintStream}

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VcsCoverageTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "allow specifying line coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {
        val io = IO(new Bundle {
          val a = Input(UInt(8.W))
          val b = Output(UInt(8.W))
        })
        io.b := io.a
      }).withAnnotations(Seq(VcsBackendAnnotation, LineCoverageAnnotation)) { c =>
        c.io.a.poke(1.U)
        c.io.b.expect(1.U)
        c.clock.step()
        c.io.a.poke(42.U)
        c.io.b.expect(42.U)
      }
    }
    val output = outputStream.toString
    output.contains("-cm line") should be(true)
  }

  it should "allow specifying toggle coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {
        val io = IO(new Bundle {
          val a = Input(UInt(8.W))
          val b = Output(UInt(8.W))
        })
        io.b := io.a
      }).withAnnotations(Seq(VcsBackendAnnotation, ToggleCoverageAnnotation)) { c =>
        c.io.a.poke(1.U)
        c.io.b.expect(1.U)
        c.clock.step()
        c.io.a.poke(42.U)
        c.io.b.expect(42.U)
      }
    }
    val output = outputStream.toString
    output.contains("-cm tgl") should be(true)
  }

  it should "allow specifying branch coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {
        val io = IO(new Bundle {
          val a = Input(UInt(8.W))
          val b = Output(UInt(8.W))
        })
        io.b := io.a
      }).withAnnotations(Seq(VcsBackendAnnotation, BranchCoverageAnnotation)) { c =>
        c.io.a.poke(1.U)
        c.io.b.expect(1.U)
        c.clock.step()
        c.io.a.poke(42.U)
        c.io.b.expect(42.U)
      }
    }
    val output = outputStream.toString
    output.contains("-cm branch") should be(true)
  }

  it should "allow specifying conditional coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {
        val io = IO(new Bundle {
          val a = Input(UInt(8.W))
          val b = Output(UInt(8.W))
        })
        io.b := io.a
      }).withAnnotations(Seq(VcsBackendAnnotation, ConditionalCoverageAnnotation)) { c =>
        c.io.a.poke(1.U)
        c.io.b.expect(1.U)
        c.clock.step()
        c.io.a.poke(42.U)
        c.io.b.expect(42.U)
      }
    }
    val output = outputStream.toString
    output.contains("-cm cond") should be(true)
  }

  it should "allow specifying structural coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {
        val io = IO(new Bundle {
          val a = Input(UInt(8.W))
          val b = Output(UInt(8.W))
        })
        io.b := io.a
      }).withAnnotations(Seq(VcsBackendAnnotation, StructuralCoverageAnnotation)) { c =>
        c.io.a.poke(1.U)
        c.io.b.expect(1.U)
        c.clock.step()
        c.io.a.poke(42.U)
        c.io.b.expect(42.U)
      }
    }
    val output = outputStream.toString
    output.contains("-cm line+tgl+branch+cond") should be(true)
  }

  it should "allow specifying user coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {
        val io = IO(new Bundle {
          val a = Input(UInt(8.W))
          val b = Output(UInt(8.W))
        })
        io.b := io.a
      }).withAnnotations(Seq(VcsBackendAnnotation, UserCoverageAnnotation)) { c =>
        c.io.a.poke(1.U)
        c.io.b.expect(1.U)
        c.clock.step()
        c.io.a.poke(42.U)
        c.io.b.expect(42.U)
      }
    }
    val output = outputStream.toString
    output.contains("-cm assert") should be(true)
  }

  it should "allow stacking coverage for Vcs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    val outputStream = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outputStream)) {
      test(new Module {
        val io = IO(new Bundle {
          val a = Input(UInt(8.W))
          val b = Output(UInt(8.W))
        })
        io.b := io.a
      }).withAnnotations(Seq(VcsBackendAnnotation, UserCoverageAnnotation, StructuralCoverageAnnotation)) { c =>
        c.io.a.poke(1.U)
        c.io.b.expect(1.U)
        c.clock.step()
        c.io.a.poke(42.U)
        c.io.b.expect(42.U)
      }
    }
    val output = outputStream.toString
    output.contains("-cm line+tgl+branch+cond+assert") should be(true)
  }
}