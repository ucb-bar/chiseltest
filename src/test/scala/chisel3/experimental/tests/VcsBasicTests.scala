package chisel3.experimental.tests

import chisel3._
import chisel3.tester._
import chisel3.tester.experimental.TestOptionBuilder._
import chisel3.tester.internal.VcsBackendAnnotation
import chisel3.tests.{PassthroughModule, StaticModule}
import org.scalatest._

class VcsBasicTests extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2_Vcs"

  val annos = Seq(VcsBackendAnnotation)

  it should "test static circuits" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    test(new StaticModule(42.U)).withAnnotations(annos) { c =>
      c.out.expect(42.U)
    }
  }

  it should "fail on poking outputs" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    assertThrows[UnpokeableException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.poke(0.U)
      }
    }
  }

  it should "fail on expect mismatch" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    assertThrows[exceptions.TestFailedException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.expect(0.U)
      }
    }
  }

  it should "fail with user-defined message" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    intercept[exceptions.TestFailedException] {
      test(new StaticModule(42.U)).withAnnotations(annos) { c =>
        c.out.expect(0.U, "user-defined failure message =(")
      }
    }.getMessage should include ("user-defined failure message =(")
  }

  it should "test inputless sequential circuits" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    test(new Module {
      val io = IO(new Bundle {
        val out = Output(UInt(8.W))
      })
      val counter = RegInit(UInt(8.W), 0.U)
      counter := counter + 1.U
      io.out := counter
    }).withAnnotations(annos) { c =>
      c.io.out.expect(0.U)
      c.clock.step()
      c.io.out.expect(1.U)
      c.clock.step()
      c.io.out.expect(2.U)
      c.clock.step()
      c.io.out.expect(3.U)
    }
  }

  it should "test combinational circuits" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    test(new PassthroughModule(UInt(8.W))).withAnnotations(annos) { c =>
      c.in.poke(0.U)
      c.out.expect(0.U)
      c.in.poke(42.U)
      c.out.expect(42.U)
    }
  }

  it should "test sequential circuits" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(8.W))
        val out = Output(UInt(8.W))
      })
      io.out := RegNext(io.in, 0.U)
    }).withAnnotations(annos) { c =>
      c.io.in.poke(0.U)
      c.clock.step()
      c.io.out.expect(0.U)
      c.io.in.poke(42.U)
      c.clock.step()
      c.io.out.expect(42.U)
    }
  }

  it should "test reset" in {
    assume(firrtl.FileUtils.isVCSAvailable)

    test(new Module {
      val io = IO(new Bundle {
        val in = Input(UInt(8.W))
        val out = Output(UInt(8.W))
      })
      io.out := RegNext(io.in, 0.U)
    }).withAnnotations(annos) { c =>
      c.io.out.expect(0.U)

      c.io.in.poke(42.U)
      c.clock.step()
      c.io.out.expect(42.U)

      c.reset.poke(true.B)
      c.io.out.expect(42.U)  // sync reset not effective until next clk
      c.clock.step()
      c.io.out.expect(0.U)

      c.clock.step()
      c.io.out.expect(0.U)

      c.reset.poke(false.B)
      c.io.in.poke(43.U)
      c.clock.step()
      c.io.out.expect(43.U)
    }
  }
}
