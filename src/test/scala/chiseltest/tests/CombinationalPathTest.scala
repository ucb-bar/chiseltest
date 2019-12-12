package chiseltest.tests

import org.scalatest._

import chisel3._
import chiseltest._

class CombinationalPathTest extends FlatSpec with ChiselScalatestTester {
  behavior of "Testers2"

  it should "detect combinationally-dependent operations when a poke is active" in {
    assertThrows[ThreadOrderDependentException] {
      test(new PassthroughModule(Bool())) { c =>
        fork {
          c.in.poke(true.B)
          c.clock.step(2)
        } .fork {
          c.clock.step(1)
          c.out.expect(true.B)
        } .join
      }
    }
  }

  it should "detect combinationally-dependent operations through internal modules" in {
    assertThrows[ThreadOrderDependentException] {
      test(new Module {
        val io = IO(new Bundle {
          val in = Input(Bool())
          val out = Output(Bool())
        })
        val innerModule = Module(new PassthroughModule(Bool()))
        innerModule.in := io.in
        io.out := innerModule.out
      }) { c =>
        fork {
          c.io.in.poke(true.B)
          c.clock.step(2)
        } .fork {
          c.clock.step(1)
          c.io.out.expect(true.B)
        } .join
      }
    }
  }

  it should "detect combinational paths across operations" in {
    assertThrows[ThreadOrderDependentException] {
      test(new Module {
        val io = IO(new Bundle {
          val in1 = Input(Bool())
          val in2 = Input(Bool())
          val out = Output(Bool())
        })
        io.out := io.in1 || io.in2
      }) { c =>
        fork {
          c.io.in1.poke(true.B)
          c.clock.step(2)
        } .fork {
          c.clock.step(1)
          c.io.out.expect(true.B)
        } .join
      }
    }
  }
}
