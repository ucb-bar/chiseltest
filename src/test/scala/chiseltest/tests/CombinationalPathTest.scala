// SPDX-License-Identifier: Apache-2.0

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CombinationalPathTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Testers2")

  it should "detect combinationally-dependent operations if they happen in the same step" in {
    assertThrows[ThreadOrderDependentException] {
      test(new PassthroughModule(Bool())) { c =>
        fork {
          c.clock.step(1)
          c.in.poke(true.B)
          c.clock.step(2)
        }.fork {
          c.clock.step(1)
          c.out.expect(true.B)
        }.join()
      }
    }
  }

  it should "detect r/w conflicts even if threads run in opposite order" in {
    assertThrows[ThreadOrderDependentException] {
      test(new PassthroughModule(Bool())) { c =>
        fork {
          c.clock.step(1)
          c.out.expect(true.B)
        }.fork {
          c.clock.step(1)
          c.in.poke(true.B)
          c.clock.step(2)
        }.join()
      }
    }
  }

  "unordered reads on dependent signals" should "be fine" in {
    test(new PassthroughModule(Bool())) { c =>
      fork {
        c.clock.step(1)
        c.in.peekInt()
      }.fork {
        c.clock.step(1)
        c.out.peekInt()
      }.join()
    }
  }

  "unordered reads on the same signal" should "be fine" in {
    test(new PassthroughModule(Bool())) { c =>
      fork {
        c.clock.step(1)
        c.out.peekInt()
      }.fork {
        c.clock.step(1)
        c.out.peekInt()
      }.join()
    }
  }

  it should "detect r/w conflicts on the same signal" in {
    assertThrows[ThreadOrderDependentException] {
      test(new PassthroughModule(Bool())) { c =>
        fork {
          c.clock.step(1)
          c.in.poke(true.B)
          c.clock.step(2)
        }.fork {
          c.clock.step(1)
          c.in.expect(true.B)
        }.join()
      }
    }
  }

  it should "detect r/w conflicts on the same signal even if threads run in opposite order" in {
    assertThrows[ThreadOrderDependentException] {
      test(new PassthroughModule(Bool())) { c =>
        fork {
          c.clock.step(1)
          c.in.expect(true.B)
        }.fork {
          c.clock.step(1)
          c.in.poke(true.B)
          c.clock.step(2)
        }.join()
      }
    }
  }

  it should "allow combinationally-dependent operations if they are synchronized by a clock step" in {
    test(new PassthroughModule(Bool())) { c =>
      fork {
        c.in.poke(true.B)
        c.clock.step(2)
      }.fork {
        c.clock.step(1)
        c.out.expect(true.B)
      }.join()
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
          c.clock.step(1)
          c.io.in.poke(true.B)
          c.clock.step(2)
        }.fork {
          c.clock.step(1)
          c.io.out.expect(true.B)
        }.join()
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
          c.clock.step(1)
          c.io.in1.poke(true.B)
          c.clock.step(2)
        }.fork {
          c.clock.step(1)
          c.io.out.expect(true.B)
        }.join()
      }
    }
  }
}
