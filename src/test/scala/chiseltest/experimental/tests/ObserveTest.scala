// SPDX-License-Identifier: Apache-2.0

package chiseltest.experimental.tests

import chiseltest.experimental.Observer

import chisel3._
import chiseltest._
import org.scalatest._

import flatspec._
import matchers.should._

class ObserverTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Observer"

  class SubModule extends Module {
    val reg  = Reg(UInt(6.W))
    val wire = Wire(UInt(6.W))
    val vec  = RegInit(VecInit(Seq.fill(32)(0.S(32.W))))
    reg     := 42.U
    wire    := reg
    vec(0)  := 1.S
    vec(10) := 10.S
    vec(20) := -25.S
    vec(31) := 150.S
  }

  class Top extends Module {
    val submodule = Module(new SubModule)
  }

  class TopWrapper extends Top with Observer {
    val observed_reg  = observe(submodule.reg)
    val observed_wire = observe(submodule.wire)
    val observed_vec  = observe(submodule.vec)
  }

  it should "observe a submodule Reg by using BoringUtils" in {
    test(new TopWrapper) { c =>
      c.observed_reg.expect(42.U)
    }
  }
  it should "observe a submodule Wire by using BoringUtils" in {
    test(new TopWrapper) { c =>
      c.observed_wire.expect(42.U)
    }
  }
  it should "observe a submodule Vector by using BoringUtils" in {
    test(new TopWrapper) { c =>
      c.clock.step()
      c.observed_vec(0).expect(1.S)
      c.observed_vec(10).expect(10.S)
      c.observed_vec(20).expect(-25.S)
      c.observed_vec(31).expect(150.S)
    }
  }
}
