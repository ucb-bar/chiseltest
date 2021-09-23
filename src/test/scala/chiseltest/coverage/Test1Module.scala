// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

// NOTE: This module is contained in its own file in order to prevent any line numbers from changing.
// It is used (among other things) for testing our automatic line coverage pass which
// will give us different results if things are moved around in the code.

import chisel3._

class Test1Module(withSubmodules: Boolean = false) extends Module {
  val a = IO(Input(UInt(3.W)))
  val b = IO(Output(UInt(3.W)))

  b := 0.U // line 5

  when(a === 4.U) {
    b := 1.U
  }

  when(5.U < a) {
    b := 2.U
  }.otherwise {
    b := 3.U
  }

  when(a === 0.U) {
    cover(true.B, "user coverage").suggestName("user_cov")
  }

  when(a === 1.U) {
    // empty
  }

  // name collision for the cover chain generator
  val cover_chain_en = RegInit(0.U(3.W))
  cover_chain_en := a

  if(withSubmodules) {
    val c0 = Module(new SubModule1).suggestName("c0")
    c0.a := a
    val c1 = Module(new SubModule1).suggestName("c1")
    c1.a := a - 4.U
  }
}

class SubModule1 extends Module {
  val a = IO(Input(UInt(3.W)))
  cover(a > 4.U, "user coverage 2").suggestName("user_cov_2")
}
