// Copyright 2018-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.tests

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class TimescopeTests extends AnyFreeSpec with ChiselScalatestTester {
  "timescopes are no longer supported" in {
    assertThrows[NotImplementedError] {
      test(new PassthroughModule(UInt(8.W))) { c =>
        c.in.poke(0.U)
        c.out.expect(0.U)
        timescope {
          c.in.poke(1.U)
          c.out.expect(1.U)
          c.clock.step()
          c.out.expect(1.U)
        }
        c.out.expect(0.U)
        c.clock.step()
        c.out.expect(0.U)
      }
    }
  }
}
