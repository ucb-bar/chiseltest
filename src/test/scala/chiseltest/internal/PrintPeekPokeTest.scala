// SPDX-License-Identifier: Apache-2.0
package chiseltest.internal

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PrintPeekPokeTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "PrintPeekPoke"

  it should "print test interactions to stdout" in {
    val (_, out) = utils.CaptureStdout {
      test(new tests.PassthroughModule(UInt(8.W))).withAnnotations(Seq(PrintPeekPoke)) { c =>
        c.in.poke(123.U)
        c.out.expect(123.U)
        c.clock.step()
      }
    }

    val lines = out.split('\n')

    assert(lines.contains("reset <- 1"))
    assert(lines.contains("step 0 -> 1"))
    assert(lines.contains("in <- 123"))
    assert(lines.contains("out -> 123"))
    assert(lines.contains("finish()"))
  }

  it should "only print when the annotation is provided" in {
    val (_, out) = utils.CaptureStdout {
      test(new tests.PassthroughModule(UInt(8.W))) { c =>
        c.in.poke(123.U)
        c.out.expect(123.U)
        c.clock.step()
      }
    }

    assert(out.trim.isEmpty)
  }
}
