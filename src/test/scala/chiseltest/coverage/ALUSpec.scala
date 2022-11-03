// See README.md for license details.

package chiseltest.coverage

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly gcd.GcdDecoupledTester
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly gcd.GcdDecoupledTester'
  * }}}
  */
class ALUSpec extends AnyFreeSpec with ChiselScalatestTester {


  "xxx" in {
      test(new ALU) {c => 
      c.io.value1.poke(3.U)
      c.io.value2.poke(10.U)
      c.io.select.poke(true.B)
      c.clock.step(1)
      c.io.output.expect(13.U)
    }
    println("SUCCESS, when poke === true")


    test(new ALU).withAnnotations(Seq(LineCoveragePass)) {c => 
      c.io.value1.poke(10.U)
      c.io.value2.poke(3.U)
      c.io.select.poke(false.B)
      c.clock.step(1)
      c.io.output.expect(7.U)
    }

    println("SUCCESS, when poke === false")

    test(new ALU) {c => 
      c.io.value1.poke(10.U)
      c.io.value2.poke(3.U)
      c.io.select.poke(false.B)
      c.io.output.expect(0.U)
    }
    println("SUCCESS, when clk not tirgger")
  }

}
