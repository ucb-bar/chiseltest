// See LICENSE for license details.

package chiseltest.tests

import utest._
import chiseltest._
import chisel3._

object UtestTesterTest extends ChiselUtestTester {
  val tests: Tests = Tests {
    "This tester does not rely on scalatest to run" - {
      testCircuit(new StaticModule(42.U)) { c =>
        c.out.expect(42.U)
      }
    }
  }
}
