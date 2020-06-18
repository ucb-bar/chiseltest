// See LICENSE for license details.

package chiseltest.tests

import utest._
import chiseltest._
import chisel3._
import firrtl.options.TargetDirAnnotation

object UtestTesterTest extends ChiselUtestTester {
  val tests: Tests = Tests {
    "Utest should work" - {
      testCircuit(new StaticModule(42.U),
        Seq(TargetDirAnnotation("test_run_dir" + java.io.File.separator + implicitly[utest.framework.TestPath].value.map(sanitizeFileName).mkString(java.io.File.separator)))
      ) { c =>
        c.out.expect(42.U)
      }
    }
  }
}
