// See LICENSE for license details.

package chisel3.tester.experimental

import chisel3._
import chisel3.experimental.MultiIOModule
import chisel3.tester._

package object TestOptionBuilder {
  implicit class ChiselScalatestOptionBuilder[T <: MultiIOModule](x: ChiselScalatestTester#TestBuilder[T]) {
    def withExecOptions(opt: firrtl.ExecutionOptionsManager): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, Some(opt), x.testOptions)
    }

    def withTesterOptions(opt: TesterOptions): ChiselScalatestTester#TestBuilder[T] = {
      new x.outer.TestBuilder[T](x.dutGen, x.execOptions, Some(opt))
    }
  }
}
