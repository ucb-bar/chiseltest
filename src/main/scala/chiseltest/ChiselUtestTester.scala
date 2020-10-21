// SPDX-License-Identifier: Apache-2.0

package chiseltest

import chisel3.MultiIOModule
import chisel3.stage.ChiselGeneratorAnnotation
import chiseltest.experimental.sanitizeFileName
import chiseltest.stage.{ChiselTestStage, TestFunctionAnnotation, TestNameAnnotation}
import firrtl.AnnotationSeq
import utest.TestSuite
import utest.framework.Formatter

/**
  * Using utest as test framework
  * {{{
  *   // define test spec in trait
  *   trait HasTestChipSpec {
  *     import chisel3.tester._
  *     def testChipSpec(dut: TestChip): Unit = { c =>
  *       // body of the unit test, c is a reference
  *       c.io.input.poke(1.U)
  *       c.io.output.expect(2.U)
  *     }
  *   }
  *
  *   object SomeCircuitSpecTester extends ChiselUtestTester with HasSomeCircuitSpec {
  *     // define test by Tests macro
  *     val tests: Tests = Tests {
  *       // invoke test with test(""){}
  *       test("comments or name to a test"){
  *         // test function runs here
  *         testCircuit(new SomeCircuit, Seq(WriteVcdAnnotation))(SomeCircuitSpec)
  *       }
  *     }
  *   }
  * }}}
  */
trait ChiselUtestTester extends TestSuite {
  override def utestFormatter: Formatter = new Formatter {
    override def exceptionStackFrameHighlighter(s: StackTraceElement): Boolean = {
      Set("chisel3.", "scala.", "java.").map(!s.getClassName.startsWith(_)).reduce(_ && _)
    }
  }

  /**
    * Since [[utest.test]] collides with [[chisel3.tester.RawTester.test]], it is renamed to [[testCircuit]],
    * Here is a example to constructs a unit test harness for the Chisel Module PlusOne generated as dutGen.
    * {{{
    *   testCircuit(new PlusOne) { c =>
    *     // body of the unit test, c is a a reference
    *     c.io.input.poke(1.U)
    *     c.io.output.expect(2.U)
    *   }
    * }}}
    *
    * If you need to add your own [[AnnotationSeq]] to this test, you can add it as second parameter.
    *
    * For example:
    * {{{
    *   // WriteVcdAnnotation will ask backend export VCD file
    *   testCircuit(new PlusOne, Seq(WriteVcdAnnotation)) { c =>
    *     // body of the unit test, c is a a reference
    *     c.io.input.poke(1.U)
    *     c.io.output.expect(2.U)
    *   }
    * }}}
    *
    * @note This API is experimental and forward compatibility is not yet guaranteed
    * @param dutGen A generator of a Chisel Module
    * @tparam T The DUT type, must be a subclass of MultiIOModule
    */
  def testCircuit[T <: MultiIOModule](
    dutGen:        => T,
    annotationSeq: AnnotationSeq = Seq.empty
  )(testFn:        T => Unit
  )(
    implicit testPath: utest.framework.TestPath
  ): Unit = {
    (new ChiselTestStage).run(
      Seq(
        TestNameAnnotation(
          implicitly[utest.framework.TestPath].value.map(sanitizeFileName).mkString(java.io.File.separator)
        ),
        TestFunctionAnnotation(testFn),
        new ChiselGeneratorAnnotation(() => dutGen)
      ) ++ annotationSeq
    )
  }
}
