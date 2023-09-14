// SPDX-License-Identifier: Apache-2.0

package chiseltest

import chiseltest.internal._
import chisel3.Module
import chiseltest.iotesters.PeekPokeTester
import firrtl2.AnnotationSeq
import org.scalatest._
import org.scalatest.exceptions.TestFailedException

import scala.util.DynamicVariable

trait HasTestName { def getTestName: String }

trait ChiselScalatestTester extends Assertions with TestSuiteMixin with HasTestName {
  this: TestSuite =>

  override def getTestName: String = TesterUtils.sanitizeFileName(scalaTestContext.value.get.name)

  class TestBuilder[T <: Module](
    val dutGen:              () => T,
    val annotationSeq:       AnnotationSeq,
    val chiselAnnotationSeq: firrtl.AnnotationSeq) {
    def getTestName: String = {
      TesterUtils.sanitizeFileName(scalaTestContext.value.get.name)
    }

    def apply(testFn: T => Unit): TestResult = {
      runTest(dutGen, finalAnnos(annotationSeq), chiselAnnotationSeq, testFn)
    }

    private def finalAnnos(annos: AnnotationSeq): AnnotationSeq = {
      TesterUtils.addDefaultTargetDir(getTestName, annos) ++
        (if (scalaTestContext.value.get.configMap.contains("writeVcd")) {
           Seq(WriteVcdAnnotation)
         } else {
           Seq.empty
         })
    }

    // TODO: in the future, allow reset and re-use of a compiled design to avoid recompilation cost per test
    val outer: ChiselScalatestTester = ChiselScalatestTester.this

    def withAnnotations(annotationSeq: AnnotationSeq): TestBuilder[T] = {
      new TestBuilder[T](dutGen, this.annotationSeq ++ annotationSeq, this.chiselAnnotationSeq)
    }

    def withChiselAnnotations(chiselAnnotationSeq: firrtl.AnnotationSeq): TestBuilder[T] = {
      new TestBuilder[T](dutGen, this.annotationSeq, this.chiselAnnotationSeq ++ chiselAnnotationSeq)
    }

    /** Resets and then executes the circuit until a timeout or a stop or assertion failure. Throws an exception if the
      * timeout is reached or an assertion failure is encountered.
      * @param timeout
      *   number of cycles after which to timeout; set to 0 for no timeout
      */
    def runUntilStop(timeout: Int = 1000): TestResult = {
      new TestResult(
        HardwareTesterBackend.run(dutGen, finalAnnos(annotationSeq), timeout = timeout, expectFail = false)
      )
    }

    /** Resets and then executes the circuit until a timeout or a stop or assertion failure. Throws an exception if the
      * timeout is reached or a normal stop is encountered.
      * @param timeout
      *   number of cycles after which to timeout; set to 0 for no timeout
      */
    def runUntilAssertFail(timeout: Int = 1000): TestResult = {
      new TestResult(HardwareTesterBackend.run(dutGen, finalAnnos(annotationSeq), timeout = timeout, expectFail = true))
    }

    /** Executes a tester extending [[chiseltest.iotesters.PeekPokeTester]]. */
    def runPeekPoke(tester: T => PeekPokeTester[T]): Unit = {
      new TestResult(PeekPokeTesterBackend.run(dutGen, tester, finalAnnos(annotationSeq), chiselAnnotationSeq))
    }
  }

  // Provide test fixture data as part of 'global' context during test runs
  protected var scalaTestContext = new DynamicVariable[Option[NoArgTest]](None)

  abstract override def withFixture(test: NoArgTest): Outcome = {
    require(scalaTestContext.value.isEmpty)
    scalaTestContext.withValue(Some(test)) {
      super.withFixture(test)
    }
  }

  private def runTest[T <: Module](
    dutGen:        () => T,
    annotationSeq: AnnotationSeq,
    chiselAnnos:   firrtl.AnnotationSeq,
    testFn:        T => Unit
  ): TestResult = {
    try {
      Context.runTest(dutGen, annotationSeq, chiselAnnos, testFn)
    } catch {
      // Translate testers2's FailedExpectException into ScalaTest TestFailedException that is more readable
      case exc: FailedExpectException =>
        val newExc = new TestFailedException(exc, exc.failedCodeStackDepth)
        newExc.setStackTrace(exc.getStackTrace)
        throw newExc
    }
  }

  /** Constructs a unit test harness for the Chisel Module generated by dutGen. General use looks like
    * {{{
    *   test(new PlusOne) { c =>
    *     // body of the unit test, c is a a reference
    *     c.io.input.poke(1.U)
    *     c.io.output.expect(2.U)
    *   }
    * }}}
    *
    * If you need to add options to this unit test you can specify them as a second argument or tack on .withAnnotations
    * modifier or a .withFlags modifier. These modifiers can be used together.
    *
    * For example:
    * {{{
    *   test(new TestModule, Seq(WriteVcdAnnotation)) { c =>
    *     // body of the unit test
    *   }
    *   // alternative:
    *   test(new TestModule).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
    *     // body of the unit test
    *   }
    * }}}
    * @see
    *   src/test/scala/chisel3/tests/OptionsBackwardCompatibilityTest for examples
    *
    * @note
    *   This API is experimental and forward compatibility is not yet guaranteed
    *
    * @param dutGen
    *   A generator of a Chisel Module
    * @tparam T
    *   The DUT type, must be a subclass of Module
    * @return
    */
  def test[T <: Module](dutGen: => T): TestBuilder[T] = {
    new TestBuilder(() => dutGen, Seq(), Seq())
  }
}
