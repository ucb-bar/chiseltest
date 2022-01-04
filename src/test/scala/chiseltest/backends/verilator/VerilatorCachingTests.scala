// SPDX-License-Identifier: Apache-2.0
package chiseltest.backends.verilator

import chisel3._
import chiseltest._
import chiseltest.experimental.sanitizeFileName
import chiseltest.internal.CachingAnnotation
import chiseltest.simulator.{CachingDebugAnnotation, RequiresVerilator}
import chiseltest.utils.CaptureStdout
import chiseltest.tests.StaticModule
import firrtl.AnnotationSeq
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VerilatorCachingTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2 with Verilator and caching"

  val default = Seq(VerilatorBackendAnnotation)
  val withCaching = Seq(VerilatorBackendAnnotation, CachingAnnotation, CachingDebugAnnotation)

  private def runTest(num: Int, annos: AnnotationSeq): Int = {
    countReuse(test(new StaticModule(num.U)).withAnnotations(annos) { c =>
      c.out.expect(num.U)
    })._2
  }

  it should "re-compile by default" taggedAs RequiresVerilator in {
    startWithEmptyTestDir()
    // by default no reuse should occur
    (0 until 3).foreach { _ => assert(runTest(42, default) == 0) }
  }

  it should "not re-compile when caching is enabled" taggedAs RequiresVerilator in {
    startWithEmptyTestDir()
    // the first time no re-use occurs
    assert(runTest(42, withCaching) == 0)
    // after that the test should be reuse
    assert(runTest(42, withCaching) == 1)
    assert(runTest(42, withCaching) == 1)
  }

  it should "not cache when two different modules are instantiated" taggedAs RequiresVerilator in {
    startWithEmptyTestDir()
    // this generates a cache
    println("StaticModule(42)")
    assert(runTest(42, withCaching) == 0)

    // this call should invalidate the cache of the first module
    println("StaticModule(101)")
    assert(runTest(101, withCaching) == 0)

    // thus only the second and third call here should ever be cached
    println("StaticModule(42)")
    assert(runTest(42, withCaching) == 0)
    println("StaticModule(42)")
    assert(runTest(42, withCaching) == 1)
    println("StaticModule(42)")
    assert(runTest(42, withCaching) == 1)
  }

  /** counts how many lines containing "Re-using compiled simulation" appears on stdout while executing `f` */
  private def countReuse[T](f: => T): (T, Int) = {
    val (r, out) = CaptureStdout { f }
    print(out)
    val count = out.split('\n').count(_.contains("Re-using compiled simulation"))
    (r, count)
  }

  private def testDir: os.Path = os.pwd / "test_run_dir" / sanitizeFileName(scalaTestContext.value.get.name)

  private def startWithEmptyTestDir(): Unit = {
    if(os.exists(testDir)) { os.remove.all(testDir) }
  }
}

