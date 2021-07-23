// SPDX-License-Identifier: Apache-2.0
package chiseltest.backends.verilator

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.experimental.sanitizeFileName
import chiseltest.internal.{CachingAnnotation, VerilatorBackendAnnotation}
import chiseltest.tests.StaticModule
import firrtl.AnnotationSeq
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VerilatorCachingTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2 with Verilator and caching"

  val default = Seq(VerilatorBackendAnnotation)
  val withCaching = Seq(VerilatorBackendAnnotation, CachingAnnotation)
  private val cachingMin = 0.7    // at least 70% difference between first (uncached) and subsequent runs (cached)
  private val nonCachingMax = 0.4 // max 40% difference between runs

  private def runTest(num: Int, annos: AnnotationSeq): Long = {
    time(test(new StaticModule(num.U)).withAnnotations(annos) { c =>
      c.out.expect(num.U)
    })._2
  }

  it should "re-compile by default" in {
    // warmup to improve measurement accuracy
    (0 until 2).foreach { _ => runTest(42, default) }
    startWithEmptyTestDir()
    // here we test out hypothesis that without caching, the tet execution times will all be about the same
    val times = (0 until 3).map { _ => runTest(42, default) }
    val delta = avgRelativeDelta(times)
    assert(delta < nonCachingMax)
    println(times)
    println(delta)
  }

  it should "not re-compile when caching is enabled" in {
    // warmup to improve measurement accuracy
    (0 until 2).foreach { _ => runTest(42, default) }
    startWithEmptyTestDir()
    // with caching, the first time we execute the test should be significantly slower
    val times = (0 until 3).map { _ => runTest(42, withCaching) }
    val delta = avgRelativeDelta(times)
    assert(delta > cachingMin)
    println(times)
    println(delta)
  }

  it should "not cache when two different modules are instantiated" in {
    startWithEmptyTestDir()
    // this generates a cache
    val first = time(test(new StaticModule(42.U)).withAnnotations(withCaching) { c =>
      c.out.expect(42.U)
    })._2

    // this call should invalidate the cache of the first module
    val second = time(test(new StaticModule(101.U)).withAnnotations(withCaching) { c =>
      c.out.expect(101.U)
    })._2

    // thus only the second and third call here should ever be cached
    val others = (0 until 3).map { _ =>
      time(test(new StaticModule(42.U)).withAnnotations(withCaching) { c =>
        c.out.expect(42.U)
      })._2
    }

    val unCached = Seq(first, second, others.head)
    val cached = Seq(first) ++ others.tail
    assert(avgRelativeDelta(unCached) < nonCachingMax)
    assert(avgRelativeDelta(cached) > cachingMin)
  }

  private def time[T](f: => T): (T, Long) = {
    val start = System.nanoTime()
    val r = f
    val delta = System.nanoTime() - start
    (r, delta)
  }

  private def avgRelativeDelta(ts: Seq[Long]): Double = {
    require(ts.length >= 2)
    val first = ts.head
    val diffs = ts.tail.map(_ - first).map(math.abs)
    val rel = diffs.map(_.toDouble / first.toDouble)
    rel.sum / rel.size
  }

  private def startWithEmptyTestDir(): Unit = {
    val testName = sanitizeFileName(scalaTestContext.value.get.name)
    val testDir = os.pwd / "test_run_dir" / testName
    if(os.exists(testDir)) { os.remove.all(testDir) }
  }
}

