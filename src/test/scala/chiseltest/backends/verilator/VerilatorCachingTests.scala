// SPDX-License-Identifier: Apache-2.0
package chiseltest.backends.verilator

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.experimental.sanitizeFileName
import chiseltest.internal.{CachingAnnotation, VerilatorBackendAnnotation}
import chiseltest.tests.StaticModule
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VerilatorCachingTests extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2 with Verilator and caching"

  val default = Seq(VerilatorBackendAnnotation)
  val withCaching = Seq(VerilatorBackendAnnotation, CachingAnnotation)
  private val cachingMin = 1.4
  private val nonCachingMax = 1.2

  it should "re-compile by default" in {
    startWithEmptyTestDir()
    // here we test out hypothesis that without caching, the tet execution times will all be about the same
    val times = (0 until 3).map { _ =>
      time(test(new StaticModule(42.U)).withAnnotations(default) { c =>
        c.out.expect(42.U)
      })._2
    }
    relativeTime(times).foreach(delta => assert(delta < nonCachingMax))
  }

  it should "not re-compile when caching is enabled" in {
    startWithEmptyTestDir()
    // with caching, the first time we execute the test should be significantly slower
    val times = (0 until 3).map { _ =>
      time(test(new StaticModule(42.U)).withAnnotations(withCaching) { c =>
        c.out.expect(42.U)
      })._2
    }
    relativeTime(times).foreach(delta => assert(delta > cachingMin))
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
    relativeTime(unCached).foreach(delta => assert(delta < nonCachingMax))
    relativeTime(cached).foreach(delta => assert(delta > cachingMin))
  }

  private def time[T](f: => T): (T, Long) = {
    val start = System.nanoTime()
    val r = f
    val delta = System.nanoTime() - start
    (r, delta)
  }

  private def relativeTime(ts: Seq[Long]): Seq[Double] = {
    require(ts.length >= 2)
    val first = ts.head
    val diffs = ts.tail.map(_ - first).map(math.abs)
    val average = (ts.sum / ts.length).toDouble
    diffs.map(_.toDouble / average)
  }

  private def startWithEmptyTestDir(): Unit = {
    val testName = sanitizeFileName(scalaTestContext.value.get.name)
    val testDir = os.pwd / "test_run_dir" / testName
    if(os.exists(testDir)) { os.remove.all(testDir) }
  }
}

