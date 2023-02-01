// SPDX-License-Identifier: Apache-2.0

package treadle

import firrtl.stage.FirrtlSourceAnnotation
import treadle.chronometry.Timer

//scalastyle:off magic.number

object Regression {
  def computeGcd(a: Int, b: Int): (Int, Int) = {
    var x = a
    var y = b
    var depth = 1
    while (y > 0) {
      if (x > y) {
        x -= y
      } else {
        y -= x
      }
      depth += 1
    }
    (x, depth)
  }

  //scalastyle:off method.length
  def manyValuesTest(width: Int): Unit = {
    val gcdFirrtl: String =
      s"""
         |circuit GCD :
         |  module GCD :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_a : UInt<$width>
         |    input io_b : UInt<$width>
         |    input io_e : UInt<1>
         |    output io_z : UInt<$width>
         |    output io_v : UInt<1>
         |    reg x : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), x)
         |    reg y : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), y)
         |    node T_13 = gt(x, y)
         |    node T_14 = sub(x, y)
         |    node T_15 = tail(T_14, 1)
         |    node T_17 = eq(T_13, UInt<1>("h0"))
         |    node T_18 = sub(y, x)
         |    node T_19 = tail(T_18, 1)
         |    node T_21 = eq(y, UInt<1>("h0"))
         |    node GEN_0 = mux(T_13, T_15, x)
         |    x <= mux(io_e, io_a, GEN_0)
         |    node GEN_1 = mux(T_17, T_19, y)
         |    y <= mux(io_e, io_b, GEN_1)
         |    io_z <= x
         |    io_v <= T_21
    """.stripMargin

    val values =
      for {
        x <- 1 to 1000
        y <- 1 to 100
      } yield (x, y, computeGcd(x, y)._1)

    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(gcdFirrtl)))

    val startTime = System.nanoTime()
    tester.poke("clock", 1)

    for ((x, y, z) <- values) {
      tester.step()
      tester.poke("io_a", x)
      tester.poke("io_b", y)
      tester.poke("io_e", 1)
      tester.step()

      tester.poke("io_e", 0)
      tester.step()

      while (tester.peek("io_v") != Big1) {
        tester.step()
      }

      tester.expect("io_z", z)
    }
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    val cycle = tester.cycleCount

    println(
      f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
    )
    tester.report()
  }

  def main(args: Array[String]): Unit = {
    manyValuesTest(20)
  }

}

/** This regression demonstrates that a bad initial setting has been fixed
  * the default rollback buffer of 10 meant that large memories caused considerable slowing
  * as buffers were copied.
  */
object MemoryUsageRegression {

  /** Run a test by writing testSize numbers into a memory of memorySize, then
    * reads the numbers back out again. With a fixed testSize the run times should
    * be the same despite the size of the memory
    * @param memorySize number of elements in memory
    * @param testSize   number of values to write and read from memory
    */
  def memoryRegression(memorySize: Int, testSize: Int): Unit = {

    val elementSize = 30

    val input =
      s"""
         |circuit target_memory :
         |  module target_memory :
         |    input clock      : Clock
         |    input index      : UInt<$elementSize>
         |    input do_write   : UInt<1>
         |    input do_enable  : UInt<1>
         |    input write_data : UInt<$elementSize>
         |    output read_data : UInt<$elementSize>
         |
         |    mem ram :
         |      data-type => UInt<$elementSize>
         |      depth => $memorySize
         |      read-latency => 1
         |      write-latency => 1
         |      readwriter => RW_0
         |      read-under-write => undefined
         |
         |    ram.RW_0.clk <= clock
         |    ram.RW_0.addr <= index
         |    ram.RW_0.en <= UInt<1>("h1")
         |
         |    ram.RW_0.wmode <= do_write
         |    read_data <= ram.RW_0.rdata
         |    ram.RW_0.wdata <= write_data
         |    ram.RW_0.wmask <= UInt<1>("h1")
      """.stripMargin

    val timer = new Timer

    val tester = timer("tester assembly")(TreadleTester(Seq(FirrtlSourceAnnotation(input))))

    for (trial <- 0 until 4) {
      timer(s"trial_${trial}_$testSize") {
        tester.poke("do_write", 1)

        for (i <- 0 until testSize) {
          tester.poke("index", i)
          tester.poke("write_data", i + 3)
          tester.step()
        }
        tester.poke("do_write", 0)
        tester.step(2)

        for (i <- 0 until testSize) {
          tester.poke("index", i)
          tester.step()
          tester.expect("read_data", i + 3)
        }
      }
    }
    println(timer.report())
    tester.report()
  }

  def main(args: Array[String]): Unit = {
    for (power <- 20 to 32) {
      val memorySize = 1 << power
      println(s"Memory size $memorySize")
      memoryRegression(memorySize, 10000)
    }
  }
}
