// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Created by chick on 5/4/16.
  */
//scalastyle:off magic.number
class DynamicMemorySearch extends AnyFreeSpec with Matchers with LazyLogging {
  "dynamic memory search should run with correct results" in {
    val input =
      """
        |circuit DynamicMemorySearch :
        |  module DynamicMemorySearch :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    output io : {flip isWr : UInt<1>, flip wrAddr : UInt<3>, flip data : UInt<6>, flip en : UInt<1>, target : UInt<3>, done : UInt<1>}
        |
        |    io is invalid
        |    reg index : UInt<3>, clk with : (reset => (reset, UInt<3>("h00")))
        |    cmem list : UInt<6>[8]
        |    infer mport memVal = list[index], clk
        |    node T_10 = eq(io.en, UInt<1>("h00"))
        |    node T_11 = eq(memVal, io.data)
        |    node T_13 = eq(index, UInt<3>("h07"))
        |    node T_14 = or(T_11, T_13)
        |    node over = and(T_10, T_14)
        |    when io.isWr :
        |      infer mport T_15 = list[io.wrAddr], clk
        |      T_15 <= io.data
        |      skip
        |    node T_17 = eq(io.isWr, UInt<1>("h00"))
        |    node T_18 = and(T_17, io.en)
        |    when T_18 :
        |      index <= UInt<1>("h00")
        |      skip
        |    node T_21 = eq(over, UInt<1>("h00"))
        |    node T_23 = eq(io.isWr, UInt<1>("h00"))
        |    node T_25 = eq(io.en, UInt<1>("h00"))
        |    node T_26 = and(T_23, T_25)
        |    node T_27 = and(T_26, T_21)
        |    when T_27 :
        |      node T_29 = add(index, UInt<1>("h01"))
        |      node T_30 = tail(T_29, 1)
        |      index <= T_30
        |      skip
        |    io.done <= over
        |    io.target <= index
    """.stripMargin

    // these numbers are hard-code into above firrtl, do not change with out fixing
    val n = 8
    val w = 4

    val showLocalDebug = false

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      val list: Array[Int] = Array.fill(n)(0)
      random.setSeed(0L)

      def startSearch(searchValue: BigInt): Unit = {
        tester.poke("io_isWr", 0)
        tester.poke("io_data", searchValue)
        tester.poke("io_en", 1)
        tester.step()
        tester.poke("io_en", 0)
      }

      def checkSearch(searchValue: Int): Unit = {
        val expectedIndex = list.indexOf(searchValue) match {
          case -1 => list.length
          case x  => x
        }

        var waitCount = 0
        while (waitCount <= n && tester.peek("io_done") == Big0) {
          // logger.debug(s"Waiting for done $waitCount")
          // logger.debug(this.engine.circuitState.prettyString())
          // logger.debug(s"external list ${list.mkString(",")}")
          tester.step()
          waitCount += 1
        }

        // logger.debug(s"Done wait count is $waitCount done is ${tester.peek("io_done")} " +
        //   s"got ${tester.peek("io_target")} got $expectedIndex")
        tester.expect("io_done", 1)
        tester.expect("io_target", expectedIndex)
      }

      // initialize memory
      for (write_address <- 0 until n) {
        tester.poke("io_en", 1)
        tester.poke("io_isWr", 1)
        tester.poke("io_wrAddr", write_address)
        tester.poke("io_data", write_address)
        list(write_address) = write_address
        tester.step()
        logger.debug(
          s"Initializing memory address $write_address with $write_address mem($write_address)" +
            s" is ${tester.peekMemory("list", write_address)}"
        )
      }

      if (showLocalDebug) {
        logger.debug(s"Memory init done XXXXXXXXXX")
        (0 until n).foreach { i =>
          val mem = tester.peekMemory("list", i)
          logger.debug(s"$i : $mem ${list(i)}")
        }
      }

      startSearch(4)
      checkSearch(4)

      for (k <- 0 until 160) {
        logger.debug(s"memory test iteration $k") // ${"X"*80}")

        // Compute a random address and value
        val wrAddr = random.nextInt(n - 1)
        val data = random.nextInt((1 << w) - 1)
        if (showLocalDebug) {
          logger.debug(s"setting memory($wrAddr) = $data")
        }

        // tester.poke it into memory
        tester.poke("io_en", 0)
        tester.poke("io_isWr", 1)
        tester.poke("io_wrAddr", wrAddr)
        tester.poke("io_data", data)
        list(wrAddr) = data
        tester.step()

        if (showLocalDebug) {
          logger.debug(s"current memory ")
          logger.debug((0 until n).map { i =>
            s"$i:${tester.peekMemory("list", i)}:${list(i)}"
          }.mkString(", "))
          logger.debug("")
        }

        // SETUP SEARCH
        val target = if (k > 12) random.nextInt(1 << w) else data
        tester.poke("io_isWr", 0)
        tester.poke("io_data", target)
        tester.poke("io_en", 1)
        tester.step()
        tester.poke("io_en", 0)
        val expectedIndex = if (list.contains(target)) {
          list.indexOf(target)
        } else {
          list.length - 1
        }

        var waitCount = 0
        while (waitCount <= n && tester.peek("io_done") == Big0) {
          // logger.debug(s"Waiting for done $waitCount")
          // logger.debug(this.engine.circuitState.prettyString())
          // logger.debug(s"external list ${list.mkString(",")}")
          tester.step()
          waitCount += 1
        }

        if (showLocalDebug) {
          logger.debug(
            s"Done wait count is $waitCount done is ${tester.peek("io_done")} " +
              s"got ${tester.peek("io_target")} got $expectedIndex"
          )
        }
        tester.expect("io_done", 1)
        tester.expect("io_target", expectedIndex)
        tester.step()
      }
    }
  }
}
