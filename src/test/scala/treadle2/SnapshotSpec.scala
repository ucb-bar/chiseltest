// SPDX-License-Identifier: Apache-2.0

package treadle2

import firrtl.stage.FirrtlSourceAnnotation
import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class SnapshotSpec extends AnyFreeSpec with Matchers with LazyLogging {
  "Snapshots can be created" in {
    val input =
      """
        |circuit SnapShotExample :
        |  module SnapShotExample :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input in0 : UInt<16>
        |    output out0 : UInt<16>
        |    output out1 : UInt<44>
        |    output out2 : UInt<128>
        |    output out3 : UInt<128>
        |
        |    reg r1 : UInt<16>, clock
        |    reg r2 : UInt<44>, clock
        |    reg r3 : UInt<128>, clock
        |
        |    r1 <= in0
        |    r2 <= r1
        |    r3 <= r2
        |
        |    out0 <= in0
        |    out1 <= r1
        |    out2 <= r2
        |    out3 <= r3
        |
      """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { t =>
      t.poke("in0", 1)
      t.step()
      t.poke("in0", 2)
      t.step()
      t.poke("in0", 3)

      val snapshot0 = t.engine.dataStore.serialize

      t.step()
      t.poke("in0", 4)
      t.step()
      t.poke("in0", 5)
      t.step()

      val snapshot1 = t.engine.dataStore.serialize

      logger.debug(s"snapshot0\n$snapshot0")
      logger.debug(s"snapshot1\n$snapshot1")

      logger.debug(s"snapshot0\n$snapshot0")
      logger.debug(s"snapshot1\n$snapshot1")

      t.engine.dataStore.deserialize(snapshot0)

      val snapshot2 = t.engine.dataStore.serialize

      snapshot2 should be(snapshot0)

      logger.debug(s"snapshot2\n$snapshot2")
    }
  }
}
