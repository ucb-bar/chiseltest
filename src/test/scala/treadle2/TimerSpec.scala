// SPDX-License-Identifier: Apache-2.0

package treadle2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import treadle2.chronometry.Timer

// scalastyle:off magic.number
class TimerSpec extends AnyFlatSpec with Matchers {
  behavior.of("timer")

  it should "count times" in {
    val tag = "test1"
    val timer = new Timer
    timer.clear()
    timer(tag) {
      Thread.sleep(3000)
    }
    timer.timingLog.size should be(1)
    timer.timingLog(tag).events should be(1)
    timer.timingLog(tag).nanoseconds should be > 2000000000L
  }
}
