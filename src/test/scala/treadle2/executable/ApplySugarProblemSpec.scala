// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import logger.LazyLogging
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

//scalastyle:off magic.number
class ApplySugarProblemSpec extends AnyFreeSpec with Matchers with LazyLogging {
  type IntFunc = () => Int

  trait HasIntFunc {
    def apply: FuncInt
  }

  class Add(f1: IntFunc, f2: IntFunc, verbose: Boolean) extends HasIntFunc {
    val apply: FuncInt = if (verbose) { () =>
      {
        logger.debug("silent")
        f1() + f2()
      }
    } else { () =>
      {
        val (r1, r2) = (f1(), f2())
        logger.debug(s"add($r1, $r2) => ${r1 + r2}")
        r1 + r2
      }
    }
  }

  "should work" in {
    val add = new Add(() => 3, () => 5, false)
    logger.debug(s"start")
    add.apply() should be(8)
    logger.debug(s"after apply")
    val addv = new Add(() => 3, () => 5, true)
    addv.apply() should be(8)
    logger.debug(s"after apply")
  }
}
