// SPDX-License-Identifier: Apache-2.0

package chiseltest.iotesters.examples

import scala.annotation.tailrec

object GCDCalculator {
  @tailrec
  def computeGcdResultsAndCycles(a: Int, b: Int, depth: Int = 1): (Int, Int) = {
    if(b == 0) {
      (a, depth)
    }
    else {
      computeGcdResultsAndCycles(b, a%b, depth+1 )
    }
  }
}
