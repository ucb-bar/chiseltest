// See LICENSE for license details.

package chisel3.tests

import org.scalatest.FreeSpec

class EnvTest extends FreeSpec {
  "show env" in {
    val ev = System.getenv()

    for (k <- ev.keySet().toArray.map(_.toString).sorted) println(s"key: $k, value: ${ev.get(k)}")
  }

}
