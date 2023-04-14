// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jni

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


// object BridgeLib {
//   val soPath = os.pwd / "src" / "main" / "resources" / "jni" / "libjnibridge.so"
//   System.load(soPath.toString())
// }

class JniBridgeSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Testers2"

  it should "be able to load so through bridge library and call functions" in {
    //println("BridgeLib start")
    val soPath = os.pwd / "src" / "main" / "resources" / "jni" / "VFoo"

    val vfoo_id = JniAPI.load_so(soPath.toString())
    val vfoo_id2 = JniAPI.load_so(soPath.toString())

    val ptr = JniAPI.call_sim_init(vfoo_id)
    val ptr2 = JniAPI.call_sim_init(vfoo_id2)

    JniAPI.call_poke(vfoo_id, ptr, 1, 10)
    JniAPI.call_poke(vfoo_id2, ptr2, 1, 20)
    JniAPI.call_update(vfoo_id, ptr)
    val output = JniAPI.call_peek(vfoo_id, ptr, 1)
    val output2 = JniAPI.call_peek(vfoo_id2, ptr2, 1)

    println(output)
    println(output2)
    assert(output == 10)
    assert(output2 == 20)

    JniAPI.call_poke(vfoo_id, ptr, 1, 30)
    JniAPI.call_poke(vfoo_id2, ptr2, 1, 40)

    val output_1 = JniAPI.call_peek(vfoo_id, ptr, 1)
    val output2_1 = JniAPI.call_peek(vfoo_id2, ptr2, 1)

    println(output_1)
    println(output2_1)
    assert(output_1 == 30)
    assert(output2_1 == 40)

    JniAPI.call_finish(vfoo_id, ptr)
    JniAPI.call_finish(vfoo_id2, ptr)
  }
}
