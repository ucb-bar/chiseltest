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
    println("JniBridgeSpec start")
    val soPath = os.pwd / "src" / "main" / "resources" / "jni" / "VFoo"
    println((os.pwd / "test_run_dir").toString())
    val bridgeLib = new JniAPI(os.pwd / "test_run_dir")
    println("bridgeLib instantiated")

    val vfoo_id = bridgeLib.load_so(soPath.toString())
    val vfoo_id2 = bridgeLib.load_so(soPath.toString())

    val ptr = bridgeLib.call_sim_init(vfoo_id)
    val ptr2 = bridgeLib.call_sim_init(vfoo_id2)

    bridgeLib.call_poke(vfoo_id, ptr, 1, 10)
    bridgeLib.call_poke(vfoo_id2, ptr2, 1, 20)
    bridgeLib.call_update(vfoo_id, ptr)
    val output = bridgeLib.call_peek(vfoo_id, ptr, 1)
    val output2 = bridgeLib.call_peek(vfoo_id2, ptr2, 1)

    println(output)
    println(output2)
    assert(output == 10)
    assert(output2 == 20)

    bridgeLib.call_poke(vfoo_id, ptr, 1, 30)
    bridgeLib.call_poke(vfoo_id2, ptr2, 1, 40)

    val output_1 = bridgeLib.call_peek(vfoo_id, ptr, 1)
    val output2_1 = bridgeLib.call_peek(vfoo_id2, ptr2, 1)

    println(output_1)
    println(output2_1)
    assert(output_1 == 30)
    assert(output2_1 == 40)

    bridgeLib.call_finish(vfoo_id, ptr)
    bridgeLib.call_finish(vfoo_id2, ptr)
  }
}
