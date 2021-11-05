// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends

import chiseltest.formal.{FailedBoundedCheckException, FormalBackendOption, FormalTag}
import firrtl.annotations._
import chiseltest.utils.FlatSpecWithTargetDir
import chiseltest.simulator.{Compiler, WriteVcdAnnotation}
import firrtl._

class FirrtlBmcTests extends FlatSpecWithTargetDir {
  behavior of "maltese bmc"

  private def DefaultBackend: FormalEngineAnnotation = FormalBackendOption.getEngine(configMap)
  private val mRef = CircuitTarget("test").module("test")

  private def failAfter(n: Int): String =
    s"""circuit test:
       |  module test:
       |    input clock : Clock
       |    input reset : AsyncReset
       |
       |    reg count : UInt<32>, clock with : (reset => (reset, UInt(0)))
       |    count <= add(count, UInt(1))
       |    assert(clock, lt(count, UInt($n)), UInt(1), "") : leq_assert
       |""".stripMargin

  private val PresetAnno = PresetAnnotation(mRef.ref("reset"))

  private def loadFirrtl(src: String, annos: AnnotationSeq): CircuitState = {
    val state = CircuitState(firrtl.Parser.parse(src), annos)
    Compiler.toLowFirrtl(state)
  }
  private def loadFirrtl(src: String): CircuitState =
    loadFirrtl(src, withTargetDir(Seq(PresetAnno)))

  private val VCD = WriteVcdAnnotation

  it should "succeed for a limited amount of cycles" taggedAs FormalTag in {
    val state = loadFirrtl(failAfter(2))
    Maltese.bmc(state.circuit, DefaultBackend +: state.annotations, kMax = 1)
  }

  it should "fail after the appropriate amount of cycles" taggedAs FormalTag in {
    Seq(1,2,3,10,20,100).foreach { ii =>
      val state = loadFirrtl(failAfter(ii))

      val e = intercept[FailedBoundedCheckException] {
        Maltese.bmc(state.circuit, VCD +: DefaultBackend +: state.annotations, kMax = 101)
      }
      assert(e.failAt == ii)
    }
  }

  private def nestedSrc(n: Int): String =
    s"""circuit test:
       |  module child:
       |    input clock : Clock
       |    input reset : AsyncReset
       |    output count : UInt<32>
       |    reg count_reg : UInt<32>, clock with : (reset => (reset, UInt(0)))
       |    count_reg <= add(count_reg, UInt(1))
       |    count <= count_reg
       |
       |  module test:
       |    input clock : Clock
       |    input reset : AsyncReset
       |
       |    inst c of child
       |    c.clock <= clock
       |    c.reset <= reset
       |
       |    assert(clock, lt(c.count, UInt($n)), UInt(1), "") : leq_assert
       |""".stripMargin

  it should "work on a circuit with a submodule" taggedAs FormalTag in {
    val state = loadFirrtl(nestedSrc(5))

    // this should not fail
    Maltese.bmc(state.circuit, DefaultBackend +: state.annotations, kMax = 4)

    // this should fail
    val e = intercept[FailedBoundedCheckException] {
      Maltese.bmc(state.circuit, DefaultBackend +: state.annotations, kMax = 6)
    }
    assert(e.failAt == 5)

    // this should fail and produce a VCD
    val vcdFile = targetDir / "test.vcd"
    if(os.exists(vcdFile)) { os.remove(vcdFile) }
    assertThrows[FailedBoundedCheckException] {
      Maltese.bmc(state.circuit, VCD +: DefaultBackend +: state.annotations, kMax = 6)
    }
    assert(os.exists(vcdFile))
  }
}
