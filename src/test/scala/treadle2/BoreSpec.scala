// SPDX-License-Identifier: Apache-2.0

package treadle2


import firrtl.AnnotationSeq
import firrtl.annotations._
import firrtl.options.TargetDirAnnotation
import firrtl.passes.wiring.{SinkAnnotation, SourceAnnotation}
import firrtl.stage.{FirrtlSourceAnnotation, FirrtlStage, RunFirrtlTransformAnnotation}
import firrtl.transforms.{DontTouchAnnotation, NoDedupAnnotation}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BoreSpec extends AnyFreeSpec with Matchers {
  "WiringTransform should be honor" in {
    val input =
      """
        |circuit BoreTestTop :
        |  module BoreTestConstant :
        |    input clock : Clock
        |    input reset : Reset
        |
        |    wire x : UInt<6>
        |    x <= UInt<6>("h02a")
        |
        |  module BoreTestExpect :
        |    input clock : Clock
        |    input reset : Reset
        |    output y : UInt<6>
        |
        |    y <= UInt<1>("h00")
        |
        |  module BoreTestTop :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output y : UInt<6>
        |
        |    inst constant of BoreTestConstant @[BoreTest.scala 26:26]
        |    constant.clock <= clock
        |    constant.reset <= reset
        |    inst expect of BoreTestExpect @[BoreTest.scala 27:24]
        |    expect.clock <= clock
        |    expect.reset <= reset
        |    y <= expect.y @[BoreTest.scala 28:7]
        |""".stripMargin

    var annos: AnnotationSeq = Seq(
      FirrtlSourceAnnotation(input),
      TargetDirAnnotation("test_run_dir/BoreSpec"),
      SourceAnnotation(ComponentName("x", ModuleName("BoreTestConstant", CircuitName("BoreTestTop"))), "x"),
      DontTouchAnnotation(ReferenceTarget("BoreTestTop", "BoreTestConstant", List(), "x", List())),
      NoDedupAnnotation(ModuleName("BoreTestConstant", CircuitName("BoreTestTop"))),
      SinkAnnotation(ComponentName("y", ModuleName("BoreTestExpect", CircuitName("BoreTestTop"))), "x"),
      NoDedupAnnotation(ModuleName("BoreTestExpect", CircuitName("BoreTestTop"))),
      RunFirrtlTransformAnnotation(new firrtl.passes.wiring.WiringTransform)
    )

    annos = (new FirrtlStage).transform(annos)
    TreadleTestHarness(annos) { tester =>
      tester.expect("y", 42)
    }
  }
}
