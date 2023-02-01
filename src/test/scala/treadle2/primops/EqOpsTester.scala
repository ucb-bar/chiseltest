// SPDX-License-Identifier: Apache-2.0

package treadle2.primops

import firrtl.ir
import firrtl.ir.Type
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2._

class BlackBoxTypeParam_1(val name: String) extends ScalaBlackBox {
  var returnValue: BigInt = 0

  override def getOutput(inputValues: Seq[BigInt], tpe: Type, outputName: String): BigInt = {
    returnValue
  }

  override def setParams(params: Seq[ir.Param]): Unit = {
    val p1 = params.find(_.name == "T")
    p1.foreach {
      case valueParam: firrtl.ir.RawStringParam =>
        returnValue = BigInt("deadbeef", 16)
      case _ =>
        throw new Exception("huh? should not get here")
    }
  }

  override def outputDependencies(outputName: String): Seq[String] = Seq()
}

// scalastyle:off magic.number
class EqOpsTester extends AnyFreeSpec with Matchers {
  private val factory = new ScalaBlackBoxFactory {
    override def createInstance(instanceName: String, blackBoxName: String): Option[ScalaBlackBox] = {
      blackBoxName match {
        case "BlackBoxTypeParam" => Some(add(new BlackBoxTypeParam_1(instanceName)))
      }
    }
  }
  "EqOpsTester should pass a basic test" in {
    val input =
      """
        |circuit EqTester :
        |  extmodule BlackBoxTypeParam_1 :
        |    output out : UInt<32>
        |
        |    defname = BlackBoxTypeParam
        |    parameter T = 'bit [31:0]'
        |
        |  module EqTester :
        |    output out : UInt<1>
        |
        |    inst blackBoxTypeParamWord of BlackBoxTypeParam_1
        |
        |    out <= eq(blackBoxTypeParamWord.out, UInt<32>("hdeadbeef"))
      """.stripMargin

    val options = Seq(
      CallResetAtStartupAnnotation,
      BlackBoxFactoriesAnnotation(Seq(factory))
    )

    TreadleTestHarness(FirrtlSourceAnnotation(input) +: options) { tester =>
      tester.peek("out") should be(1)
    }
  }

  "results of equals on large numbers should have widths all work" in {
    val input =
      """
        |circuit CatProblem :
        |  module CatProblem :
        |    input clock : Clock
        |    output out : UInt<1>
        |
        |    node _T_310 = cat(UInt<32>("hffdff06f"), UInt<32>("h73")) @[Cat.scala 30:58]
        |    node _T_311 = cat(UInt<32>("h0"), UInt<32>("habcdef")) @[Cat.scala 30:58]
        |    node _T_312 = eq(_T_311, _T_310) @[Cat.scala 30:58]
        |
        |    out <= _T_312
        |
          """.stripMargin

    TreadleTestHarness(Seq(FirrtlSourceAnnotation(input))) { tester =>
      tester.peek("out") should be(BigInt(0))
    }
  }
}
