// SPDX-License-Identifier: Apache-2.0

//
//package treadle
//
//import firrtl.stage.FirrtlSourceAnnotation
//import org.scalatest.{FreeSpec, Matchers}
//
//
////scalastyle:off magic.number
//class ValidIfSpec extends FreeSpec with Matchers {
//  "ValidIf should return expression when not random" in {
//    val input =
//      """
//        |circuit ValidIfExample :
//        |  module ValidIfExample :
//        |    input in1    : UInt<16>
//        |    input enable : UInt<1>
//        |    output out1  : UInt<16>
//        |    out1 <= validif(enable, in1)
//      """.stripMargin
//
//    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input)))
//    tester.poke("in1", 42)
//    tester.expect("out1", 42)
//  }
//  "ValidIf should not return expression when not set to random" in {
//    val input =
//      """
//        |circuit ValidIfExample :
//        |  module ValidIfExample :
//        |    input in1    : UInt<16>
//        |    input enable : UInt<1>
//        |    output out1  : UInt<16>
//        |    out1 <= validif(enable, in1)
//      """.stripMargin
//
//    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), ValidIfIsRandomAnnotation))
//    tester.poke("in1", 42)
//    assert(tester.peek("out1") != 42)
//  }
//}
