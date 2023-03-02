// SPDX-License-Identifier: Apache-2.0

package treadle2.primops

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import treadle2.executable._
import treadle2.{extremaOfSIntOfWidth, extremaOfUIntOfWidth, BitTwiddlingUtils}

// scalastyle:off magic.number
class CvtNegNot extends AnyFreeSpec with Matchers {
  "Cvt, Neg and Not should pass a basic test" - {
    "Cvt is a no-op for the simulator" in {
      true should be(true)
    }

    "Neg should pass the following tests" - {
      def doNegCheck(num1: Big): Unit = {
        val got = (
          NegInts(() => num1.toInt).apply(),
          NegLongs(() => num1.toLong).apply(),
          NegBigs(() => num1).apply()
        )
        val expected = (
          BitTwiddlingUtils.neg(num1),
          BitTwiddlingUtils.neg(num1),
          BitTwiddlingUtils.neg(num1)
        )

        got should be(expected)
      }

      "bits should work on known range of SInts" in {
        for {
          bitWidth1 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfSIntOfWidth(bitWidth1)
          for {
            num1 <- lo1 to hi1
          } {
            doNegCheck(num1)
          }
        }
      }

      "bits should work on known range of UInts" in {
        for {
          bitWidth1 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfUIntOfWidth(bitWidth1)
          for {
            num1 <- lo1 to hi1
          } {
            doNegCheck(num1)
          }
        }
      }

    }

    "Not should pass the following tests" - {
      def doNotCheck(num1: Big, width: Int): Unit = {
        val got = (
          NotInts(() => num1.toInt, width).apply(),
          NotLongs(() => num1.toLong, width).apply(),
          NotBigs(() => num1, width).apply()
        )
        val expected = (
          BitTwiddlingUtils.not(num1, width),
          BitTwiddlingUtils.not(num1, width),
          BitTwiddlingUtils.not(num1, width)
        )

        got should be(expected)
      }

      "bits should work on known range of SInts" in {
        for {
          bitWidth1 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfSIntOfWidth(bitWidth1)
          for {
            num1 <- lo1 to hi1
          } {
            doNotCheck(num1, bitWidth1)
          }
        }
      }

      "bits should work on known range of UInts" in {
        for {
          bitWidth1 <- 1 to 4
        } {
          val (lo1, hi1) = extremaOfUIntOfWidth(bitWidth1)
          for {
            num1 <- lo1 to hi1
          } {
            doNotCheck(num1, bitWidth1)
          }
        }
      }

    }
  }
}
