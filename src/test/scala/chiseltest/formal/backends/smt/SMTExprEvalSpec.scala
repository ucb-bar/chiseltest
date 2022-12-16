// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends.smt

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.PrivateMethodTester

import firrtl.backends.experimental.smt._

class SMTExprEvalSpec extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption with PrivateMethodTester {
  behavior of "SMTExprEval" 
  it should "could deal with signed division and remainder in doBVOp" in {
    def doBVOp(op: Op.Value, a: BigInt, b: BigInt, width: Int): BigInt = {
      class SMTExprEval
      val f = PrivateMethod[BigInt](Symbol("doBVOp"))
      SMTExprEval.invokePrivate[BigInt](f(op, a, b, width))
    }
    assert(doBVOp(Op.SignedDiv, BigInt(10),  BigInt(3),   8) == BigInt(3))   //  10 /  3 =  3
    assert(doBVOp(Op.SignedDiv, BigInt(246), BigInt(253), 8) == BigInt(3))   // -10 / -3 =  3
    assert(doBVOp(Op.SignedDiv, BigInt(246), BigInt(3),   8) == BigInt(253)) // -10 /  3 = -3
    assert(doBVOp(Op.SignedDiv, BigInt(10),  BigInt(253), 8) == BigInt(253)) //  10 / -3 = -3

    assert(doBVOp(Op.SignedRem, BigInt(10),  BigInt(3),   8) == BigInt(1))   //  10 %  3 =  1
    assert(doBVOp(Op.SignedRem, BigInt(246), BigInt(253), 8) == BigInt(255)) // -10 % -3 = -1
    assert(doBVOp(Op.SignedRem, BigInt(246), BigInt(3),   8) == BigInt(255)) // -10 %  3 = -1
    assert(doBVOp(Op.SignedRem, BigInt(10),  BigInt(253), 8) == BigInt(1))   //  10 % -3 =  1

    assert(doBVOp(Op.SignedMod, BigInt(10),  BigInt(3),   8) == BigInt(1))   //  10 mod  3 =  1
    assert(doBVOp(Op.SignedMod, BigInt(246), BigInt(253), 8) == BigInt(255)) // -10 mod -3 = -1
    assert(doBVOp(Op.SignedMod, BigInt(246), BigInt(3),   8) == BigInt(2))   // -10 mod  3 =  2
    assert(doBVOp(Op.SignedMod, BigInt(10),  BigInt(253), 8) == BigInt(254)) //  10 mod -3 = -2

    assert(doBVOp(Op.SignedDiv, BigInt(128), BigInt(2),  8) == BigInt(192)) // -128 / 2 = -64
    assert(doBVOp(Op.SignedDiv, BigInt(46),  BigInt(2),  8) == BigInt(23))

    assert(doBVOp(Op.SignedDiv, BigInt(46),  BigInt(0),  8) == BigInt(255)) // all one
    assert(doBVOp(Op.SignedRem, BigInt(46),  BigInt(0),  8) == BigInt(46)) 
    assert(doBVOp(Op.SignedMod, BigInt(46),  BigInt(0),  8) == BigInt(46)) 
  }
}
