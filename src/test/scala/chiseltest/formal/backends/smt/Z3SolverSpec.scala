// SPDX-License-Identifier: Apache-2.0
package chiseltest.formal.backends.smt

import chiseltest.formal.FormalTag
import org.scalatest.flatspec.AnyFlatSpec
import firrtl.backends.experimental.smt._

class Z3SolverSpec extends AnyFlatSpec {

  private def makeSolver() = Z3SMTLib.createContext()

  // simple sanity check of the SMTLib based interface to z3
  it should "check a small bitvector example" taggedAs FormalTag in {
    val (a, b) = (BVSymbol("a", 8), BVSymbol("b", 8))
    val a_gt_b = BVComparison(Compare.Greater, a, b, signed = false)
    val a_lt_2 = BVNot(BVComparison(Compare.GreaterEqual, a, BVLiteral(2, 8), signed = false))
    val b_gt_2 = BVComparison(Compare.Greater, b, BVLiteral(2, 8), signed = false)

    val solver = makeSolver()
    solver.setLogic("QF_BV")
    solver.runCommand(DeclareFunction(a, Seq()))
    solver.runCommand(DeclareFunction(b, Seq()))

    // assert a > b and a < 2 and b > 2 --> UNSAT
    solver.assert(a_gt_b)
    solver.assert(a_lt_2)

    solver.push()
    solver.assert(b_gt_2)
    assert(solver.check(produceModel = false).isUnSat)
    solver.pop()

    // the above is equivalent to
    assert(solver.check(b_gt_2, produceModel = false).isUnSat)

    // assert a > b and a < 2 --> SAT
    assert(solver.check(produceModel = true).isSat)

    // get the model
    val a_value = solver.queryModel(a).get
    val b_value = solver.queryModel(b).get

    assert(a_value < 2)
    assert(a_value > b_value)

    solver.close()
  }
}
