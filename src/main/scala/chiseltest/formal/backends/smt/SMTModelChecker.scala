// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal.backends.smt

import chiseltest.formal.backends._
import firrtl.backends.experimental.smt._

case class SMTModelCheckerOptions(checkConstraints: Boolean, checkBadStatesIndividually: Boolean)
object SMTModelCheckerOptions {
  val Default: SMTModelCheckerOptions =
    SMTModelCheckerOptions(checkConstraints = true, checkBadStatesIndividually = true)
  val Performance: SMTModelCheckerOptions =
    SMTModelCheckerOptions(checkConstraints = false, checkBadStatesIndividually = false)
}

/** SMT based bounded model checking as an alternative to dispatching to a btor2 based external solver */
class SMTModelChecker(
  solver:        Solver,
  options:       SMTModelCheckerOptions = SMTModelCheckerOptions.Performance,
  printProgress: Boolean = false)
    extends IsModelChecker {
  override val name:          String = "SMTModelChecker with " + solver.name
  override val prefix:        String = solver.name
  override val fileExtension: String = ".smt2"

  override def check(
    sys:  TransitionSystem,
    kMax: Int
  ): ModelCheckResult = {
    require(kMax > 0 && kMax <= 2000, s"unreasonable kMax=$kMax")

    val ctx = solver.createContext()
    // z3 only supports the non-standard as-const array syntax when the logic is set to ALL
    val logic = if (solver.name.contains("z3")) { "ALL" }
    else if (solver.supportsUninterpretedSorts) { "QF_AUFBV" }
    else { "QF_ABV" }
    ctx.setLogic(logic)

    // create new context
    ctx.push()

    // declare/define functions and encode the transition system
    val enc: TransitionSystemSmtEncoding = if (solver.supportsUninterpretedSorts) {
      new CompactSmtEncoding(sys)
    } else {
      new UnrollSmtEncoding(sys)
    }
    enc.defineHeader(ctx)
    enc.init(ctx)

    val constraints = sys.signals.filter(_.lbl == IsConstraint).map(_.name)
    val assertions = sys.signals.filter(_.lbl == IsBad).map(_.name)

    (0 to kMax).foreach { k =>
      if (printProgress) println(s"Step #$k")

      // assume all constraints hold in this step
      constraints.foreach(c => ctx.assert(enc.getConstraint(c)))

      // make sure the constraints are not contradictory
      if (options.checkConstraints) {
        val res = ctx.check(produceModel = false)
        assert(res.isSat, s"Found unsatisfiable constraints in cycle $k")
      }

      if (options.checkBadStatesIndividually) {
        // check each bad state individually
        assertions.zipWithIndex.foreach { case (b, bi) =>
          if (printProgress) print(s"- b$bi? ")

          ctx.push()
          ctx.assert(BVNot(enc.getAssertion(b)))
          val res = ctx.check(produceModel = false)

          // did we find an assignment for which the bad state is true?
          if (res.isSat) {
            if (printProgress) println("❌")
            val w = getWitness(ctx, sys, enc, k, Seq(b))
            ctx.pop()
            ctx.pop()
            assert(ctx.stackDepth == 0, s"Expected solver stack to be empty, not: ${ctx.stackDepth}")
            ctx.close()
            return ModelCheckFail(w)
          } else {
            if (printProgress) println("✅")
          }
          ctx.pop()
        }
      } else {
        val anyBad = BVNot(BVAnd(assertions.map(enc.getAssertion)))
        ctx.push()
        ctx.assert(anyBad)
        val res = ctx.check(produceModel = false)

        // did we find an assignment for which at least one bad state is true?
        if (res.isSat) {
          val w = getWitness(ctx, sys, enc, k)
          ctx.pop()
          ctx.pop()
          assert(ctx.stackDepth == 0, s"Expected solver stack to be empty, not: ${ctx.stackDepth}")
          ctx.close()
          return ModelCheckFail(w)
        }
        ctx.pop()
      }

      // advance
      enc.unroll(ctx)
    }

    // clean up
    ctx.pop()
    assert(ctx.stackDepth == 0, s"Expected solver stack to be empty, not: ${ctx.stackDepth}")
    ctx.close()
    ModelCheckSuccess()
  }

  private def getWitness(
    ctx:             SolverContext,
    sys:             TransitionSystem,
    enc:             TransitionSystemSmtEncoding,
    kMax:            Int,
    failedAssertion: Seq[String] = Seq()
  ): Witness = {
    // btor2 numbers states in the order that they are declared in starting at zero
    val stateInit = sys.states.zipWithIndex.map {
      case (State(sym: BVSymbol, _, _), ii) =>
        ctx.getValue(enc.getSignalAt(sym, 0)) match {
          case Some(value) => (Some(ii -> value), None)
          case None        => (None, None)
        }
      case (State(sym: ArraySymbol, _, _), ii) =>
        val value = ctx.getValue(enc.getSignalAt(sym, 0))
        (None, Some(ii -> value))
    }

    val regInit = stateInit.flatMap(_._1).toMap
    val memInit = stateInit.flatMap(_._2).toMap

    val inputs = (0 to kMax).map { k =>
      sys.inputs.zipWithIndex.flatMap { case (input, i) =>
        ctx.getValue(enc.getSignalAt(input, k)).map(value => i -> value)
      }.toMap
    }

    Witness(failedAssertion, regInit, memInit, inputs)
  }

}

trait TransitionSystemSmtEncoding {
  def defineHeader(ctx:   SolverContext): Unit
  def init(ctx:           SolverContext): Unit
  def unroll(ctx:         SolverContext): Unit
  def getConstraint(name: String):        BVExpr
  def getAssertion(name:  String): BVExpr
  def getSignalAt(sym:    BVSymbol, k: Int): BVExpr
  def getSignalAt(sym:    ArraySymbol, k: Int): ArrayExpr
}
