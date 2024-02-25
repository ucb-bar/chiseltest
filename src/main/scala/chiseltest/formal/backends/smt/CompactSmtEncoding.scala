// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal.backends.smt

import firrtl2.backends.experimental.smt._
import scala.collection.mutable

class CompactSmtEncoding(sys: TransitionSystem) extends TransitionSystemSmtEncoding {
  import SMTTransitionSystemEncoder._
  private def id(s: String): String = SMTLibSerializer.escapeIdentifier(s)
  private val stateType = id(sys.name + "_s")
  private val stateInitFun = id(sys.name + "_i")
  private val stateTransitionFun = id(sys.name + "_t")

  private val states = mutable.ArrayBuffer[UTSymbol]()

  def defineHeader(ctx: SolverContext): Unit = encode(sys).foreach(ctx.runCommand)

  private def appendState(ctx: SolverContext): UTSymbol = {
    val s = UTSymbol(s"s${states.length}", stateType)
    ctx.runCommand(DeclareUninterpretedSymbol(s.name, s.tpe))
    states.append(s)
    s
  }

  def init(ctx: SolverContext, isArbitraryStep: Boolean): Unit = {
    assert(states.isEmpty)
    val s0 = appendState(ctx)
    if (!isArbitraryStep) {
      ctx.assert(BVFunctionCall(stateInitFun, List(s0), 1))
    }
  }

  def unroll(ctx: SolverContext): Unit = {
    assert(states.nonEmpty)
    appendState(ctx)
    val tStates = states.takeRight(2).toList
    ctx.assert(BVFunctionCall(stateTransitionFun, tStates, 1))
  }

  /** returns an expression representing the constraint in the current state */
  def getConstraint(name: String): BVExpr = {
    assert(states.nonEmpty)
    val foo = id(name + "_f")
    BVFunctionCall(foo, List(states.last), 1)
  }

  /** returns an expression representing the assertion in the current state */
  def getAssertion(name: String): BVExpr = {
    assert(states.nonEmpty)
    val foo = id(name + "_f")
    BVFunctionCall(foo, List(states.last), 1)
  }

  def getSignalAt(sym: BVSymbol, k: Int): BVExpr = {
    assert(states.length > k, s"no state s$k")
    val state = states(k)
    val foo = id(sym.name + "_f")
    BVFunctionCall(foo, List(state), sym.width)
  }

  def getSignalAt(sym: ArraySymbol, k: Int): ArrayExpr = {
    assert(states.length > k, s"no state s$k")
    val state = states(k)
    val foo = id(sym.name + "_f")
    ArrayFunctionCall(foo, List(state), sym.indexWidth, sym.dataWidth)
  }
}
