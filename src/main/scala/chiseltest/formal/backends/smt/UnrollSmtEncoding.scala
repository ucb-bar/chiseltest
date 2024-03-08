// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.formal.backends.smt

import firrtl2.backends.experimental.smt._

class UnrollSmtEncoding(sys: TransitionSystem) extends TransitionSystemSmtEncoding {
  private def at(sym:  SMTSymbol, step: Int): SMTSymbol = sym.rename(s"${sym.name}@$step")
  private def at(name: String, step:    Int): String = s"$name@$step"
  private var currentStep = -1
  private val isSignal = (sys.inputs.map(_.name) ++ sys.signals.map(_.name) ++ sys.states.map(_.name)).toSet

  override def defineHeader(ctx: SolverContext): Unit = {
    // nothing to do in this encoding
  }

  override def init(ctx: SolverContext, isArbitraryStep: Boolean): Unit = {
    require(currentStep == -1)
    currentStep = 0
    // declare initial states
    sys.states.foreach { state =>
      state.init match {
        case Some(value) =>
          if (isArbitraryStep) {
            ctx.runCommand(DeclareFunction(at(state.sym, 0), Seq()))
          } else {
            ctx.runCommand(DefineFunction(at(state.name, 0), Seq(), signalsAndStatesAt(value, 0)))
          }
        case None =>
          ctx.runCommand(DeclareFunction(at(state.sym, 0), Seq()))
      }
    }
    // define the inputs for the initial state and all signals derived from it
    defineInputsAndSignals(ctx, currentStep)
  }

  override def unroll(ctx: SolverContext): Unit = {
    val (prevStep, nextStep) = (currentStep, currentStep + 1)
    // define next state
    sys.states.foreach { state =>
      state.next match {
        case Some(value) =>
          ctx.runCommand(DefineFunction(at(state.name, nextStep), Seq(), signalsAndStatesAt(value, prevStep)))
        case None =>
          ctx.runCommand(DeclareFunction(at(state.sym, nextStep), Seq()))
      }
    }

    // declare the inputs and all signals derived from the new state and inputs
    defineInputsAndSignals(ctx, nextStep)

    // update step count
    currentStep = nextStep
  }

  private def defineInputsAndSignals(ctx: SolverContext, step: Int): Unit = {
    // declare inputs
    sys.inputs.foreach(input => ctx.runCommand(DeclareFunction(at(input, step), Seq())))

    // define signals
    sys.signals.foreach { signal =>
      ctx.runCommand(DefineFunction(at(signal.name, step), Seq(), signalsAndStatesAt(signal.e, step)))
    }
  }

  private def signalsAndStatesAt(expr: SMTExpr, step: Int): SMTExpr = expr match {
    case sym: SMTSymbol if isSignal(sym.name) => at(sym, step)
    case other => SMTExprMap.mapExpr(other, signalsAndStatesAt(_, step))
  }

  private val isConstraint = sys.signals.filter(_.lbl == IsConstraint).map(_.name).toSet
  override def getConstraint(name: String): BVExpr = {
    require(isConstraint(name))
    require(currentStep >= 0)
    at(BVSymbol(name, 1), currentStep).asInstanceOf[BVExpr]
  }

  private val isBad = sys.signals.filter(_.lbl == IsBad).map(_.name).toSet
  override def getAssertion(name: String): BVExpr = {
    require(isBad(name))
    require(currentStep >= 0)
    // we need to invert because we are asked for the assertion, not for the bad state!
    BVNot(at(BVSymbol(name, 1), currentStep).asInstanceOf[BVExpr])
  }

  override def getSignalAt(sym: BVSymbol, k: Int): BVExpr = {
    require(isSignal(sym.name))
    require(k <= currentStep)
    at(sym, k).asInstanceOf[BVExpr]
  }

  override def getSignalAt(sym: ArraySymbol, k: Int): ArrayExpr = {
    require(isSignal(sym.name))
    require(k <= currentStep)
    at(sym, k).asInstanceOf[ArrayExpr]
  }
}
