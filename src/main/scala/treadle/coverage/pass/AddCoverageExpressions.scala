// SPDX-License-Identifier: Apache-2.0

package treadle.coverage.pass

import firrtl.{CircuitState, Namespace, Transform}
import firrtl.PrimOps.Not
import firrtl.ir.{
  Block,
  Circuit,
  Connect,
  DefModule,
  DoPrim,
  Expression,
  ExtModule,
  HasInfo,
  Module,
  Mux,
  NoInfo,
  Statement
}
import firrtl.options.Dependency
import firrtl.passes.{InferTypes, Pass, ResolveFlows, ResolveKinds}
import firrtl.stage.TransformManager.TransformDependency
import firrtl.transforms.BlackBoxSourceHelper
import treadle.EnableCoverageAnnotation
import treadle.coverage.{CoverageInfo, Ledger}

import scala.collection.mutable

/** Adds additional coverage statements to the Low FIRRTL source at every mux location.
  * These statements can then be used to gather statement coverage information from inside of the Treadle tester
  */
object AddCoverageExpressions extends Pass {

  override def prerequisites: Seq[TransformDependency] = firrtl.stage.Forms.LowForm

  override def name = "Coverage!"

  override def invalidates(a: Transform): Boolean = a match {
    case InferTypes | ResolveKinds | ResolveFlows => true
    case _                                        => false
  }

  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(Dependency[BlackBoxSourceHelper])

  /** Only run this pass if there is an EnableCoverageAnnotation present
    *
    * @param state
    * @return
    */
  override def execute(state: CircuitState): CircuitState = {
    if (state.annotations.contains(EnableCoverageAnnotation)) {
      state.copy(circuit = run(state.circuit))
    } else {
      state
    }
  }

  /** Run the coverage extension on every module
    * @param c the circuit on with we want to add the coverage extensions
    * @return the newly modified version of the circuit including coverage validators
    */
  override def run(c: Circuit): Circuit = {
    //Check that the circuit only contains one module
    if (c.modules.length > 1)
      throw new IllegalArgumentException("Coverage doesn't support multi-module circuits yet!")

    val ledger = new Ledger()
    val newModule = c.modules.map(coverM(ledger))

    //Return a new circuit containing the modified module and info about the added ports
    c.copy(info = c.info ++ CoverageInfo(ledger.ports.map(_.name)), modules = newModule)
  }

  /** Run coverage on every statement and then add additional coverage statements
    */
  private def coverM(ledger: Ledger)(module: DefModule): DefModule = {
    //Set the module name in the ledger
    ledger.setModuleName(module.name)

    val namespace = Namespace(module)
    val newModule = module.mapStmt(coverS(ledger, namespace))
    val newPorts = newModule.ports ++ ledger.ports

    //Add new ports to the module
    newModule match {
      case mod:    Module    => mod.copy(ports = newPorts)
      case extMod: ExtModule => extMod.copy(ports = newPorts)
    }
  }

  /** Traverse statements, find muxes and insert coverage expressions there
    */
  private def coverS(ledger: Ledger, namespace: Namespace)(s: Statement): Statement = {
    s match {
      case s: HasInfo =>
        val block = mutable.ArrayBuffer[Statement]()
        val newStmt = s.mapExpr(coverE(ledger, namespace, block))
        block.length match {
          case 0 => newStmt
          case _ =>
            block += newStmt
            Block(block.toSeq)
        }
      case s => s.mapStmt(coverS(ledger, namespace))
    }
  }

  private def coverE(
    ledger:    Ledger,
    namespace: Namespace,
    block:     mutable.ArrayBuffer[Statement]
  )(e:         Expression
  ): Expression =
    e.mapExpr(coverE(ledger, namespace, block)) match {
      //Look for muxes, if we find one, add the two coverage statements
      case Mux(cond, _, _, tpe) =>
        ledger.foundMux()

        //Create the two new ports that we will use as coverage validators
        val ref1 = ledger.addCoveragePort(namespace)
        val ref2 = ledger.addCoveragePort(namespace)
        block += Connect(NoInfo, ref1, cond)
        block += Connect(NoInfo, ref2, DoPrim(Not, Seq(cond), Seq(), tpe))
        e

      //We don't care about non-mux expressions
      case notmux => notmux
    }
}
