// SPDX-License-Identifier: Apache-2.0

package treadle2.utils

import firrtl.ir.Circuit
import firrtl.options.Dependency
import firrtl.stage.{Forms, TransformManager}
import firrtl.{CircuitState, DependencyAPIMigration, Emitter, Transform}

import scala.collection.mutable

/** Printf statements that print registers will show wrong values
  * unless this pass adds a delay for each register
  */
class AugmentPrintf extends Transform with DependencyAPIMigration {
  override def prerequisites:          Seq[TransformManager.TransformDependency] = Forms.LowForm
  override def optionalPrerequisites:  Seq[TransformManager.TransformDependency] = Forms.LowFormOptimized
  override def optionalPrerequisiteOf: Seq[Dependency[Emitter]] = Forms.LowEmitters
  override def invalidates(a: Transform): Boolean = false

  def apply(circuit: Circuit): Circuit = {
    import firrtl._
    import firrtl.ir._
    def insert(
      newStmts:        mutable.ArrayBuffer[Statement],
      namespace:       Namespace,
      info:            Info,
      clockExpression: Expression
    )(a:               Expression
    ): Expression = {
      val newName = namespace.newTemp
      val wref = WRef(newName, a.tpe, NodeKind, SourceFlow)
      newStmts += DefRegister(info, newName, a.tpe, clockExpression, UIntLiteral(0), UIntLiteral(0))
      newStmts += Connect(info, wref, a)
      wref
    }

    def fixPrintsStmt(namespace: firrtl.Namespace)(s: Statement): Statement =
      s.mapStmt(fixPrintsStmt(namespace)) match {
        case s: Stop =>
          val newStmts = mutable.ArrayBuffer[Statement]()
          val newStop = s.mapExpr(insert(newStmts, namespace, s.info, s.clk))
          newStmts += newStop
          Block(newStmts.toSeq)
        case p: Print =>
          val newStmts = mutable.ArrayBuffer[Statement]()
          val newName = namespace.newTemp
          val wref = WRef(newName, p.en.tpe, NodeKind, SourceFlow)
          newStmts += DefRegister(p.info, newName, p.en.tpe, p.clk, UIntLiteral(0), UIntLiteral(0))
          newStmts += Connect(p.info, wref, p.en)

          val newPrint: Print =
            p.mapExpr(insert(newStmts, namespace, p.info, p.clk)).asInstanceOf[Print].copy(en = wref)
          newStmts += newPrint
          Block(newStmts.toSeq)
        case v: Verification =>
          val newStmts = mutable.ArrayBuffer[Statement]()
          val enableNewName = namespace.newTemp
          val enableWRef = WRef(enableNewName, v.en.tpe, NodeKind, SourceFlow)
          newStmts += DefRegister(v.info, enableNewName, v.en.tpe, v.clk, UIntLiteral(0), UIntLiteral(0))
          newStmts += Connect(v.info, enableWRef, v.en)

          val predNewName = namespace.newTemp
          val predWRef = WRef(predNewName, v.pred.tpe, NodeKind, SourceFlow)
          newStmts += DefRegister(v.info, predNewName, v.pred.tpe, v.clk, UIntLiteral(0), UIntLiteral(0))
          newStmts += Connect(v.info, predWRef, v.pred)

          val newVerification: Verification =
            v.mapExpr(insert(newStmts, namespace, v.info, v.clk)).asInstanceOf[Verification].copy(en = enableWRef)
          newStmts += newVerification
          Block(newStmts.toSeq)
        case other => other
      }

    circuit.mapModule { m =>
      val ns = Namespace(m)
      m.mapStmt(fixPrintsStmt(ns))
    }
  }

  override protected def execute(state: CircuitState): CircuitState = {
    state.copy(circuit = apply(state.circuit))
  }
}
