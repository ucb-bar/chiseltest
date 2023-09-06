// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.sequences

import firrtl2._
import firrtl2.analyses.InstanceKeyGraph
import firrtl2.stage.Forms

/** Inlines all modules in the circuit into the top. Works on High Firrtl. warn: Currently does not rename any
  * annotations!
  */
object HighFirrtlFlatten extends Transform {
  override def prerequisites = Forms.Resolved
  override def invalidates(a: Transform) = false
  override protected def execute(state: CircuitState): CircuitState = {
    val iGraph = InstanceKeyGraph(state.circuit)
    val hasChildren = iGraph.getChildInstances.filter(_._2.nonEmpty).map(_._1).toSet
    // we visit modules from bottom to top and skip any that do not have children
    val inlineOrder = iGraph.moduleOrder.reverse.filter(m => hasChildren(m.name)).map(_.asInstanceOf[ir.Module])

    var modules = state.circuit.modules.collect { case m: ir.Module => m.name -> m }.toMap
    inlineOrder.foreach { mod =>
      val inlined = inlineModule(modules.get)(mod)
      modules = modules + (inlined.name -> inlined)
    }
    assert(modules.size == 1)

    val circuit = state.circuit.copy(modules = Seq(modules.head._2))
    state.copy(circuit = circuit)
  }

  private def inlineModule(getModule: String => Option[ir.Module])(m: ir.Module): ir.Module = {
    ???

  }

  private def onStmt(namespace: Namespace, getModule: String => Option[ir.Module])(s: ir.Statement): ir.Statement = {
    ???
  }

}
