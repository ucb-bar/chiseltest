// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.annotations._
import firrtl2.options.Dependency
import firrtl2.stage.Forms
import firrtl2.transforms._

case class KeepClockAndResetAnnotation(target: ReferenceTarget)
    extends SingleTargetAnnotation[ReferenceTarget]
    with HasDontTouches {
  override def duplicate(n: ReferenceTarget) = copy(target = n)
  override def dontTouches = List(target)
}

/** Marks all `clock` and `reset` signals as DontTouch so that they are not removed by Dead Code Elimination. This makes
  * adding coverage that relies on those pins being available easier.
  */
object KeepClockAndResetPass extends Transform {
  // try to run early
  override def prerequisites = Forms.Checks
  override def invalidates(a: Transform) = false
  // need to run before DCE
  override def optionalPrerequisiteOf = Seq(Dependency[DeadCodeElimination])

  override def execute(state: CircuitState): CircuitState = {
    val c = CircuitTarget(state.circuit.main)
    val annos = state.circuit.modules.flatMap(onModule(_, c))
    state.copy(annotations = annos ++ state.annotations)
  }

  private def onModule(m: ir.DefModule, c: CircuitTarget): List[KeepClockAndResetAnnotation] = m match {
    case mod: ir.Module =>
      val clock = Builder.findClocks(mod)
      val reset = Builder.findResets(mod)
      val mRef = c.module(mod.name)
      (clock).map(e => KeepClockAndResetAnnotation(Builder.refToTarget(mRef, e))).toList
    case _ => List()
  }
}

object RemoveKeepClockAndResetAnnotations extends Transform {
  override def prerequisites = Seq(Dependency(KeepClockAndResetPass))
  override def invalidates(a: Transform) = a == KeepClockAndResetPass
  override def execute(state: CircuitState): CircuitState = {
    val filtered = state.annotations.filterNot(_.isInstanceOf[KeepClockAndResetAnnotation])
    state.copy(annotations = filtered)
  }
}
