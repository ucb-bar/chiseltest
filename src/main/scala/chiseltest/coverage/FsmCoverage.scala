// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._
import firrtl2.options.Dependency
import firrtl2.stage.RunFirrtlTransformAnnotation

object FsmCoverage {
  def annotations: AnnotationSeq = Seq(
    RunFirrtlTransformAnnotation(Dependency(FsmCoveragePass)),
    RunFirrtlTransformAnnotation(Dependency(ModuleInstancesPass))
  )

  def processCoverage(annos: AnnotationSeq): Seq[FsmCoverageData] = {
    val fsms = annos.collect { case a: FsmCoverageAnnotation => a }
    val cov = Coverage.collectTestCoverage(annos).toMap
    val moduleToInst = Coverage.moduleToInstances(annos)

    fsms.flatMap { fsm =>
      val top = fsm.stateReg.circuit + "."
      moduleToInst(fsm.stateReg.module).map { inst =>
        val states = fsm.states
          .map(s => s._1 -> cov(Coverage.path(inst, s._2.ref)))
          .toList
          .sortBy(_._1)
        val transitions = fsm.transitions
          .map(t => t._1 -> cov(Coverage.path(inst, t._2.ref)))
          .toList
          .sortBy(_._1)
        FsmCoverageData(top + Coverage.path(inst, fsm.stateReg.ref), states, transitions)
      }
    }
  }

  def textReport(data: Seq[FsmCoverageData]): Iterable[String] = data.flatMap { fsm =>
    Seq(fsm.name, fsm.states.map { case (name, count) => s"$name ($count)" }.mkString(", ")) ++
      fsm.transitions.map { case ((from, to), count) => s"$from -> $to: $count" }
  }
}

case class FsmCoverageData(name: String, states: List[(String, Long)], transitions: List[((String, String), Long)])
