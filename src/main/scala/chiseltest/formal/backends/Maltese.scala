// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends

import chiseltest.formal.FailedBoundedCheckException
import firrtl._
import firrtl.annotations._
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage, RunFirrtlTransformAnnotation}
import maltese.mc._
import maltese.smt.solvers._
import maltese.smt
import chiseltest.simulator.{Compiler, WriteVcdAnnotation}
import firrtl.options.Dependency

sealed trait FormalEngineAnnotation extends NoTargetAnnotation

/** Use a SMTLib based model checker with the CVC4 SMT solver.
  * @note CVC4 often performs better than Z3.
  */
case object CVC4EngineAnnotation extends FormalEngineAnnotation

/** Use a SMTLib based model checker with the Z3 SMT solver.
  * @note Z3 is the most widely available and easiest to install SMT solver.
  */
case object Z3EngineAnnotation extends FormalEngineAnnotation

/** Uses the btormc model checker from the boolector code base.
  * @note btormc is generally faster than Z3 or CVC4 but needs to be built from source
  */
private case object BtormcEngineAnnotation extends FormalEngineAnnotation

/** Formal Verification based on the firrtl compiler's SMT backend and the maltese SMT libraries solver bindings. */
private[chiseltest] object Maltese {
  def bmc(circuit: ir.Circuit, annos: AnnotationSeq, kMax: Int, resetLength: Int = 0): Unit = {
    require(kMax > 0)
    require(resetLength >= 0)
    val targetDir = Compiler.requireTargetDir(annos)
    val sysInfo = toTransitionSystem(circuit, annos)
    val checkers = makeCheckers(annos)
    assert(checkers.size == 1, "Parallel checking not supported atm!")
    checkers.head.check(sysInfo.sys, kMax = (kMax + resetLength)) match {
      case ModelCheckFail(witness) =>
        val writeVcd = annos.contains(WriteVcdAnnotation)
        if (writeVcd) {
          val sim = new TransitionSystemSimulator(sysInfo.sys)
          sim.run(witness, vcdFileName = Some((targetDir / s"${circuit.main}.bmc.vcd").toString))
          val trace = witnessToTrace(sysInfo, witness)
          Trace.replayOnTreadle(trace, circuit, annos)
        }
        val failSteps = witness.inputs.length - 1 - resetLength
        throw FailedBoundedCheckException(circuit.main, failSteps)
      case ModelCheckSuccess() => // good!
    }
  }

  private val LoweringAnnos: AnnotationSeq = Seq(
    // we need to flatten the whole circuit
    RunFirrtlTransformAnnotation(Dependency(FlattenPass)),
    RunFirrtlTransformAnnotation(Dependency[firrtl.passes.InlineInstances])
  )

  private case class SysInfo(sys: TransitionSystem, stateMap: Map[String, String], memDepths: Map[String, Int])

  private def toTransitionSystem(circuit: ir.Circuit, annos: AnnotationSeq): SysInfo = {
    val logLevel = Seq() // Seq("-ll", "info")
    val res = firrtlStage.execute(
      Array("--start-from", "low", "-E", "smt2") ++ logLevel,
      FirrtlCircuitAnnotation(circuit) +: annos ++: LoweringAnnos
    )
    val stateMap = FlattenPass.getStateMap(circuit.main, res)
    val memDepths = FlattenPass.getMemoryDepths(circuit.main, res)
    val sys = firrtl.backends.experimental.smt.ExpressionConverter.toMaltese(res).get

    // print the system, convenient for debugging, might disable once we have done more testing
    if (true) {
      val targetDir = Compiler.requireTargetDir(annos)
      os.write.over(targetDir / s"${circuit.main}.sys", sys.serialize)
    }

    SysInfo(sys, stateMap, memDepths)
  }

  private def firrtlStage = new FirrtlStage

  private def makeCheckers(annos: AnnotationSeq): Seq[IsModelChecker] = {
    val engines = annos.collect { case a: FormalEngineAnnotation => a }
    assert(engines.nonEmpty, "You need to provide at least one formal engine annotation!")
    engines.map {
      case CVC4EngineAnnotation   => new SMTModelChecker(new CVC4SMTLib)
      case Z3EngineAnnotation     => new SMTModelChecker(new Z3SMTLib)
      case BtormcEngineAnnotation => new BtormcModelChecker
    }
  }

  private def expandMemWrites(depth: Int, values: Seq[(Option[BigInt], BigInt)]): Seq[BigInt] = {
    var mem = Vector.fill(depth)(BigInt(0))
    values.foreach {
      case (None, value) =>
        mem = Vector.fill(depth)(value)
      case (Some(addr), value) =>
        mem = mem.updated(addr.toInt, value)
    }
    mem
  }

  private def witnessToTrace(sysInfo: SysInfo, w: Witness): Trace = {
    val inputNames = sysInfo.sys.inputs.map(_.name).toIndexedSeq
    val stateNames = sysInfo.sys.states.map(_.name).map(sysInfo.stateMap).toIndexedSeq

    Trace(
      inputs = w.inputs.map(_.toSeq.map { case (i, value) => inputNames(i) -> value }),
      regInit = w.regInit.toSeq.map { case (i, value) => stateNames(i) -> value },
      memInit = w.memInit.toSeq.map { case (i, values) =>
        val name = stateNames(i)
        name -> expandMemWrites(sysInfo.memDepths(name), values)
      }
    )
  }

}
