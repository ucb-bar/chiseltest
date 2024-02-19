// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import chisel3.Module
import chiseltest.HasTestName
import chiseltest.formal.backends.FormalEngineAnnotation
import chiseltest.internal.TesterUtils
import chiseltest.simulator.{Compiler, WriteVcdAnnotation}
import firrtl2.{AnnotationSeq, CircuitState}
import firrtl2.annotations.NoTargetAnnotation
import firrtl2.transforms.formal.DontAssertSubmoduleAssumptionsAnnotation

sealed trait FormalOp extends NoTargetAnnotation
case class BoundedCheck(kMax: Int = -1) extends FormalOp
case class InductionCheck(kMax: Int = -1) extends FormalOp

/** Specifies how many cycles the circuit should be reset for. */
case class ResetOption(cycles: Int = 1) extends NoTargetAnnotation {
  require(cycles >= 0, "The number of cycles must not be negative!")
}

class FailedBoundedCheckException(val message: String, val failAt: Int) extends Exception(message)
private[chiseltest] object FailedBoundedCheckException {
  def apply(module: String, failAt: Int): FailedBoundedCheckException = {
    val msg = s"[$module] found an assertion violation $failAt steps after reset!"
    new FailedBoundedCheckException(msg, failAt)
  }
}

class FailedInductionCheckException(val message: String, val failAt: Int) extends Exception(message)
private[chiseltest] object FailedInductionCheckException {
  def apply(module: String, failAt: Int): FailedInductionCheckException = {
    val msg = s"[$module] found an assertion violation after $failAt steps!"
    new FailedInductionCheckException(msg, failAt)
  }
}

/** Adds the `verify` command for formal checks to a ChiselScalatestTester */
trait Formal { this: HasTestName =>
  def verify[T <: Module](dutGen: => T, annos: AnnotationSeq, chiselAnnos: firrtl.AnnotationSeq = Seq()): Unit = {
    val withTargetDir = TesterUtils.addDefaultTargetDir(getTestName, annos)
    Formal.verify(dutGen, withTargetDir, chiselAnnos)
  }
}

/** An _escape hatch_ to disable more pessimistic modelling of undefined values. */
case object DoNotModelUndef extends NoTargetAnnotation

/** Disables firrtl optimizations when converting to a SMT/Btor2 system. This is an escape hatch in case you suspect
  * optimizations of doing something wrong! Normally this annotation should *not* be needed!
  */
case object DoNotOptimizeFormal extends NoTargetAnnotation

private object Formal {
  def verify[T <: Module](dutGen: => T, annos: AnnotationSeq, chiselAnnos: firrtl.AnnotationSeq): Unit = {
    val ops = getOps(annos)
    assert(ops.nonEmpty, "No verification operation was specified!")
    val withDefaults = addDefaults(annos)

    // elaborate the design and compile to low firrtl
    val (highFirrtl, _) = Compiler.elaborate(() => dutGen, withDefaults, chiselAnnos)
    val lowFirrtl = Compiler.toLowFirrtl(highFirrtl, Seq(DontAssertSubmoduleAssumptionsAnnotation))

    // add reset assumptions
    val withReset = AddResetAssumptionPass.execute(lowFirrtl)

    // execute operations
    val resetLength = AddResetAssumptionPass.getResetLength(withDefaults)
    ops.foreach(executeOp(withReset, resetLength, _))
  }

  val DefaultEngine: FormalEngineAnnotation = Z3EngineAnnotation
  def addDefaults(annos: AnnotationSeq): AnnotationSeq = {
    Seq(addDefaultEngine(_), addWriteVcd(_)).foldLeft(annos)((old, f) => f(old))
  }
  def addDefaultEngine(annos: AnnotationSeq): AnnotationSeq = {
    if (annos.exists(_.isInstanceOf[FormalEngineAnnotation])) { annos }
    else { DefaultEngine +: annos }
  }
  def addWriteVcd(annos: AnnotationSeq): AnnotationSeq = {
    if (annos.contains(WriteVcdAnnotation)) { annos }
    else { WriteVcdAnnotation +: annos }
  }
  def getOps(annos: AnnotationSeq): Seq[FormalOp] = {
    annos.collect { case a: FormalOp => a }.distinct
  }
  def executeOp(state: CircuitState, resetLength: Int, op: FormalOp): Unit = op match {
    case BoundedCheck(kMax) =>
      backends.Maltese.bmc(state.circuit, state.annotations, kMax = kMax, resetLength = resetLength)
    case InductionCheck(kMax) =>
      backends.Maltese.induction(state.circuit, state.annotations, kMax = kMax, resetLength = resetLength)
  }
}
