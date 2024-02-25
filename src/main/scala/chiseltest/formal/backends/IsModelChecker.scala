// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>
package chiseltest.formal.backends

import firrtl2.backends.experimental.smt._

private[chiseltest] trait ModelCheckResult {
  def isFail:    Boolean
  def isSuccess: Boolean = !isFail
}
private[chiseltest] case class ModelCheckSuccess() extends ModelCheckResult { override def isFail: Boolean = false }
private[chiseltest] case class ModelCheckFail(witness: Witness) extends ModelCheckResult {
  override def isFail: Boolean = true
}
private[chiseltest] case class ModelCheckFailInduction(witness: Witness) extends ModelCheckResult {
  override def isFail: Boolean = true
}

private[chiseltest] trait IsModelChecker {
  def name: String
  val prefix:        String
  val fileExtension: String
  def checkBounded(sys:   TransitionSystem, kMax:        Int = -1): ModelCheckResult
  def checkInduction(sys: TransitionSystem, resetLength: Int, kMax: Int = -1): ModelCheckResult
}

private[chiseltest] case class Witness(
  failed:  Seq[String],
  regInit: Map[Int, BigInt],
  memInit: Map[Int, Seq[(Option[BigInt], BigInt)]],
  inputs:  Seq[Map[Int, BigInt]])
