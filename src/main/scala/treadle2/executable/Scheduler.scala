// SPDX-License-Identifier: Apache-2.0

package treadle2.executable

import firrtl.ir.{Info, NoInfo}
import logger.LazyLogging

import scala.collection.mutable

/** The scheduler holds the ordered assignment statements of the entire circuit.
  * Clocks have magic shadow symbols "clockName/prev". These shadows are
  * used to make the circuit evaluation idempotent, i.e. evaluating the
  * circuit at the moment of an positive clock transition can be done
  * repeatedly and registers will only be advanced on the first call.
  *
  * @param symbolTable symbol table is used to find orphans
  */
class Scheduler(val symbolTable: SymbolTable) extends LazyLogging {

  var combinationalAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer
  val endOfCycleAssigns:    mutable.HashSet[Assigner] = new mutable.HashSet

  val orphanedAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer

  private val toAssigner: mutable.HashMap[Symbol, Assigner] = new mutable.HashMap()

  var executionEngineOpt: Option[ExecutionEngine] = None

  def addAssigner(
    symbol:                   Symbol,
    assigner:                 Assigner,
    excludeFromCombinational: Boolean = false
  ): Unit = {

    toAssigner(symbol) = assigner
    combinationalAssigns += assigner
  }

  def addEndOfCycleAssigner(assigner: Assigner): Unit = {
    if (!endOfCycleAssigns.exists(a => a.symbol == assigner.symbol)) {
      endOfCycleAssigns += assigner
    }
  }

  def hasAssigner(symbol: Symbol): Boolean = {
    toAssigner.contains(symbol)
  }

  def getAllAssigners: Seq[Assigner] = {
    toAssigner.values.toSeq
  }

  def getAssignerInfo(symbol: Symbol): Info = {
    getAllAssigners.find(assigner => assigner.symbol == symbol) match {
      case Some(assigner) => assigner.info
      case _              => NoInfo
    }
  }

  def getAssignerInfo(symbolName: String): Info = {
    symbolTable.get(symbolName) match {
      case Some(symbol) => getAssignerInfo(symbol)
      case _            => NoInfo
    }
  }

  def inputChildrenAssigners(): Seq[Assigner] = {
    val assigners = {
      symbolTable
        .getChildren(symbolTable.inputPortsNames.map(symbolTable.nameToSymbol(_)).toSeq)
        .flatMap { symbol =>
          toAssigner.get(symbol)
        }
        .toSeq
    }
    assigners
  }

  def getAssigners(symbols: Seq[Symbol]): Seq[Assigner] = {
    val assigners = symbols.flatMap { symbol =>
      toAssigner.get(symbol)
    }
    assigners
  }

  def organizeAssigners(): Unit = {
    val orphansAndSensitives = symbolTable.orphans.flatMap(s => toAssigner.get(s)).flatMap {
      case _: BlackBoxCycler => None
      case _: StopOp         => None
      case _: PrintfOp       => None
      case assigner => Some(assigner)
    }

    setOrphanedAssigners(orphansAndSensitives)
    sortInputSensitiveAssigns()
  }

  def setVerboseAssign(isVerbose: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setVerbose(isVerbose)
    }
    getAllAssigners.foreach { setMode }
  }

  def setLeanMode(setLean: Boolean): Unit = {
    def setMode(assigner: Assigner): Unit = {
      assigner.setLeanMode(setLean)
    }
    getAllAssigners.foreach { setMode }
  }

  /** Execute the seq of assigners
    * @param assigners list of assigners
    */
  private def executeAssigners(assigners: Seq[Assigner]): Unit = {
    var index = 0
    val lastIndex = assigners.length

    while (index < lastIndex) {
      assigners(index).run()
      index += 1
    }
  }

  /**  updates signals that depend on inputs
    */
  def executeCombinationalAssigns(): Unit = {
    executeAssigners(combinationalAssigns.toSeq)
  }

  /**  updates signals that depend on inputs
    */
  def executeOrphanedAssigns(): Unit = {
    executeAssigners(orphanedAssigns.toSeq)
  }

  /** de-duplicates and sorts assignments that depend on top level inputs.
    */
  def sortInputSensitiveAssigns(): Unit = {
    val buf = mutable.ArrayBuffer[Assigner]()
    buf ++= combinationalAssigns
    buf --= orphanedAssigns
    val deduplicatedAssigns = buf.distinct
    combinationalAssigns = deduplicatedAssigns.sortBy { assigner: Assigner =>
      assigner.symbol.cardinalNumber
    } ++ endOfCycleAssigns
  }

  def setOrphanedAssigners(assigners: Seq[Assigner]): Unit = {
    orphanedAssigns.clear()
    orphanedAssigns ++= assigners
  }
}

object Scheduler {
  def apply(symbolTable: SymbolTable): Scheduler = new Scheduler(symbolTable)
}
