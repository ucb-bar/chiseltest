// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2._

/** Display information about all coverage instrumentation. This pass does not modify the circuit itself, it only prints
  * out some information. Make sure to set the log level at least to "info" to see the output.
  */
object CoverageStatisticsPass extends Transform {
  override def prerequisites = Seq()
  override def optionalPrerequisites = Coverage.AllPasses
  override def optionalPrerequisiteOf = AllEmitters()
  override def invalidates(a: Transform) = false

  override def execute(state: CircuitState): CircuitState = {
    analysis.foreach(a => a(state))
    state
  }

  private val analysis = Seq(
    generalAnalysis(_),
    analyzeLineCoverage(_),
    analyzeToggleCoverage(_),
    analyzeFsmCoverage(_)
  )

  private def generalAnalysis(state: CircuitState): Unit = {
    val coverPoints = state.annotations.collect { case a: CoverageInfo => a }.size
    logger.info("Coverage Statistics:")
    logger.info(s"- Total automatic cover points: $coverPoints")
    val ignored = Coverage.collectModulesToIgnore(state)
    if (ignored.nonEmpty) {
      logger.info(s"- Ignored modules: " + ignored.toSeq.sorted.mkString(", "))
    }
  }

  private def analyzeLineCoverage(state: CircuitState): Unit = {
    val line = state.annotations.collect { case a: LineCoverageAnnotation => a }
    if (line.nonEmpty) {
      logger.info("Line Coverage:")
      logger.info(s"- Line cover points: ${line.size}")
    }
  }

  private def analyzeToggleCoverage(state: CircuitState): Unit = {
    val annos = state.annotations
    val toggle = annos.collect { case a: ToggleCoverageAnnotation => a }
    if (toggle.nonEmpty) {
      logger.info("Toggle Coverage:")
      logger.info(s"- Toggle cover points: ${toggle.size}")
      val allBits = toggle.flatMap(a => a.signals.map(_.toString() + "[" + a.bit + "]"))
      val allSignals = toggle.flatMap(_.signals.map(_.toString())).distinct
      logger.info(s"- Signals covered: ${allSignals.size}")
      logger.info(s"- Signal Bits covered: ${allBits.size}")
      val opts = Seq(
        PortToggleCoverage -> "ports",
        RegisterToggleCoverage -> "regs",
        MemoryToggleCoverage -> "mems",
        WireToggleCoverage -> "wires"
      )
      val on = opts.map { case (a, s) => if (annos.contains(a)) s + " ✅" else s + " ❌" }.mkString(" ")
      logger.info("- " + on)
    }
  }

  private def analyzeFsmCoverage(state: CircuitState): Unit = {
    val annos = state.annotations
    val fsms = annos.collect { case a: FsmCoverageAnnotation => a }
    if (fsms.nonEmpty) {
      logger.info("FSM Coverage:")
      fsms.foreach { case FsmCoverageAnnotation(stateReg, states, transitions) =>
        logger.info(s"- ${stateReg}: ${states.length} states, ${transitions.length} transitions")
      }
    }
  }
}
