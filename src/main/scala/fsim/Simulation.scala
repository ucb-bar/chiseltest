// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

/** Presents an interface to the outside world to interact with a simulation. */
class Simulation(exec: Executable) {
  private val data = exec.data
  private val symbols = exec.info.symbols
  private val sortedSymbols = symbols.values.toSeq.sortBy(_.index)
  private val inputs = sortedSymbols.collect { case i @ IntSymbol(_, IsInput, _, _, _, _) => i }
  private val clockInputs = inputs.filter(_.clock)

  // simple offset mechanism allows us to catch long/big/bool confusions
  private val BoolOffset = sortedSymbols.length * 2
  private val LongOffset = 0
  private val BigOffset = sortedSymbols.length * 4
  private def getOffset(kind: WidthUtils.WidthKind): Int = kind match {
    case WidthUtils.BoolWidth => BoolOffset
    case WidthUtils.LongWidth => LongOffset
    case WidthUtils.BigWidth  => BigOffset
  }

  def getSymbolId(name: String): Int = {
    val sym = symbols.getOrElse(name, throw new RuntimeException(s"Unknown symbol $name"))
    sym.index + getOffset(WidthUtils.toKind(sym.width))
  }

  private var inputPoked = false
  def peekLong(id: Int): Long = {
    updateComb()
    data.longData(id - LongOffset)
  }
  def pokeLong(id: Int, value: Long): Unit = {
    inputPoked = true
    data.longData(id - LongOffset) = value
  }

  def peekBool(id: Int): Boolean = {
    updateComb()
    data.boolData(id - BoolOffset)
  }

  def pokeBool(id: Int, value: Boolean): Unit = {
    inputPoked = true
    data.boolData(id - BoolOffset) = value
  }

  private lazy val clockSym = clockInputs match {
    case Seq()      => throw new RuntimeException("Cannot step a circuit with no clock inputs!")
    case Seq(clock) => clock
    case other =>
      throw new RuntimeException(s"Circuits with multiple different clock are currently not supported! $other")
  }
  private var stepCount: Int = 0
  def step(): Unit = {
    // enable clock
    data.boolData(clockSym.index) = true
    // update
    exec.update()
    // disable clock
    data.boolData(clockSym.index) = false
    stepCount += 1
  }
  def getStepCount: Int = stepCount

  @inline private def updateComb(): Unit = {
    if (inputPoked) {
      inputPoked = false
      exec.update()
    }
  }

  def printSignals(): Unit = {
    println(s"Values at Step #$stepCount")
    sortedSymbols.collect { case i: IntSymbol => i }.foreach { sym =>
      val value = WidthUtils.toKind(sym.width) match {
        case WidthUtils.BoolWidth => data.boolData(sym.index).toString
        case WidthUtils.LongWidth => data.longData(sym.index).toString
        case WidthUtils.BigWidth  => data.bigData(sym.index).toString
      }
      println(s"${sym.name} = $value")
    }
    println()
  }

}
