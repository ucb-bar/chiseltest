// Copyright 2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package fsim

/** Presents an interface to the outside world to interact with a simulation. */
class Simulation(exec: Executable) {
  private val data = exec.data
  private val instructions = exec.instructions
  private val symbols = exec.info.symbols

  def getSymbolId(name: String): Int = {
    val sym = symbols.getOrElse(name, throw new RuntimeException(s"Unknown symbol $name"))
    sym.index
  }

  private var inputPoked = false
  def peekLong(id: Int): Long = {
    update()
    data.longData(id)
  }
  def pokeLong(id: Int, value: Long): Unit = {
    inputPoked = true
    data.longData(id) = value
  }

  @inline private def update(): Unit = {
    if (inputPoked) {
      inputPoked = false
      exec.update()
    }
  }

}
