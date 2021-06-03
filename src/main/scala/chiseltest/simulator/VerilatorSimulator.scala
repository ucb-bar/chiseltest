package chiseltest.simulator
import firrtl.CircuitState

object VerilatorSimulator extends Simulator {
  override def name: String = "verilator"

  /** is this simulator installed on the local machine? */
  override def isAvailable: Boolean = ???

  /** search the local computer for an installation of this simulator and print versions */
  override def findVersions: Unit = ???

  /** start a new simulation
   *
   * @param state LoFirrtl circuit + annotations
   */
  override def createContext(state: CircuitState): SimulatorContext = ???
}
