// See LICENSE for license details.

package chiseltest.legacy.backends.vcs

import chisel3._
import chiseltest.legacy.backends.verilator.VerilatorBackend
import firrtl.ir.Circuit

/** Supports Backend and Threaded traits for ex
  *
  * @param dut                  the device under test
  * @param dataNames            basically the IO ports
  * @param combinationalPaths   paths detected by CheckCombLoop
  * @param command              the simulation program to execute
  * @tparam T                   the dut's type
  */
class VcsBackend[T <: MultiIOModule](
  dut: T,
  fir: Circuit,
  dataNames: Map[Data, String],
  combinationalPaths: Map[Data, Set[Data]],
  command: Seq[String]
) extends VerilatorBackend(dut, fir, dataNames, combinationalPaths, command)

