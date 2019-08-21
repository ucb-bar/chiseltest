// See LICENSE for license details.

package chisel3.tester.legacy.backends.vcs

import chisel3._
import chisel3.tester.legacy.backends.verilator.VerilatorBackend

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
  dataNames: Map[Data, String],
  combinationalPaths: Map[Data, Set[Data]],
  command: Seq[String]
) extends VerilatorBackend(dut, dataNames, combinationalPaths, command)

