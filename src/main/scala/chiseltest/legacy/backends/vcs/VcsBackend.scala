// SPDX-License-Identifier: Apache-2.0

package chiseltest.legacy.backends.vcs

import chisel3._
import chiseltest.legacy.backends.verilator.VerilatorBackend

/** Supports Backend and Threaded traits for ex
  *
  * @param dut                  the device under test
  * @param dataNames            basically the IO ports
  * @param combinationalPaths   paths detected by CheckCombLoop
  * @param command              the simulation program to execute
  * @tparam T                   the dut's type
  */
class VcsBackend[T <: Module](
  dut:                T,
  dataNames:          Map[Data, String],
  combinationalPaths: Map[Data, Set[Data]],
  command:            Seq[String],
  targetDir:          String)
    extends VerilatorBackend(dut, dataNames, combinationalPaths, command, targetDir)
