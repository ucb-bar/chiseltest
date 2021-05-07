// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends.verilator

import chiseltest.coverage.SimulatorCoverageTest
import chiseltest.internal.VerilatorBackendAnnotation

class VerilatorCoverageTest extends SimulatorCoverageTest("Verilator", VerilatorBackendAnnotation) {}
