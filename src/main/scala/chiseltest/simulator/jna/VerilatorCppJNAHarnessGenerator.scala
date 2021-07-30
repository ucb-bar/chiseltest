// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jna

import chiseltest.simulator.TopmoduleInfo
import chiseltest.simulator.ipc.VerilatorCppHarnessGenerator

/** Generates the Module specific verilator harness cpp file for verilator compilation.
  *  This version generates a harness that can be called into through the JNI.
  */
private[chiseltest] object VerilatorCppJNAHarnessGenerator {
  def codeGen(
    toplevel:     TopmoduleInfo,
    vcdFilePath:  os.Path,
    targetDir:    os.Path,
    majorVersion: Int,
    minorVersion: Int
  ): (String, String) = {
    val pokeable = toplevel.inputs.zipWithIndex
    val peekable = (toplevel.inputs ++ toplevel.outputs).zipWithIndex

    val codeBuffer = new StringBuilder
    // generate Verilator specific "sim_state" class
    codeBuffer.append(s"""
struct sim_state {
  TOP_CLASS* dut;
  VERILATED_C* tfp;
  vluint64_t main_time;

  sim_state() :
    dut(new TOP_CLASS),
    tfp(nullptr),
    main_time(0)
  {
    // std::cout << "Allocating! " << ((long long) dut) << std::endl;
  }

  inline void step() { _step(tfp, dut, main_time); }
  inline void update() { dut->eval(); }
  inline void finish() {
    dut->eval();
    _finish(tfp, dut);
  }
  inline void resetCoverage() { VerilatedCov::zero(); }
  inline void writeCoverage(const char* filename) {
    VerilatedCov::write(filename);
  }
  inline void poke(int32_t id, int64_t value) {
    const uint64_t u = value;
    switch(id) {
""")
    pokeable.foreach { case ((name, _), id) =>
      codeBuffer.append(s"      case $id : dut->$name = u; break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      break;
    }
  }
  inline int64_t peek(int32_t id) {
    switch(id) {
""")
    peekable.foreach { case ((name, _), id) =>
      codeBuffer.append(s"      case $id : return dut->$name;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      return -1;
    }
  }
};

static sim_state* create_sim_state() {
  sim_state *s = new sim_state();
  std::string dumpfile = "${vcdFilePath}";
  _startCoverageAndDump(&s->tfp, dumpfile, s->dut);
  return s;
}
""")

    val (jnaCode, className) = JNIUtils.genJNACppCode(codeBuffer.toString())
    val code = VerilatorCppHarnessGenerator.commonCodeGen(toplevel, targetDir, majorVersion, minorVersion) + jnaCode
    (code, className)
  }
}
