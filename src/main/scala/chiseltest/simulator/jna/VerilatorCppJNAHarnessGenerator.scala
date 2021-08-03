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
    def fitsIn64Bits(s: ((String, Int), Int)): Boolean = s._1._2 <= 64

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
    // std::cout << "poking: " << std::hex << u << std::endl;
    switch(id) {
""")
    pokeable.filter(fitsIn64Bits).foreach { case ((name, _), id) =>
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
    uint64_t value = 0;
    switch(id) {
""")
    peekable.filter(fitsIn64Bits).foreach { case ((name, _), id) =>
      codeBuffer.append(s"      case $id : value = dut->$name; break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      return -1;
    }
    // std::cout << "peeking: " << std::hex << value << std::endl;
    return value;
  }
  inline void poke_wide(int32_t id, int32_t offset, int64_t value) {
    const uint64_t u = value;
    WData* data = nullptr;
    size_t words = 0;
    switch(id) {
""")
    pokeable.filterNot(fitsIn64Bits).foreach { case ((name, width), id) =>
      val numWords = (width - 1) / 32 + 1
      codeBuffer.append(s"      case $id : data = dut->$name; words = $numWords; break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      break;
    }
    const size_t firstWord = offset * 2;
    const size_t secondWord = firstWord + 1;
    if(firstWord >= words || firstWord < 0) {
      std::cerr << "Out of bounds index for id = " << id << " index = " << offset << std::endl;
      finish();
    } else if(secondWord >= words) {
      data[firstWord] = u;
    } else {
      data[firstWord] = u & 0xffffffffu;
      data[secondWord] = (u >> 32) & 0xffffffffu;
    }
  }
  inline int64_t peek_wide(int32_t id, int32_t offset) {
    WData* data = nullptr;
    size_t words = 0;
    switch(id) {
""")
    peekable.filterNot(fitsIn64Bits).foreach { case ((name, width), id) =>
      val numWords = (width - 1) / 32 + 1
      codeBuffer.append(s"      case $id : data = dut->$name; words = $numWords; break;\n")
    }
    codeBuffer.append(s"""
    default:
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish();
      return -1;
    }
    const size_t firstWord = offset * 2;
    const size_t secondWord = firstWord + 1;
    if(firstWord >= words || firstWord < 0) {
      std::cerr << "Out of bounds index for id = " << id << " index = " << offset << std::endl;
      finish();
      return -1;
    } else if(secondWord >= words) {
      return (uint64_t)data[firstWord];
    } else {
      return (((uint64_t)data[secondWord]) << 32) | ((uint64_t)data[firstWord]);
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
