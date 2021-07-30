// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.ipc

import chiseltest.simulator.TopmoduleInfo

/** Generates the Module specific verilator harness cpp file for verilator compilation */
private[chiseltest] object VerilatorCppHarnessGenerator {
  // generated code that can be re-used for the JNI based interface
  def commonCodeGen(toplevel: TopmoduleInfo, targetDir: os.Path, majorVersion: Int, minorVersion: Int): String = {
    val dutName = toplevel.name
    val dutVerilatorClassName = "V" + dutName

    require(toplevel.clocks.length <= 1, "Multi clock circuits are currently not supported!")
    val clockName = toplevel.clocks.headOption
    val clockLow = clockName.map("top->" + _ + " = 0;").getOrElse("")
    val clockHigh = clockName.map("top->" + _ + " = 1;").getOrElse("")

    val coverageInit =
      if (majorVersion >= 4 && minorVersion >= 202)
        """|#if VM_COVERAGE
           |    Verilated::defaultContextp()->coveragep()->forcePerInstance(true);
           |#endif
           |""".stripMargin
      else ""

    val verilatorRunFlushCallback = if (majorVersion >= 4 && minorVersion >= 38) {
      "Verilated::runFlushCallbacks();\n  Verilated::runExitCallbacks();"
    } else {
      "Verilated::flushCall();"
    }

    val codeBuffer = new StringBuilder
    codeBuffer.append(s"""#include "$dutVerilatorClassName.h"
                         |#include "verilated.h"
                         |#include "veri_api.h"
                         |
                         |#define TOP_CLASS $dutVerilatorClassName
                         |
                         |#ifndef VM_TRACE_FST
                         |#define VM_TRACE_FST 0
                         |#endif
                         |
                         |#if VM_TRACE
                         |#if VM_TRACE_FST
                         |  #include "verilated_fst_c.h"
                         |  #define VERILATED_C VerilatedFstC
                         |#else // !(VM_TRACE_FST)
                         |  #include "verilated_vcd_c.h"
                         |  #define VERILATED_C VerilatedVcdC
                         |#endif
                         |#else // !(VM_TRACE)
                         |  #define VERILATED_C VerilatedVcdC
                         |#endif
                         |#include <iostream>
                         |
                         |
                         |// Override Verilator definition so first $$finish ends simulation
                         |// Note: VL_USER_FINISH needs to be defined when compiling Verilator code
                         |void vl_finish(const char* filename, int linenum, const char* hier) {
                         |  $verilatorRunFlushCallback
                         |  exit(0);
                         |}
                         |
                         |static void _startCoverageAndDump(VERILATED_C** tfp, const std::string& dumpfile, TOP_CLASS* top) {
                         |$coverageInit
                         |#if VM_TRACE || VM_COVERAGE
                         |    Verilated::traceEverOn(true);
                         |#endif
                         |#if VM_TRACE
                         |    VL_PRINTF(\"Enabling waves..\\n\");
                         |    *tfp = new VERILATED_C;
                         |    top->trace(*tfp, 99);
                         |    (*tfp)->open(dumpfile.c_str());
                         |#endif
                         |}
                         |
                         |static void _step(VERILATED_C* tfp, TOP_CLASS* top, vluint64_t& main_time) {
                         |    $clockLow
                         |    top->eval();
                         |#if VM_TRACE
                         |    if (tfp) tfp->dump(main_time);
                         |#endif
                         |    main_time++;
                         |    $clockHigh
                         |    top->eval();
                         |#if VM_TRACE
                         |    if (tfp) tfp->dump(main_time);
                         |#endif
                         |    main_time++;
                         |}
                         |
                         |static void _finish(VERILATED_C* tfp, TOP_CLASS* top) {
                         |#if VM_TRACE
                         |  if (tfp) tfp->close();
                         |  delete tfp;
                         |#endif
                         |#if VM_COVERAGE
                         |  VerilatedCov::write("$targetDir/coverage.dat");
                         |#endif
                         |  // TODO: re-enable!
                         |  // delete top;
                         |}
                         |""".stripMargin)

    codeBuffer.toString()
  }

  def codeGen(
    toplevel:     TopmoduleInfo,
    dumpFilePath: os.Path,
    targetDir:    os.Path,
    majorVersion: Int,
    minorVersion: Int
  ): String = {

    val codeBuffer = new StringBuilder

    def pushBack(vector: String, pathName: String, width: BigInt): Unit = {
      if (width == 0) {
        // Do nothing- 0 width wires are removed
      } else if (width <= 8) {
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorCData(&($pathName)));\n"
        )
      } else if (width <= 16) {
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorSData(&($pathName)));\n"
        )
      } else if (width <= 32) {
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorIData(&($pathName)));\n"
        )
      } else if (width <= 64) {
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorQData(&($pathName)));\n"
        )
      } else {
        val numWords = (width - 1) / 32 + 1
        codeBuffer.append(
          s"        sim_data.$vector.push_back(new VerilatorWData($pathName, $numWords));\n"
        )
      }
    }

    val dutName = toplevel.name
    val dutApiClassName = dutName + "_api_t"
    val dutVerilatorClassName = "V" + dutName

    codeBuffer.append(s"""
class $dutApiClassName: public sim_api_t<VerilatorDataWrapper*> {
    public:
    $dutApiClassName(TOP_CLASS* _dut) {
        dut = _dut;
        main_time = 0L;
        is_exit = false;
        tfp = nullptr;
    }
    void init_sim_data() {
        sim_data.inputs.clear();
        sim_data.outputs.clear();
        sim_data.signals.clear();

""")
    toplevel.inputs.foreach { case (name, width) =>
      // replaceFirst used here in case port name contains the dutName
      pushBack("inputs", "dut->" + name, width)
    }
    toplevel.outputs.foreach { case (name, width) =>
      // replaceFirst used here in case port name contains the dutName
      pushBack("outputs", "dut->" + name, width)
    }
    codeBuffer.append(
      s"""
    }
    void init_dump(VERILATED_C* _tfp) { tfp = _tfp; }
    inline bool exit() { return is_exit; }

    // required for sc_time_stamp()
    virtual inline double get_time_stamp() {
        return main_time;
    }

    private:
    $dutVerilatorClassName* dut;
    bool is_exit;
    vluint64_t main_time;
    VERILATED_C* tfp;
    virtual inline size_t put_value(VerilatorDataWrapper* &sig, uint64_t* data, bool force=false) {
        return sig->put_value(data);
    }
    virtual inline size_t get_value(VerilatorDataWrapper* &sig, uint64_t* data) {
        return sig->get_value(data);
    }
    virtual inline size_t get_chunk(VerilatorDataWrapper* &sig) {
        return sig->get_num_words();
    }
    virtual inline void start() {
    }
    virtual inline void finish() {
        dut->eval();
        is_exit = true;
    }
    virtual inline void step() {
        _step(tfp, dut, main_time);
    }
    virtual inline void update() {
        dut->eval();
    }
};

// The following isn't strictly required unless we emit (possibly indirectly) something
// requiring a time-stamp (such as an assert).
static $dutApiClassName * _Top_api;
double sc_time_stamp () { return _Top_api->get_time_stamp(); }

int main(int argc, char **argv, char **env) {
    Verilated::commandArgs(argc, argv);
    TOP_CLASS* top = new TOP_CLASS;
    std::string dumpfile = "$dumpFilePath";
    std::vector<std::string> args(argv+1, argv+argc);
    std::vector<std::string>::const_iterator it;
    for (it = args.begin() ; it != args.end() ; it++) {
        if (it->find("+waveform=") == 0) dumpfile = it->c_str()+10;
    }
    VERILATED_C* ftp = nullptr;
    _startCoverageAndDump(&ftp, dumpfile, top);

    $dutApiClassName api(top);
    _Top_api = &api; /* required for sc_time_stamp() */
    api.init_sim_data();
    api.init_channels();
#if VM_TRACE
    api.init_dump(tfp);
#endif
    while(!api.exit()) api.tick();

    _finish(ftp, top);
    exit(0);
}
"""
    )
    commonCodeGen(toplevel, targetDir, majorVersion, minorVersion) + codeBuffer.toString()
  }
}
