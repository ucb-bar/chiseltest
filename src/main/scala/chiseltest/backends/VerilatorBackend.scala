// SPDX-License-Identifier: Apache-2.0

package chiseltest.backends

import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.ir._
import firrtl.options.Viewer

import java.io.{File, FileWriter}

/** [[SimulatorBackend]] for Verilator backend.
  * Interface should be [[VPIInterface]].
  */
object VerilatorBackend extends SubprocessSimulatorBackend {

  import scala.sys.process._

  /** Verilator use CPP as harness, this function generate this harness based on [[Circuit]] */
  private def generateVerilatorCppHarness(targetDir: String, circuit: Circuit, topPorts: Seq[Port]) = {
    def pushBack(vector: String, pathName: String, width: BigInt) = width match {
      case i if i == 0  => ""
      case i if i <= 8  => s"        sim_data.$vector.push_back(new VerilatorCData(&($pathName)));"
      case i if i <= 16 => s"        sim_data.$vector.push_back(new VerilatorSData(&($pathName)));"
      case i if i <= 32 => s"        sim_data.$vector.push_back(new VerilatorIData(&($pathName)));"
      case i if i <= 64 => s"        sim_data.$vector.push_back(new VerilatorQData(&($pathName)));"
      case _            => s"        sim_data.$vector.push_back(new VerilatorWData($pathName, ${(width - 1) / 32 + 1}));"
    }
    val vcdFile = new File(targetDir, s"${circuit.main}.vcd")
    val dutName = circuit.main
    val dutApiClassName = dutName + "_api_t"
    val dutVerilatorClassName = "V" + dutName
    val verilatorVersion = "verilator --version".!!(processLogger()).split(' ').last.stripLineEnd
    val verilatorRunFlushCallback = if (verilatorVersion >= "v4.038") {
      "Verilated::runFlushCallbacks();\nVerilated::runExitCallbacks();\n"
    } else {
      "Verilated::flushCall();\n"
    }
    val emittedStuff =
      s"""#include "$dutVerilatorClassName.h"
         |#include "verilated.h"
         |#include "veri_api.h"
         |#if VM_TRACE
         |#include "verilated_vcd_c.h"
         |#endif
         |#include <iostream>
         |class $dutApiClassName: public sim_api_t<VerilatorDataWrapper*> {
         |    public:
         |    $dutApiClassName($dutVerilatorClassName* _dut) {
         |        dut = _dut;
         |        main_time = 0L;
         |        is_exit = false;
         |#if VM_TRACE
         |        tfp = NULL;
         |#endif
         |    }
         |    void init_sim_data() {
         |        sim_data.inputs.clear();
         |        sim_data.outputs.clear();
         |        sim_data.signals.clear();
         |
         |${topPorts.map {
        case Port(_, name, direction, tpe) =>
          pushBack(
            direction match {
              case Input  => "inputs"
              case Output => "outputs"
            },
            s"dut->$name",
            tpe.asInstanceOf[GroundType].width.asInstanceOf[IntWidth].width
          )
      }.mkString("\n")}
         |
         |${pushBack("signals", "dut->reset", 1)}
         |        ${s"""sim_data.signal_map["$dutName.reset"] = 0;"""}
         |    }
         |#if VM_TRACE
         |     void init_dump(VerilatedVcdC* _tfp) { tfp = _tfp; }
         |#endif
         |    inline bool exit() { return is_exit; }
         |
         |    // required for sc_time_stamp()
         |    virtual inline double get_time_stamp() {
         |        return main_time;
         |    }
         |
         |    private:
         |    $dutVerilatorClassName* dut;
         |    bool is_exit;
         |    vluint64_t main_time;
         |#if VM_TRACE
         |    VerilatedVcdC* tfp;
         |#endif
         |    virtual inline size_t put_value(VerilatorDataWrapper* &sig, uint64_t* data, bool force=false) {
         |        return sig->put_value(data);
         |    }
         |    virtual inline size_t get_value(VerilatorDataWrapper* &sig, uint64_t* data) {
         |        return sig->get_value(data);
         |    }
         |    virtual inline size_t get_chunk(VerilatorDataWrapper* &sig) {
         |        return sig->get_num_words();
         |    }
         |    virtual inline void reset() {
         |        dut->reset = 1;
         |        step();
         |    }
         |    virtual inline void start() {
         |        dut->reset = 0;
         |    }
         |    virtual inline void finish() {
         |        dut->eval();
         |        is_exit = true;
         |    }
         |    virtual inline void step() {
         |        dut->clock = 0;
         |        dut->eval();
         |#if VM_TRACE
         |        if (tfp) tfp->dump(main_time);
         |#endif
         |        main_time++;
         |        dut->clock = 1;
         |        dut->eval();
         |#if VM_TRACE
         |        if (tfp) tfp->dump(main_time);
         |#endif
         |        main_time++;
         |    }
         |    virtual inline void update() {
         |        // This seems to force a full eval of circuit, so registers with alternate clocks are update correctly
         |        dut->eval();
         |        // This was the original call, did not refresh registers when some  other clock transitioned
         |        // dut->_eval_settle(dut->__VlSymsp);
         |    }
         |};
         |
         |// The following isn't strictly required unless we emit (possibly indirectly) something
         |// requiring a time-stamp (such as an assert).
         |static $dutApiClassName * _Top_api;
         |double sc_time_stamp () { return _Top_api->get_time_stamp(); }
         |
         |// Override Verilator definition so first $$finish ends simulation
         |// Note: VL_USER_FINISH needs to be defined when compiling Verilator code
         |void vl_finish(const char* filename, int linenum, const char* hier) {
         |  $verilatorRunFlushCallback;
         |  exit(0);
         |}
         |
         |int main(int argc, char **argv, char **env) {
         |    Verilated::commandArgs(argc, argv);
         |    $dutVerilatorClassName* top = new $dutVerilatorClassName;
         |    std::string vcdfile = "${vcdFile.toString}";
         |    std::vector<std::string> args(argv+1, argv+argc);
         |    std::vector<std::string>::const_iterator it;
         |    for (it = args.begin() ; it != args.end() ; it++) {
         |        if (it->find("+waveform=") == 0) vcdfile = it->c_str()+10;
         |    }
         |#if VM_TRACE || VM_COVERAGE
         |    Verilated::traceEverOn(true);
         |#endif
         |#if VM_TRACE
         |    VL_PRINTF(\"Enabling waves..\");
         |    VerilatedVcdC* tfp = new VerilatedVcdC;
         |    top->trace(tfp, 99);
         |    tfp->open(vcdfile.c_str());
         |#endif
         |    $dutApiClassName api(top);
         |    _Top_api = &api; /* required for sc_time_stamp() */
         |    api.init_sim_data();
         |    api.init_channels();
         |#if VM_TRACE
         |    api.init_dump(tfp);
         |#endif
         |    while(!api.exit()) api.tick();
         |#if VM_TRACE
         |    if (tfp) tfp->close();
         |    delete tfp;
         |#endif
         |#if VM_COVERAGE
         |    VL_PRINTF(\"Writing Coverage..\");
         |    Verilated::mkdir("$targetDir/logs");
         |    VerilatedCov::write("$targetDir/logs/coverage.dat");
         |#endif
         |    delete top;
         |    exit(0);
         |}""".stripMargin
    val cppHarnessFileName = s"${circuit.main}-harness.cpp"
    val cppHarnessFile = new File(targetDir, cppHarnessFileName)
    val cppHarnessWriter = new FileWriter(cppHarnessFile)
    cppHarnessWriter.append(emittedStuff)
    cppHarnessWriter.close()
    cppHarnessFile.getAbsoluteFile.toString
  }

  /** Compile Verilog to executable. */
  private def compileVerilatorDut(
    targetDir:           String,
    circuit:             Circuit,
    waveForm:            Option[String],
    userSimulatorFlags:  Option[Seq[String]],
    userSimulatorCFlags: Option[Seq[String]],
    coverageAnnotations: Set[CoverageAnnotations],
    cppHarnessFile:      String
  ): Seq[String] = {
    val topName = circuit.main
    val writeVcdFlag: Seq[String] = waveForm match {
      case Some("vcd") => Seq("--trace")
      case _           => Seq.empty
    }
    val coverageFlags: Seq[String] = coverageAnnotations.collect {
      case LineCoverageAnnotation       => Set("--coverage-line")
      case ToggleCoverageAnnotation     => Set("--coverage-toggle")
      case UserCoverageAnnotation       => Set("--coverage-user")
      case StructuralCoverageAnnotation => Set("--coverage-line", "--coverage-toggle")
      /* unsupported coverages by verilator. */
      case BranchCoverageAnnotation      => Set.empty[String]
      case ConditionalCoverageAnnotation => Set.empty[String]
    }.flatten.toSeq
    val coverageCFlags =
      if (
        coverageAnnotations
          .intersect(Set(LineCoverageAnnotation, ToggleCoverageAnnotation, UserCoverageAnnotation))
          .nonEmpty
      ) {
        Seq("-DSP_COVERAGE_ENABLE")
      } else { Seq() }
    val blackBoxVerilogListFile = new File(targetDir, firrtl.transforms.BlackBoxSourceHelper.defaultFileListName)
    val blackBoxVerilogListFlag = if (blackBoxVerilogListFile.exists()) {
      Seq("-f", blackBoxVerilogListFile.getAbsolutePath)
    } else {
      Seq.empty[String]
    }
    val verilatorCFlags = Seq(
      "-Wno-undefined-bool-conversion",
      "-O1",
      s"-DTOP_TYPE=V$topName",
      "-DVL_USER_FINISH",
      s"-include V$topName.h",
      s"-I${getClass.getResource("/vpi/").getPath}"
    ) ++ coverageCFlags ++ userSimulatorCFlags.getOrElse(Seq.empty)
    val verilatorFlags = Seq(
      "--assert",
      "-Wno-fatal",
      "-Wno-WIDTH",
      "-Wno-STMTDLY",
      "-O1",
      "--top-module",
      topName,
      s"+define+TOP_TYPE=V$topName",
      s"+define+PRINTF_COND=!$topName.reset",
      s"+define+STOP_COND=!$topName.reset",
      "-CFLAGS",
      s""""${verilatorCFlags.mkString(" ")}"""",
      "-Mdir",
      targetDir,
      "--exe",
      cppHarnessFile
    ) ++ blackBoxVerilogListFlag ++ writeVcdFlag ++ coverageFlags ++ userSimulatorFlags.getOrElse(Seq.empty)
    val compileToCppCommand = s"cd $targetDir && verilator --cc $topName.v ${verilatorFlags.mkString(" ")}"
    val compileToBinCommand = s"make -C $targetDir -j -f V$topName.mk V$topName"
    logger.warn(s"compiling verilog to c++: $compileToCppCommand")
    assert(
      Seq("bash", "-c", compileToCppCommand).!(processLogger()) == 0,
      s"verilator command failed on circuit $topName in work dir $targetDir: \n$compileToCppCommand"
    )
    logger.warn(s"compiling c++ to binary: $compileToBinCommand")

    assert(
      Seq("bash", "-c", compileToBinCommand).!(processLogger()) == 0,
      s"Compilation of verilator generated code failed for circuit $topName in work dir $targetDir"
    )

    Seq(new File(targetDir, s"V${circuit.main}").toString)
  }

  def compileDut(annotations: AnnotationSeq): AnnotationSeq = {
    val options = Viewer[ChiselTestOptions].view(annotations)
    annotations :+ SimulatorInterfaceAnnotation(
      new VPIInterface(
        options.topPorts.get,
        options.topName.get,
        options.commands.getOrElse(
          compileVerilatorDut(
            options.targetDir.get,
            options.circuit.get,
            options.waveForm,
            options.simulatorFlags,
            options.simulatorCFlags,
            options.coverageAnnotations,
            generateVerilatorCppHarness(
              options.targetDir.get,
              options.circuit.get,
              options.topPorts.get
            )
          )
        )
      )
    )
  }
}
