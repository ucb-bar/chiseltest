package chiseltest.stage

import java.io.{File, FileWriter, IOException}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{FileAlreadyExistsException, Files, Paths}

import chiseltest.internal.WriteVcdAnnotation
import chiseltest.legacy.backends.verilator.{CommandEditsFile, TestCommandOverride, VerilatorCFlags, VerilatorFlags, verilogToVerilator}
import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.{Phase, TargetDirAnnotation}
import firrtl.stage.FirrtlCircuitAnnotation

import scala.sys.process.{ProcessBuilder, ProcessLogger, _}

case class CommandAnnotation(value: Seq[String]) extends NoTargetAnnotation

class VerilatorCompilerPhase extends Phase {
  /** @todo should this init as a new Phase. */
  def copyVerilatorHeaderFiles(destinationDirPath: String) = {
    new File(destinationDirPath).mkdirs()
    val simApiHFilePath = Paths.get(destinationDirPath + "/sim_api.h")
    val verilatorApiHFilePath = Paths.get(destinationDirPath + "/veri_api.h")
    try {
      Files.createFile(simApiHFilePath)
      Files.createFile(verilatorApiHFilePath)
    } catch {
      case _: FileAlreadyExistsException =>
        System.out.format("")
      case x: IOException =>
        System.err.format("createFile error: %s%n", x)
    }
    Files.copy(
      getClass.getResourceAsStream("/chisel3/tester/legacy/backends/verilator/sim_api.h"),
      simApiHFilePath,
      REPLACE_EXISTING
    )
    Files.copy(
      getClass.getResourceAsStream("/chisel3/tester/legacy/backends/verilator/veri_api.h"),
      verilatorApiHFilePath,
      REPLACE_EXISTING
    )
  }

  /** @todo should this init as a new Phase. */
  def generateCppHarnessFile(destinationDirPath: String, circuit: Circuit) = {
    val cppHarnessFileName = s"${circuit.main}-harness.cpp"
    val cppHarnessFile = new File(destinationDirPath, cppHarnessFileName)
    val cppHarnessWriter = new FileWriter(cppHarnessFile)
    val vcdFile = new File(destinationDirPath, s"${circuit.main}.vcd")
    val emittedStuff = codeGen(circuit, vcdFile.toString)
    cppHarnessWriter.append(emittedStuff)
    cppHarnessWriter.close()
    cppHarnessFile
  }

  /** @todo should this init as a new Phase. */
  def codeGen(circuit: Circuit, vcdFilePath: String): String = {
    def pushBack(vector: String, pathName: String, width: BigInt) = width match {
      case i if i == 0 => ""
      case i if i <= 8 => s"        sim_data.$vector.push_back(new VerilatorCData(&($pathName)));"
      case i if i <= 16 => s"        sim_data.$vector.push_back(new VerilatorSData(&($pathName)));"
      case i if i <= 32 => s"        sim_data.$vector.push_back(new VerilatorIData(&($pathName)));"
      case i if i <= 64 => s"        sim_data.$vector.push_back(new VerilatorQData(&($pathName)));"
      case _ =>
        val numWords = (width - 1) / 32 + 1
        s"        sim_data.$vector.push_back(new VerilatorWData($pathName, $numWords));"
    }

    val ports = getPorts(circuit)
    val dutName = circuit.main
    val dutApiClassName = dutName + "_api_t"
    val dutVerilatorClassName = "V" + dutName
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
       |${
      (ports map { case Port(_, name, direction, tpe) => pushBack(direction match {
        case Input => "inputs"
        case Output => "outputs"
      }, s"dut->$name", tpe.asInstanceOf[GroundType].width.asInstanceOf[IntWidth].width)
      }).reduce(_ + "\n" + _)
    }
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
       |  Verilated::flushCall();
       |  exit(0);
       |}
       |
       |int main(int argc, char **argv, char **env) {
       |    Verilated::commandArgs(argc, argv);
       |    $dutVerilatorClassName* top = new $dutVerilatorClassName;
       |    std::string vcdfile = "$vcdFilePath";
       |    std::vector<std::string> args(argv+1, argv+argc);
       |    std::vector<std::string>::const_iterator it;
       |    for (it = args.begin() ; it != args.end() ; it++) {
       |        if (it->find("+waveform=") == 0) vcdfile = it->c_str()+10;
       |    }
       |#if VM_TRACE
       |    Verilated::traceEverOn(true);
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
       |    delete top;
       |    exit(0);
       |}""".stripMargin
  }

  def getPorts(circuit: Circuit) = circuit.modules.collectFirst { case Module(_, name, ports, _) if name == circuit.main => ports }.get

  override def transform(a: AnnotationSeq): AnnotationSeq = {
    val targetDir = a.collectFirst { case TargetDirAnnotation(t) => t }.get
    val circuit = a.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    val topName = circuit.main
    copyVerilatorHeaderFiles(targetDir)
    val cppHarnessFile = generateCppHarnessFile(targetDir, circuit)

    val moreVerilatorFlags = a.collectFirst { case VerilatorFlags(f) => f }.getOrElse(Seq.empty)

    val moreVerilatorCFlags = a.collectFirst { case VerilatorCFlags(f) => f }.getOrElse(Seq.empty)

    val writeVcdFlag = if (a.contains(WriteVcdAnnotation)) {
      Seq("--trace")
    } else {
      Seq()
    }
    val commandEditsFile = a.collectFirst { case CommandEditsFile(f) => f }.getOrElse("")

    val verilatorFlags = moreVerilatorFlags ++ writeVcdFlag
    assert(
      verilogToVerilator(
        topName,
        new File(targetDir),
        cppHarnessFile,
        moreVerilatorFlags = verilatorFlags,
        moreVerilatorCFlags = moreVerilatorCFlags,
        editCommands = commandEditsFile
      ).! == 0,
      s"verilator command failed on circuit $topName in work dir $targetDir"
    )
    assert(
      Seq("make", "-C", targetDir, "-j", "-f", s"V$topName.mk", s"V$topName").! == 0,
      s"Compilation of verilator generated code failed for circuit $topName in work dir $targetDir"
    )
    val command = a.collectFirst[Seq[String]] {
      case TestCommandOverride(f) => f.split(" +")
    }.getOrElse {
      Seq(new File(targetDir, s"V$topName").toString)
    }
    a ++ Seq(CommandAnnotation(command))
  }
}