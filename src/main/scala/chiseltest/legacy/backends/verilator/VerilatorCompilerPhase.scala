package chiseltest.stage

import java.io.{File, FileWriter, IOException}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{FileAlreadyExistsException, Files, Paths}

import chiseltest.internal.WriteVcdAnnotation
import chiseltest.legacy.backends.verilator.verilogToVerilator.{constructCSimulatorCommand, editCSimulatorCommand}
import chiseltest.legacy.backends.verilator.{CommandEditor, CommandEditsFile, TestCommandOverride, VerilatorCFlags, VerilatorFlags, verilogToVerilator}
import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.ir._
import firrtl.options.{Phase, PreservesAll, TargetDirAnnotation}
import firrtl.stage.FirrtlCircuitAnnotation

import scala.sys.process.{ProcessBuilder, ProcessLogger, _}

case class CommandAnnotation(value: Seq[String]) extends NoTargetAnnotation

case class SimulatorHFileDictionary(file: String) extends NoTargetAnnotation

case class VerilatorBinaryPath(file: String) extends NoTargetAnnotation

case class CppHarnessFile(file: String) extends NoTargetAnnotation

class Prepare extends Phase with PreservesAll[Phase] {
  def transform(a: AnnotationSeq): AnnotationSeq = {
    a :+ a.collectFirst { case f: SimulatorHFileDictionary => f }.getOrElse {
      SimulatorHFileDictionary(getClass.getResource("/chisel3/tester/legacy/backends/verilator/").getPath)
    } :+ a.collectFirst { case f: VerilatorBinaryPath => f }.getOrElse {
      VerilatorBinaryPath("verilator")
    }
  }
}

class GenerateCppHarnessFile extends Phase with PreservesAll[Phase] {
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

  def getPorts(circuit: Circuit) = circuit.modules.collectFirst { case Module(_, name, ports, _) if name == circuit.main => ports }.get

  def transform(a: AnnotationSeq): AnnotationSeq = {
    a :+ a.collectFirst { case f: CppHarnessFile => f }.getOrElse {
      val targetDir = a.collectFirst { case TargetDirAnnotation(t) => t }.get
      val circuit = a.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
      val vcdFile = new File(targetDir, s"${circuit.main}.vcd")
      val ports = getPorts(circuit)
      val dutName = circuit.main
      val dutApiClassName = dutName + "_api_t"
      val dutVerilatorClassName = "V" + dutName
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
           |    std::string vcdfile = "${vcdFile.toString}";
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
      val cppHarnessFileName = s"${circuit.main}-harness.cpp"
      val cppHarnessFile = new File(targetDir, cppHarnessFileName)
      val cppHarnessWriter = new FileWriter(cppHarnessFile)
      cppHarnessWriter.append(emittedStuff)
      cppHarnessWriter.close()
      CppHarnessFile(cppHarnessFile.getAbsoluteFile.toString)
    }
  }
}

class VerilatorCompile extends Phase with PreservesAll[Phase] {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    val targetDir: String = new File(a.collectFirst { case TargetDirAnnotation(t) => t }.get).getAbsolutePath
    val circuit: Circuit = a.collectFirst { case FirrtlCircuitAnnotation(c) => c }.get
    val topName = circuit.main
    val verilatorBinary = a.collectFirst { case VerilatorBinaryPath(p) => p }.get
    val writeVcdFlag: Seq[String] = if (a.contains(WriteVcdAnnotation)) Seq("--trace") else Seq.empty
    val userVerilatorFlags: Seq[String] = a.collectFirst { case VerilatorFlags(f) => f }.getOrElse(Seq.empty)
    val moreVerilatorCFlags: Seq[String] = a.collectFirst { case VerilatorCFlags(f) => f }.getOrElse(Seq.empty)
    val commandEditsFile: String = a.collectFirst { case CommandEditsFile(f) => f }.getOrElse("")
    val cppHarnessFile: String = a.collectFirst { case CppHarnessFile(f) => f }.get
    val simulatorHFileDirectory: String = a.collectFirst { case SimulatorHFileDictionary(f) => f }.get
    val blackBoxVerilogListFile = new File(targetDir, firrtl.transforms.BlackBoxSourceHelper.defaultFileListName)
    val blackBoxVerilogListFlag = if (blackBoxVerilogListFile.exists()) {
      Seq("-f", blackBoxVerilogListFile.getAbsolutePath)
    } else {
      Seq.empty[String]
    }
    val verilatorCFlags = Seq(
      "-Wno-undefined-bool-conversion", "-O1", s"-DTOP_TYPE=V$topName", "-DVL_USER_FINISH",
      s"-include V$topName.h", s"-I${simulatorHFileDirectory}"
    ) ++ moreVerilatorCFlags
    val verilatorFlags = Seq(
      "--assert", "-Wno-fatal", "-Wno-WIDTH", "-Wno-STMTDLY", "-O1",
      "--top-module", topName,
      s"+define+TOP_TYPE=V$topName", s"+define+PRINTF_COND=!$topName.reset", s"+define+STOP_COND=!$topName.reset",
      "-CFLAGS", s""""${verilatorCFlags mkString " "}"""",
      "-Mdir", targetDir,
      "--exe", cppHarnessFile
    ) ++ blackBoxVerilogListFlag ++ writeVcdFlag ++ userVerilatorFlags

    val generatedCommand = s"cd $targetDir && $verilatorBinary --cc $topName.v ${verilatorFlags.mkString(" ")}"
    /** @todo new phase here. */
    val commandEditorPrefix = "verilator-command-edit"
    val commandEditor = CommandEditor(commandEditsFile, commandEditorPrefix)
    val editedCommand = commandEditor(generatedCommand)
    /** @todo new phase here. */
    assert(
      Seq("bash", "-c", editedCommand).! == 0,
      s"verilator command failed on circuit $topName in work dir $targetDir: \n$editedCommand"
    )
    assert(
      Seq("make", "-C", targetDir, "-j", "-f", s"V$topName.mk", s"V$topName").! == 0,
      s"Compilation of verilator generated code failed for circuit $topName in work dir $targetDir"
    )
    val command = a.collectFirst[Seq[String]] { case TestCommandOverride(f) => f.split(" +") }.getOrElse(Seq(new File(targetDir, s"V$topName").toString))
    a ++ Seq(CommandAnnotation(command))
  }
}

class VerilatorCompiler extends Phase {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    Seq(
      new Prepare,
      new GenerateCppHarnessFile,
      new VerilatorCompile
    ).foldLeft(a) { case (a, t) => t.transform(a) }
  }
}