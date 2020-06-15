package chiseltest.stage.phases

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}
import java.security.MessageDigest

import chisel3.MultiIOModule
import chiseltest.backends.{TreadleBackend, VPIBackend}
import chiseltest.internal.ThreadedBackendAnnotation
import chiseltest.stage._
import firrtl.AnnotationSeq
import firrtl.ir.{Circuit, GroundType, Input, IntWidth, Output, Port}
import firrtl.options.{Dependency, Phase, PreservesAll, TargetDirAnnotation}
import firrtl.stage.FirrtlCircuitAnnotation
import treadle.{TreadleTester, TreadleTesterAnnotation}
import treadle.stage.phases.PrepareAstFromLowFIRRTL

import scala.sys.process._


/** [[CompileDut]] is to prepare executable circuit for different simulators.
  * currently, in treadle backend:
  * it will execute [[PrepareAstFromLowFIRRTL]] to [[treadle.utils.FixupOps]] and [[treadle.utils.AugmentPrintf]]
  * in verilator backend:
  * if user has already define [[CommandAnnotation]], annotations will be directly passed to simulator executor.
  * if circuit md5 is different with the md5 file in the [[TargetDirAnnotation]],
  * it will generate verilator cpp harness based, and compile a dut.
  * */
class CompileDut extends Phase with ChiselTesterAnnotationHelper with PreservesAll[Phase] {
  override def prerequisites: Seq[Dependency[Phase]] = Seq(
    Dependency[AddDefaults],
    Dependency[AnalysisCircuit]
  )

  private def pushBack(vector: String, pathName: String, width: BigInt) = width match {
    case i if i == 0 => ""
    case i if i <= 8 => s"        sim_data.$vector.push_back(new VerilatorCData(&($pathName)));"
    case i if i <= 16 => s"        sim_data.$vector.push_back(new VerilatorSData(&($pathName)));"
    case i if i <= 32 => s"        sim_data.$vector.push_back(new VerilatorIData(&($pathName)));"
    case i if i <= 64 => s"        sim_data.$vector.push_back(new VerilatorQData(&($pathName)));"
    case _ => s"        sim_data.$vector.push_back(new VerilatorWData($pathName, ${(width - 1) / 32 + 1}));"
  }

  /** @todo refactor this with unified VPI. */
  private def generateVerilatorCppHarness(annos: AnnotationSeq) = {
    val targetDir = getTargetDir(annos)
    val circuit = getCircuit(annos)
    val vcdFile = new File(targetDir, s"${circuit.main}.vcd")
    val ports = getTopPorts(annos)
    val dutName = circuit.main
    val dutApiClassName = dutName + "_api_t"
    val dutVerilatorClassName = "V" + dutName
    val verilatorVersion = "verilator --version".!!.split(' ').last.stripLineEnd
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
    CppHarnessFileAnnotaion(cppHarnessFile.getAbsoluteFile.toString)
  }

  private def hasCache(annos: AnnotationSeq) = {
    if (getEnableCache(annos)) {
      val circuit = getCircuit(annos)
      val targetDir = getTargetDir(annos)
      val hash = MessageDigest.getInstance("MD5").digest(circuit.serialize.getBytes).map(0xFF & _).map("%02x".format(_)).mkString
      val hashFile = new File(targetDir, s"${circuit.main}-hash.md5")
      val oldHash = if (hashFile.canRead) {
        val f = scala.io.Source.fromFile(hashFile)
        val r = f.mkString
        r
      } else ""
      // write new to file.
      val hashWriter = new PrintWriter(new FileOutputStream(hashFile, false))
      hashWriter.append(hash)
      hashWriter.close()
      oldHash == hash
    } else false
  }

  private def compileVerilatorDut(annos: AnnotationSeq) = {
    val circuit: Circuit = getCircuit(annos)
    val topName = circuit.main
    val targetDir: String = getTargetDir(annos)
    val simulatorBinary = getSimulatorBinary(annos)
    val writeVcdFlag: Seq[String] = if (getWaveform(annos) != "none") Seq("--trace") else Seq.empty
    val userSimulatorFlags: Seq[String] = annos.collectFirst { case SimulatorFlagsAnnotation(f) => f }.getOrElse(Seq.empty)
    val userSimulatorCFlags: Seq[String] = annos.collectFirst { case SimulatorCFlagsAnnotation(f) => f }.getOrElse(Seq.empty)
    val cppHarnessFile: String = annos.collectFirst { case CppHarnessFileAnnotaion(f) => f }.get
    val blackBoxVerilogListFile = new File(targetDir, firrtl.transforms.BlackBoxSourceHelper.defaultFileListName)
    val blackBoxVerilogListFlag = if (blackBoxVerilogListFile.exists()) {
      Seq("-f", blackBoxVerilogListFile.getAbsolutePath)
    } else {
      Seq.empty[String]
    }
    val verilatorCFlags = Seq(
      "-Wno-undefined-bool-conversion", "-O1", s"-DTOP_TYPE=V$topName", "-DVL_USER_FINISH",
      s"-include V$topName.h", s"-I${getClass.getResource("/verilator/").getPath}"
    ) ++ userSimulatorCFlags
    val verilatorFlags = Seq(
      "--assert", "-Wno-fatal", "-Wno-WIDTH", "-Wno-STMTDLY", "-O1",
      "--top-module", topName,
      s"+define+TOP_TYPE=V$topName", s"+define+PRINTF_COND=!$topName.reset", s"+define+STOP_COND=!$topName.reset",
      "-CFLAGS", s""""${verilatorCFlags mkString " "}"""",
      "-Mdir", targetDir,
      "--exe", cppHarnessFile
    ) ++ blackBoxVerilogListFlag ++ writeVcdFlag ++ userSimulatorFlags
    val generatedCommand = s"cd $targetDir && $simulatorBinary --cc $topName.v ${verilatorFlags.mkString(" ")}"
    assert(
      Seq("bash", "-c", generatedCommand).! == 0,
      s"verilator command failed on circuit $topName in work dir $targetDir: \n$generatedCommand"
    )
    assert(
      Seq("make", "-C", targetDir, "-j", "-f", s"V$topName.mk", s"V$topName").! == 0,
      s"Compilation of verilator generated code failed for circuit $topName in work dir $targetDir"
    )
  }

  def transform(a: AnnotationSeq): AnnotationSeq = {
    // if user define his own Command to run

    getSimulatorBackend(a) match {
      case "treadle" =>
        val treadleAnnotations = PrepareAstFromLowFIRRTL.transform(a)
        val treadleTesterAnnotations = TreadleTesterAnnotation(new TreadleTester(treadleAnnotations))
        val annos = treadleAnnotations :+ treadleTesterAnnotations
        annos :+ ThreadedBackendAnnotation(new TreadleBackend[MultiIOModule](annos))
      case "verilator" =>
        val command = getCommand(a).getOrElse {
          if (!hasCache(a)) {
            val verilatorCppHarness = a.collectFirst { case f: CppHarnessFileAnnotaion => f }.getOrElse {
              generateVerilatorCppHarness(a)
            }
            compileVerilatorDut(a :+ verilatorCppHarness)
          }
          Seq(new File(getTargetDir(a), s"V${getCircuit(a).main}").toString)
        }
        val annos = a :+ CommandAnnotation(command)
        annos :+ ThreadedBackendAnnotation(new VPIBackend[MultiIOModule](annos))
      case "vcs" => a
    }
  }
}