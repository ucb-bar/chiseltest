// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jni

import chiseltest.simulator.TopmoduleInfo
import chiseltest.simulator.ipc.VerilatorCppHarnessGenerator

/** Generates the Module specific verilator harness cpp file for verilator compilation.
  *  This version generates a harness that can be called into through the JNI.
  */
private[chiseltest] object VerilatorCppJNIHarnessGenerator {
  def codeGen(
    toplevel:     TopmoduleInfo,
    vcdFilePath:  os.Path,
    targetDir:    os.Path,
    majorVersion: Int,
    minorVersion: Int
  ): (String, String) = {
    val codeBuffer = new StringBuilder

    def pushBack(vector: String, pathName: String, width: BigInt): Unit = {
      if (width == 0) {
        // Do nothing- 0 width wires are removed
      } else if (width <= 8) {
        codeBuffer.append(s"  $vector.push_back(new VerilatorCData(&($pathName)));\n")
      } else if (width <= 16) {
        codeBuffer.append(s"  $vector.push_back(new VerilatorSData(&($pathName)));\n")
      } else if (width <= 32) {
        codeBuffer.append(s"  $vector.push_back(new VerilatorIData(&($pathName)));\n")
      } else if (width <= 64) {
        codeBuffer.append(s"  $vector.push_back(new VerilatorQData(&($pathName)));\n")
      } else {
        val numWords = (width - 1) / 32 + 1
        codeBuffer.append(s"  $vector.push_back(new VerilatorWData($pathName, $numWords));\n")
      }
    }

    // generate Verilator specific "sim_state" class
    codeBuffer.append(s"""
struct sim_state {
  TOP_CLASS* dut;
  VERILATED_C* tfp;
  vluint64_t main_time;
  std::vector<VerilatorDataWrapper*> signals;

  sim_state() :
    dut(new TOP_CLASS),
    tfp(nullptr),
    main_time(0)
  {
    // std::cout << "Allocating! " << ((long long) dut) << std::endl;
  }

  inline void step(JNIEnv* /*env*/) { _step(tfp, dut, main_time); }
  inline void update(JNIEnv* /*env*/) { dut->eval(); }
  inline void finish(JNIEnv* /*env*/) {
    dut->eval();
    _finish(tfp, dut);
  }
  inline void resetCoverage(JNIEnv* /*env*/) { VerilatedCov::zero(); }
  inline void writeCoverage(JNIEnv* env, jstring filename) {
    const char *c_str = env->GetStringUTFChars(filename, NULL);
    VerilatedCov::write(c_str);
  }
  inline void poke(JNIEnv* env, jint id, jlong value) {
    VerilatorDataWrapper *sig = signals[id];
    if (!sig) {
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish(env);
      // TODO what?
    } else {
      // std::cout << "Poking signal " << id << " with value " << value << std::endl;
    }
    uint64_t toput = value;
    sig->put_value(&toput);
  }
  inline jlong peek(JNIEnv* env, jint id) {
    VerilatorDataWrapper *sig = signals[id];
    if (!sig) {
      std::cerr << "Cannot find the object of id = " << id << std::endl;
      finish(env);
      // TODO what?
    } else {
      // std::cout << "Peeking signal " << id << std::endl;
    }
    uint64_t toret;
    sig->get_value(&toret);
    return toret;
  }
};

static sim_state* create_sim_state() {
  sim_state *s = new sim_state();
  std::string dumpfile = "${vcdFilePath}";

  _startCoverageAndDump(&s->tfp, dumpfile, s->dut);
  s->signals.reserve(${toplevel.inputs.length + toplevel.outputs.length});
""")
    (toplevel.inputs ++ toplevel.outputs).foreach { case (name, width) =>
      pushBack("s->signals", "s->dut->" + name, width)
    }
    codeBuffer.append(s"""
  return s;
}
""")

    val (jniCode, className) = JNIUtils.genJNICppCode(codeBuffer.toString())
    val code = VerilatorCppHarnessGenerator.commonCodeGen(toplevel, targetDir, majorVersion, minorVersion) + jniCode
    (code, className)
  }
}
