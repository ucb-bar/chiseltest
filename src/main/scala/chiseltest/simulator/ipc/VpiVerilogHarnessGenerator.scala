// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.ipc

import chiseltest.simulator.{PinInfo, TopmoduleInfo}

/** Generates the Module specific Verilog harness file for a VPI based interface */
private[chiseltest] object VpiVerilogHarnessGenerator {
  def codeGen(
    toplevel:    TopmoduleInfo,
    moduleNames: Seq[String],
    useVpdDump:  Boolean = false,
    useFsdbDump: Boolean = false
  ): String = {
    val testbenchName = firrtl.Namespace(moduleNames).newName("testbench")

    val dutName = toplevel.name
    val namespace = firrtl.Namespace(toplevel.portNames)

    toplevel.requireNoMultiClock()
    val clockName = toplevel.clocks.headOption.getOrElse(namespace.newName("clock"))
    val dumpFileVar = namespace.newName("dumpfile")
    val dumpOnVar = namespace.newName("dumpon")

    val codeBuffer = new StringBuilder
    codeBuffer.append(s"module $testbenchName;\n")
    codeBuffer.append(s"  reg $clockName = 0;\n")
    toplevel.inputs.foreach { case PinInfo(name, width, _) =>
      codeBuffer.append(s"  reg[${width - 1}:0] $name = 0;\n")
    }
    toplevel.outputs.foreach { case PinInfo(name, width, _) =>
      codeBuffer.append(s"  wire[${width - 1}:0] $name;\n")
    }

    codeBuffer.append(s"  initial begin\n")
    codeBuffer.append(s"    #`CLOCK_PERIOD; // Delay first clock edge to avoid race condition with randomization\n")
    codeBuffer.append(s"    forever begin\n")
    codeBuffer.append(s"      #`CLOCK_PERIOD $clockName = ~$clockName;\n")
    codeBuffer.append(s"    end\n")
    codeBuffer.append(s"  end\n")
    codeBuffer.append(s"  reg [4095:0] $dumpFileVar = 0;\n")
    codeBuffer.append(s"  reg $dumpOnVar = 0;\n") // this is a hack to exclude the first half-cycle from the wave dump

    codeBuffer.append("\n  /*** DUT instantiation ***/\n")
    codeBuffer.append(s"  $dutName $dutName(\n")
    codeBuffer.append(toplevel.clocks.map(c => s"    .$c($c),\n").mkString(""))
    val ioNames = (toplevel.inputs ++ toplevel.outputs).map(_.name)
    codeBuffer.append(ioNames.map(name => s"    .$name($name)").mkString(",\n"))
    codeBuffer.append("  );\n\n")

    codeBuffer.append("  initial begin\n")
    val inputNames = toplevel.inputs.map(_.name)
    codeBuffer.append(s"    $$init_ins(${inputNames.mkString(", ")});\n")
    val outputNames = toplevel.outputs.map(_.name)
    codeBuffer.append(s"    $$init_outs(${outputNames.mkString(", ")});\n")
    codeBuffer.append(s"    $$init_sigs($dutName);\n")

    /* Dump VPD Waveform File*/
    if (useVpdDump) {
      codeBuffer.append("    /*** Enable VPD dump ***/\n")
      codeBuffer.append("    if ($value$plusargs(\"vcdplusfile=%s\", " + dumpFileVar + ")) begin\n")
      codeBuffer.append(s"      $$vcdplusfile($dumpFileVar);\n")
      codeBuffer.append(s"      $$dumpvars(0, $dutName);\n")
      codeBuffer.append(s"      $$vcdpluson;\n")
      codeBuffer.append("    end\n")
    }

    /* Dump FSDB Waveform File*/
    if (useFsdbDump) {
      codeBuffer.append("    /*** Enable FSDB dump ***/\n")
      codeBuffer.append("    if ($value$plusargs(\"fsdbfile=%s\", " + dumpFileVar + ")) begin\n")
      codeBuffer.append(s"      $$fsdbDumpfile($dumpFileVar);\n")
      codeBuffer.append(s"      $$fsdbDumpvars(0, $dutName);\n")
      codeBuffer.append("    end\n")
    }

    codeBuffer.append("    /*** Enable VCD dump ***/\n")
    codeBuffer.append("    if ($value$plusargs(\"dumpfile=%s\", " + dumpFileVar + ")) begin\n")
    codeBuffer.append(s"      $$dumpfile($dumpFileVar);\n")
    codeBuffer.append(s"      $$dumpvars(0, $dutName);\n")
    codeBuffer.append("      /* exclude the startup from the wave dump */\n")
    codeBuffer.append("      $dumpoff;\n")
    codeBuffer.append("    end\n")

    codeBuffer.append("  end\n\n")

    codeBuffer.append(s"  always @(negedge $clockName)  begin\n")
    codeBuffer.append(s"    if ($dumpFileVar && !$dumpOnVar) begin\n")
    codeBuffer.append(s"      $dumpOnVar = 1;\n")
    if (useFsdbDump) codeBuffer.append("      $fsdbDumpon;\n")
    else codeBuffer.append("      $dumpon;\n")
    codeBuffer.append("    end\n")
    codeBuffer.append("    $tick();\n")
    codeBuffer.append("    $dumpflush;\n")
    codeBuffer.append("  end\n\n")
    codeBuffer.append("endmodule\n")

    codeBuffer.toString()
  }
}
