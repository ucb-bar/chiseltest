package chiseltest.legacy.backends.vcs

import java.io.Writer

import chisel3._
import chisel3.experimental.DataMirror
import chiseltest.legacy.backends.verilator.getDataNames

/** Generates the Module specific verilator harness cpp file for verilator compilation
  */
object GenVcsVerilogHarness {

  def getPorts(dut: MultiIOModule, separator: String = "."): (Seq[(Element, String)], Seq[(Element, String)]) = {
    getDataNames(dut, separator).partition { case (e, _) => DataMirror.directionOf(e) == ActualDirection.Input }
  }

  // getPorts() is going to return names prefixed with the dut name.
  // These don't correspond to code currently generated for verilog modules,
  //  so we have to strip the dut name prefix (and the delimiter) from the name.
  // We tell getPorts() to use "_" as the delimiter to simplify the replacement regexp.
  def fixNames(
    dutName: String,
    portSeq: (Seq[(Element, String)], Seq[(Element, String)])
  ): (Seq[(Element, String)], Seq[(Element, String)]) = {
    val replaceRegexp = ("^" + dutName + "_").r
    (
      portSeq._1.map { case (e: Element, s: String) => (e, replaceRegexp.replaceFirstIn(s, "")) },
      portSeq._2.map { case (e: Element, s: String) => (e, replaceRegexp.replaceFirstIn(s, "")) }
    )
  }

  def apply(dut: MultiIOModule, writer: Writer, vpdFilePath: String, isGateLevel: Boolean = false) {
    val dutName = dut.name
    // getPorts() is going to return names prefixed with the dut name.
    // These don't correspond to code currently generated for verilog modules,
    //  so we have to strip the dut name prefix (and the delimiter) from the name.
    // We tell getPorts() to use "_" as the delimiter to simplify the replacement regexp.
    def fixNames(
      portSeq: (Seq[(Element, String)], Seq[(Element, String)])
    ): (Seq[(Element, String)], Seq[(Element, String)]) = {
      val replaceRegexp = ("^" + dutName + "_").r
      (
        portSeq._1.map { case (e: Element, s: String) => (e, replaceRegexp.replaceFirstIn(s, "")) },
        portSeq._2.map { case (e: Element, s: String) => (e, replaceRegexp.replaceFirstIn(s, "")) }
      )
    }

    val (inputs, outputs) = fixNames(getPorts(dut, "_"))

    writer.write("module test;\n")
    writer.write("  reg clock = 1;\n")
    writer.write("  reg reset = 1;\n")
    val delay = if (isGateLevel) "#0.1" else ""
    inputs.foreach { case (node, name) =>
      writer.write(s"  reg[${node.getWidth - 1}:0] $name = 0;\n")
      writer.write(s"  wire[${node.getWidth - 1}:0] ${name}_delay;\n")
      writer.write(s"  assign $delay ${name}_delay = $name;\n")
    }
    outputs.foreach { case (node, name) =>
      writer.write(s"  wire[${node.getWidth - 1}:0] ${name}_delay;\n")
      writer.write(s"  wire[${node.getWidth - 1}:0] $name;\n")
      writer.write(s"  assign $delay $name = ${name}_delay;\n")
    }

    writer.write("  always #`CLOCK_PERIOD clock = ~clock;\n")
    writer.write("  reg vcdon = 0;\n")
    writer.write("  reg [1023:0] vcdfile = 0;\n")
    writer.write("  reg [1023:0] vpdfile = 0;\n")

    writer.write("\n  /*** DUT instantiation ***/\n")
    writer.write(s"  $dutName $dutName(\n")
    writer.write("    .clock(clock),\n")
    writer.write("    .reset(reset),\n")
    writer.write((inputs ++ outputs).map(_._2).map(name => s"    .$name(${name}_delay)").mkString(",\n"))
    writer.write("  );\n\n")

    writer.write("  initial begin\n")
    writer.write("    $init_rsts(reset);\n")
    writer.write("    $init_ins(%s);\n".format(inputs.map(_._2).mkString(", ")))
    writer.write("    $init_outs(%s);\n".format(outputs.map(_._2).mkString(", ")))
    writer.write("    $init_sigs(%s);\n".format(dutName))
    writer.write("    /*** VCD & VPD dump ***/\n")
    writer.write("    if ($value$plusargs(\"vcdfile=%s\", vcdfile)) begin\n")
    writer.write("      $dumpfile(vcdfile);\n")
    writer.write("      $dumpvars(0, %s);\n".format(dutName))
    writer.write("      $dumpoff;\n")
    writer.write("      vcdon = 0;\n")
    writer.write("    end\n")
    writer.write("    if ($value$plusargs(\"waveform=%s\", vpdfile)) begin\n")
    writer.write("      $vcdplusfile(vpdfile);\n")
    writer.write("    end else begin\n")
    writer.write("      $vcdplusfile(\"%s\");\n".format(vpdFilePath))
    writer.write("    end\n")
    writer.write("    if ($test$plusargs(\"vpdmem\")) begin\n")
    writer.write("      $vcdplusmemon;\n")
    writer.write("    end\n")
    writer.write("    $vcdpluson(0);\n")
    writer.write("  end\n\n")

    writer.write("  always @(%s clock) begin\n".format(if (isGateLevel) "posedge" else "negedge"))
    writer.write("    if (vcdfile && reset) begin\n")
    writer.write("      $dumpoff;\n")
    writer.write("      vcdon = 0;\n")
    writer.write("    end\n")
    writer.write("    else if (vcdfile && !vcdon) begin\n")
    writer.write("      $dumpon;\n")
    writer.write("      vcdon = 1;\n")
    writer.write("    end\n")
    writer.write("    %s $tick();\n".format(if (isGateLevel) "#0.05" else ""))
    writer.write("    $vcdplusflush;\n")
    writer.write("  end\n\n")
    writer.write("endmodule\n")
    writer.close()
  }
}
