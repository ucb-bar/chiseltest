// SPDX-License-Identifier: Apache-2.0

package treadle.vcd.diff

import firrtl.AnnotationSeq
import firrtl.options.{ProgramArgsAnnotation, Shell, Stage}
import treadle.vcd.VCD

trait VcdDiffCli { this: Shell =>
  parser.note("VCDDiff Command Line Options")
  Seq(
    DisplayRadix,
    V1StartTime,
    MaxDiffLines,
    TimeOffset,
    CompareWires,
    UnmatchedWires,
    DontDiffValues,
    IgnoreTempWires,
    WirePrefix1,
    WirePrefix2
  ).foreach(_.addOptions(parser))
}

class VcdDiffStage extends Stage {
  override val shell: Shell = new Shell("VCDDiff") with VcdDiffCli

  override def run(annotations: AnnotationSeq): AnnotationSeq = {

    val vcds = annotations.collect { case ProgramArgsAnnotation(fileName) =>
      VCD.read(fileName)
    }
    if (vcds.length != 2) {
      println("Error: Two files must be specifed for diff to run\nUsage: VCDDiff options <file1> <file2>")
      System.exit(1)
    }

    val vcdDiff = new VcdComparator(annotations)
    vcdDiff.compare(vcds.head, vcds.tail.head)

    annotations
  }
}
