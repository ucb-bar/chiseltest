/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle.vcd

import firrtl.AnnotationSeq
import firrtl.options.{Shell, Stage}
import treadle.vcd.VCD.read

class VcdStage extends Stage {
  override val shell: Shell = new Shell("vcd") with VcdCli

  override def run(annotations: AnnotationSeq): AnnotationSeq = {

    val vcdSource = annotations.collectFirst { case VcdSourceNameAnnotation(name) => name }.getOrElse {
      println("You must specify source vcd source file")
      System.exit(1)
      ""
    }

    val vcdStartScope = annotations.collectFirst { case VcdStartScopeAnnotation(name) => name }
      .getOrElse("")
    val vcdRenameStartScope = annotations.collectFirst { case VcdRenameStartScopeAnnotation(name) => name }
      .getOrElse("")
    val vcdVarPrefix = annotations.collectFirst { case VcdVarPrefixScopeAnnotation(name) => name }
      .getOrElse("")

    val vcd = read(
      vcdSource,
      vcdStartScope,
      vcdRenameStartScope,
      vcdVarPrefix
    )

    println(s"${vcd.info}")

    if (annotations.contains(VcdDumpHumanReadableAnnotation)) {
      vcd.dumpHumanReadable()
    }

    annotations.collectFirst { case VcdTargetNameAnnotation(name) => name }.foreach { targetName =>
      vcd.write(targetName)
    }
    Seq.empty
  }
}

trait VcdCli { this: Shell =>
  parser.note("VCD Front End Options")
  Seq(
    VcdSourceNameAnnotation,
    VcdTargetNameAnnotation,
    VcdStartScopeAnnotation,
    VcdRenameStartScopeAnnotation,
    VcdVarPrefixScopeAnnotation,
    VcdDumpHumanReadableAnnotation
  )
}
