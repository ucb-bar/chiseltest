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

package treadle2

import firrtl.AnnotationSeq
import firrtl.options.Shell
import firrtl.stage.FirrtlCli
import logger.Logger

/** This harness allows command line options to be added more easily to tests when requried
  * and most importantly allows logging to be contained within a Logger.makeScope. Without
  * this parallel running tests that use logging step on each others logger settings
  */
object TreadleTestHarness {
  //Note: typically shell here would need to include something like a TreadleCli in order to get options
  //but since treadle is a registered library treadles options are picked up automagically
  val shell: Shell = new Shell("treadle2") with FirrtlCli

  def apply(
    annotationSeq: AnnotationSeq = Seq.empty,
    flags:         Array[String] = Array.empty
  )(thunk:         TreadleTester => Unit
  ): Unit = {
    val allAnnotations = shell.parse(flags, annotationSeq)
    Logger.makeScope(allAnnotations) {
      val tester = TreadleTester(annotationSeq)
      thunk(tester)
      tester.finish
    }
  }
}
