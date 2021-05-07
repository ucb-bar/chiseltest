// SPDX-License-Identifier: Apache-2.0

package chiseltest

import firrtl.AnnotationSeq

// Returned from a call to test(...){...} in any of the testers (RawTester, ChiselScalatestTester, ChiselUtestTester).
// Currently only contains the annotations relevant for calculating coverage.
class TestResult(annotations: AnnotationSeq) {
  def getAnnotationSeq: AnnotationSeq = annotations
}
