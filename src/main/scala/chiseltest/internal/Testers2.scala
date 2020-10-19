// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import firrtl.AnnotationSeq
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation

import scala.util.DynamicVariable

object Context {
  class Instance(val backend: ThreadedBackend)

  val context = new DynamicVariable[Option[Instance]](None)

  def apply(): Instance = context.value.get
}
