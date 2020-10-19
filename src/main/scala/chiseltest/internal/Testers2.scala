// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import firrtl.AnnotationSeq
import firrtl.stage.phases.DriverCompatibility.TopNameAnnotation

import scala.util.DynamicVariable

object Context {
  val context = new DynamicVariable[Option[ThreadedBackend]](None)

  def apply(): ThreadedBackend = context.value.get
}
