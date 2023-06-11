// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal

import chisel3.experimental.{ChiselAnnotation, annotate}
import chiseltest.simulator.{Firrtl2AnnotationWrapper, convertTargetToFirrtl2}
import firrtl2.annotations.PresetAnnotation

/** marks a signal as a "preset", i.e., a reset signal that will turn all connected registers into
  * pre-initialized registers. */
object annotateAsPreset {
  def apply(toTarget: => firrtl.annotations.ReferenceTarget) = {
    annotate(new ChiselAnnotation {
      override def toFirrtl = Firrtl2AnnotationWrapper(PresetAnnotation(convertTargetToFirrtl2(toTarget)))
    })
  }

}
