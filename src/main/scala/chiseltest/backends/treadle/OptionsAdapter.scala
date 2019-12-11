// See LICENSE for license details.

package chiseltest.backends.treadle

import firrtl.AnnotationSeq
import firrtl.options.Phase

/** This provides a mechanism to map between generic testers2 level annotations and
  * any differences the backend may have in expressing their intent.
  *
  */
class OptionsAdapter extends Phase {
  /** Currently the WriteVcd annotation must be mapped to Treadle's notion of that
    *
    * @param annos the annotation list
    * @return
    */
  override def transform(annos: AnnotationSeq): AnnotationSeq = {
    annos.map {
      case chiseltest.internal.WriteVcdAnnotation => treadle.WriteVcdAnnotation
      case anno => anno
    }
  }
}
