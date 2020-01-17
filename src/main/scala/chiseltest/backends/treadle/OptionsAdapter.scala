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
