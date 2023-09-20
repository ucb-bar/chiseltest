// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import firrtl2.annotations.NoTargetAnnotation

/** Hint for the backend to try and re-use a compiled simulation from a prior run.
  * @warn
  *   this is an experimental option and might be removed in the next release without warning
  */
case object CachingAnnotation extends NoTargetAnnotation

/** Use this option to have all simulator interactions printed to stdout.
  * @warn
  *   this is an experimental option and might be removed in the next release without warning
  */
case object PrintPeekPoke extends NoTargetAnnotation
