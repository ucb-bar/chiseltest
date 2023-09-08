// SPDX-License-Identifier: Apache-2.0

package chiseltest.internal

import chisel3.Module
import chiseltest.TestResult
import firrtl2.annotations.NoTargetAnnotation

import scala.util.DynamicVariable

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

/** This option may speed up your test, but any use of `fork` or `timescope` will fail.
  * @warn
  *   this is an experimental option and might be removed in the next release without warning
  */
case object NoThreadingAnnotation extends NoTargetAnnotation
