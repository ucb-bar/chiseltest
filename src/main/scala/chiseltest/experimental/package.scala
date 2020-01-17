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

package chiseltest

/** Your warranty is now void.
  *
  * experimental contains cutting edge features that are, well, experimental, and carry no
  * expectation of long-term support. We may break experimental APIs at any time. These may not
  * work as expected, or may have unforeseen side effects, or may be powerful yet dangerous.
  *
  * You have been warned.
  */
package object experimental {
  type TesterOptions = chiseltest.internal.TesterOptions
  val TesterOptions = chiseltest.internal.TesterOptions  // expose this internal object, whose "API" is unstable

  /**
    * Simple file name sanitizer
    * @param name file name to be sanitized
    * @return
    *
    * @note This function is not considered a standard part of testers2 API, it will likely go away
    */
  //TODO: make this internal but there should be a way end user can ask for current working directory of a test
  def sanitizeFileName(name: String): String = {
    name.replaceAll(" ", "_").replaceAll("\\W+", "") // sanitize name
  }
}
