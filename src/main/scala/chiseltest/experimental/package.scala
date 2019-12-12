// See LICENSE for license details.

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
