package chiseltest.internal

object sanitizeFileName {

  /** Simple file name sanitizer
    *
    * @param name
    *   file name to be sanitized
    * @return
    * @note
    *   This function is not considered a standard part of testers2 API, it will likely go away
    */
  def apply(name: String): String = {
    name.replaceAll(" ", "_").replaceAll("\\W+", "") // sanitize name
  }
}
