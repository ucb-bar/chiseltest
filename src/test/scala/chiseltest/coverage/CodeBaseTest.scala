// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import org.scalatest.flatspec.AnyFlatSpec
import java.nio.file._

class CodeBaseTest extends AnyFlatSpec {
  behavior of "CodeBase"

  it should "read in a code base" in {
    val c = new CodeBase(Paths.get("src"))

    // TODO: test warn about duplicates
    //c.warnAboutDuplicates()
    assert(c.duplicateKeys == List("package.scala"))
    assert(c.isDuplicate("package.scala"))
    assert(!c.isDuplicate("CodeBaseTest.scala"))

    // get this line
    assert(c.getLine("CodeBaseTest.scala", 20).get.trim == "// get this line")
  }
}