// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import org.scalatest.flatspec.AnyFlatSpec

class CodeBaseTest extends AnyFlatSpec {
  behavior.of("CodeBase")

  it should "read in a code base" in {
    val c = new CodeBase(os.pwd / "src" / "test" / "scala")

    // TODO: test warn about duplicates
    // c.warnAboutDuplicates()
    // assert(c.duplicateKeys == List("package.scala"))
    // assert(c.isDuplicate("package.scala"))
    assert(!c.isDuplicate("CodeBaseTest.scala"))

    // get this line
    assert(c.getLine("CodeBaseTest.scala", 21).get.trim == "// get this line")
  }

  it should "by default use the same root dir as chisel" in {
    val c = new CodeBase()
    assert(c.root == os.pwd)
  }
}
