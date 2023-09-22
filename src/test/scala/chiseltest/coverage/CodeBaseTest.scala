// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import org.scalatest.flatspec.AnyFlatSpec

class CodeBaseTest extends AnyFlatSpec {
  behavior.of("CodeBase")

  it should "read in a code base" in {
    val c = new CodeBase(os.pwd / "src" / "test" / "scala")

    // get this line with only the filename
    assert(c.getLine("CodeBaseTest.scala", 15).get.trim == "// get this line with only the filename")
    // get this line with relative path
    assert(
      c.getLine("chiseltest/coverage/CodeBaseTest.scala", 17).get.trim ==
        "// get this line with relative path"
    )
  }

  it should "by default use the same root dir as chisel" in {
    val c = new CodeBase()
    assert(c.root == os.pwd)
  }
}
