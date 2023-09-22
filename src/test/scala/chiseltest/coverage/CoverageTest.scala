// Copyright 2021-2023 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.coverage

import firrtl2.ir
import org.scalatest.flatspec.AnyFlatSpec

class CoverageTest extends AnyFlatSpec {

  "parseFileInfo" should "be robust" in {
    assert(
      Coverage.parseFileInfo(ir.FileInfo("Atomics.scala 35:16 Atomics.scala 35:16")) ==
        Seq("Atomics.scala" -> 35, "Atomics.scala" -> 35)
    )
  }

}
