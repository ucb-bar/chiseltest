// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import chisel3.Module
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl._
import firrtl.options.Dependency
import firrtl.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class CoverageScanChainTest extends AnyFlatSpec {
  behavior of "CoverageScanChain"

  private val annos = Seq(
    RunFirrtlTransformAnnotation(Dependency(LineCoveragePass)),
    RunFirrtlTransformAnnotation(Dependency(CoverageScanChainPass))
  )

  it should "insert a scan chain for all coverage statements" in {
    val width = 30
    val widthAnno = CoverageScanChainOptions(width)

    val (result, rAnnos) = compile(new Test1Module(withSubmodules = true), Seq(widthAnno))
    // println(result)
    val l = result.split('\n').map(_.trim)

    val chainInfo = rAnnos.collectFirst { case a: CoverageScanChainInfo => a }.get
    assert(chainInfo.width == width)

    // the cover chain ports will need to use an additional '_' because of the colliding register named `cover_chain_en`
    assert(chainInfo.prefix == "cover_chain_")
    assert(l.contains("input cover_chain__en : UInt<1>"))
    assert(l.contains(s"input cover_chain__in : UInt<$width>"))
    assert(l.contains(s"output cover_chain__out : UInt<$width>"))

    // the submodules do not have a collision
    assert(l.contains("input cover_chain_en : UInt<1>"))
    assert(l.contains(s"input cover_chain_in : UInt<$width>"))
    assert(l.contains(s"output cover_chain_out : UInt<$width>"))

    val expectedCovers = List(
      "Test1Module.l_3", "Test1Module.l_0", "Test1Module.l_1", "Test1Module.l_2", "Test1Module.cover_0",
      "Test1Module.c0.l_0", "Test1Module.c0.cover_0",
      "Test1Module.c1.l_0", "Test1Module.c1.cover_0",
    )
    assert(chainInfo.covers == expectedCovers)
  }

  private def compile[M <: Module](gen: => M, a: AnnotationSeq = List()): (String, AnnotationSeq) = {
    val stage = new ChiselStage

    // "-ll", "trace"
    val r = stage.execute(Array("-X", "low"), ChiselGeneratorAnnotation(() => gen) +: a ++: annos)
    val src = r.collect {
      case EmittedFirrtlCircuitAnnotation(a) => a
      case EmittedFirrtlModuleAnnotation(a)  => a
      case EmittedVerilogCircuitAnnotation(a) => a
      case EmittedVerilogModuleAnnotation(a) => a
    }.map(_.value).mkString("")

    (src, r)
  }
}
