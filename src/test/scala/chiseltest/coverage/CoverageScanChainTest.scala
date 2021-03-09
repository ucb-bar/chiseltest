// SPDX-License-Identifier: Apache-2.0

package chiseltest.coverage

import chisel3.Module
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.{AnnotationSeq, EmittedFirrtlCircuitAnnotation, EmittedFirrtlModuleAnnotation}
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
    val (result, rAnnos) = compile(new Test1Module)
    println(result)

    val chainInfo = rAnnos.collectFirst { case a: CoverageScanChainInfo => a }.get
    println(chainInfo)
  }

  private def compile[M <: Module](gen: => M): (String, AnnotationSeq) = {
    val stage = new ChiselStage

    // "-ll", "trace"
    val r = stage.execute(Array("-X", "low"), ChiselGeneratorAnnotation(() => gen) +: annos)
    val src = r.collect {
      case EmittedFirrtlCircuitAnnotation(a) => a
      case EmittedFirrtlModuleAnnotation(a)  => a
    }.map(_.value).mkString("")

    (src, r)
  }
}
