package chiseltest.coverage

import org.scalatest.freespec.AnyFreeSpec
import chisel3._
import chisel3.util._
import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation

class AnalyzeCircuitSpec extends AnyFreeSpec with CompilerTest {
  "verify analyzeCircuit works" in {

    val (str, annos) = compile(new Queue(UInt(8.W), 16), "verilog", List(RunFirrtlTransformAnnotation(AnalyzeCircuit)))
    println(str)
    val message = annos.collect {
      case NumMuxesAnnotation(message) => message
    }
    println(message)
    //println(annos.foreach(println(_)))
  }
}
