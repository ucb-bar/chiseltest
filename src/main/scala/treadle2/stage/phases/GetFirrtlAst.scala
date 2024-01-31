// SPDX-License-Identifier: Apache-2.0

package treadle2.stage.phases

import firrtl2.options.Phase
import firrtl2.stage.{FirrtlCircuitAnnotation, FirrtlFileAnnotation, FirrtlSourceAnnotation}
import firrtl2.{AnnotationSeq, Parser}

/** There are multiple ways to get a FirrtlCircuit into treadle. There is a priority to these methods
  *   1. Specify a Firrtl AST with the FirrtlCircuitAnnotation 2. Specify Firrtl text with a FirrtlSourceAnnotation 3.
  *      Specify a file containing Firrtl with the FirrtlFileAnnotation
  */
object GetFirrtlAst extends Phase {
  override def transform(annotationSeq: AnnotationSeq): AnnotationSeq = {

    /* first priority, does circuit already exist */
    def handleTreadleCircuit(): Option[AnnotationSeq] = {
      if (annotationSeq.exists { case FirrtlCircuitAnnotation(_) => true; case _ => false }) {
        Some(annotationSeq)
      } else {
        None
      }
    }

    /* second priority, does firrtl source exist */
    def handleFirrtlSource(): Option[AnnotationSeq] = {
      annotationSeq.collectFirst { case FirrtlSourceAnnotation(firrtlText) => firrtlText } match {
        case Some(text) =>
          val circuit = Parser.parse(text)
          Some(FirrtlCircuitAnnotation(circuit) +: annotationSeq)

        case _ =>
          None
      }
    }

    /* third priority, does firrtl file exist */
    def handleFirrtlFile(): Option[AnnotationSeq] = {
      annotationSeq.collectFirst { case FirrtlFileAnnotation(fileName) => fileName } match {
        case Some(fileName) =>
          val file = scala.io.Source.fromFile(fileName)
          val text = file.mkString
          file.close()

          val circuit = Parser.parse(text)
          Some(FirrtlCircuitAnnotation(circuit) +: annotationSeq)

        case _ =>
          None
      }
    }

    val newAnnotations = handleTreadleCircuit().getOrElse {
      handleFirrtlSource().getOrElse {
        handleFirrtlFile().getOrElse {
          annotationSeq
        }
      }
    }
    newAnnotations
  }
}
