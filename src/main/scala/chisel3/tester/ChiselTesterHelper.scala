// See LICENSE for license details.

package chisel3.tester

import org.scalatest.exceptions.TestFailedException

import scala.collection.mutable

trait ChiselTesterHelper { self: TestEnvInterface =>

  protected val batchedFailures: mutable.ArrayBuffer[TestFailedException] = new mutable.ArrayBuffer

  def topFileName: Option[String]

  override def testerFail(msg: String): Unit = {
    batchedFailures += new TestFailedException(s"$msg", 4)
  }

  protected def getExpectDetailedTrace(trace: Seq[StackTraceElement], inFile: String): String = {
    val fullTrace = Context().backend.getParentTraceElements ++ trace

    // In the threading case, this needs to be overridden to trace through parent threads
    val lineNumbers = fullTrace.collect {
      case ste if ste.getFileName == inFile => ste.getLineNumber
    }.mkString(", ")
    if (lineNumbers.isEmpty) {
      s" (no lines in $inFile)"
    } else {
      s" (lines in $inFile: $lineNumbers)"
    }
  }

  override def testerExpect(expected: Any, actual: Any, signal: String, msg: Option[String]): Unit = {
    if (expected != actual) {
      val appendMsg = msg match {
        case Some(_) => s": $msg"
        case _ => ""
      }

      val trace = new Throwable
      val expectStackDepth = trace.getStackTrace.indexWhere(ste =>
        ste.getClassName == "chisel3.tester.package$testableData" && ste.getMethodName == "expect")
      require(expectStackDepth != -1,
        s"Failed to find expect in stack trace:\r\n${trace.getStackTrace.mkString("\r\n")}")

      val trimmedTrace = trace.getStackTrace.drop(expectStackDepth + 2)
      val detailedTrace = topFileName.map(getExpectDetailedTrace(trimmedTrace.toSeq, _)).getOrElse("")

      batchedFailures += new TestFailedException(
        s"$signal=$actual did not equal expected=$expected$appendMsg$detailedTrace",
        expectStackDepth + 1)
    }
  }

  override def checkpoint(): Unit = {
    // TODO: report multiple exceptions simultaneously
    for (failure <- batchedFailures) {
      throw failure
    }
  }
}

