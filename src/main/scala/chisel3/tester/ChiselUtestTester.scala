package chisel3.tester

import utest.TestSuite
import chisel3.MultiIOModule
import chisel3.tester.internal._
import chisel3.tester.experimental.sanitizeFileName

import firrtl.AnnotationSeq

trait ChiselUtestTester extends TestSuite with TestEnvInterface {
  val topFileName = Some(getClass.getSimpleName)

  def testCircuit[T <: MultiIOModule](dutGen: => T, annotationSeq: AnnotationSeq = Seq.empty)(testFn: T => Unit): Unit = {
    val newAnnos = addDefaultTargetDir(sanitizeFileName(s"${getClass.getSimpleName}_${System.currentTimeMillis()}"), annotationSeq)
    batchedFailures.clear()
    Context.run(defaults.createDefaultTester(() => dutGen, newAnnos), this, testFn)
  }
}
