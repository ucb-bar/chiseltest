// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends.btor

import chiseltest.formal.backends._
import firrtl2.backends.experimental.smt._

private[chiseltest] trait Solver {
  def cmd(btorFile: String, kMax: Int): Seq[String]
}

object BtormcBtor2 extends Solver {
  override def cmd(btorFile: String, kMax: Int): Seq[String] = {
    val kmaxOpt = if (kMax > 0) Seq("--kmax", kMax.toString) else Seq()
    Seq("btormc") ++ kmaxOpt ++ Seq(btorFile)
  }
}

object PonoBtor2 extends Solver {
  override def cmd(btorFile: String, kMax: Int): Seq[String] = {
    val kmaxOpt = if (kMax > 0) Seq("-k", kMax.toString) else Seq()
    Seq("pono") ++ Seq("-e", "bmc") ++ kmaxOpt ++ Seq("--vcd", "ignore.vcd") ++ Seq(btorFile)
  }
}

class Btor2ModelChecker(solver: Solver, targetDir: os.Path) extends IsModelChecker {
  override val fileExtension = ".btor2"
  override val name:   String = "btormc"
  override val prefix: String = "btormc"

  override def check(sys: TransitionSystem, kMax: Int): ModelCheckResult = {
    // serialize the system to btor2
    val filename = sys.name + ".btor"
    // btromc isn't happy if we include output nodes, so we skip them during serialization
    val lines = Btor2Serializer.serialize(sys, skipOutput = true)
    os.write.over(targetDir / filename, lines.mkString("", "\n", "\n"))

    // execute model checker
    val cmd = solver.cmd(filename, kMax)
    val r = os.proc(cmd).call(cwd = targetDir, check = false)

    // write stdout to file for debugging
    val res = r.out.lines()
    os.write.over(targetDir / (filename + ".out"), res.mkString("", "\n", "\n"))

    // check to see if we were successful
    assert(r.exitCode == 0, s"We expect btormc to always return 0, not ${r.exitCode}. Maybe there was an error.")
    val isSat = res.nonEmpty && res.head.trim.startsWith("sat")

    if (isSat) {
      val witness = Btor2WitnessParser.read(res, 1).head
      ModelCheckFail(convertWitness(sys, witness))
    } else {
      ModelCheckSuccess()
    }
  }

  private def convertWitness(sys: TransitionSystem, bw: Btor2Witness): Witness = {
    val badNames = sys.signals.filter(_.lbl == IsBad).map(_.name).toIndexedSeq
    val failed = bw.failed.map(badNames)
    Witness(failed, bw.regInit, bw.memInit, bw.inputs)
  }
}
