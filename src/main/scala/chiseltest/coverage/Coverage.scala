package chiseltest.coverage

import firrtl._

import scala.util.matching.Regex

object Coverage {

  type Lines = List[(String, List[Int])]
  private val chiselFileInfo: Regex = raw"\s*([^\.]+\.\w+) (\d+):(\d+)".r

  def parseFileInfo(i: ir.FileInfo): (String, Int) = i.unescaped match {
    case chiselFileInfo(filename, line, col) => (filename, line.toInt)
  }

  def infosToLines(infos: Seq[ir.Info]): Lines = {
    val parsed = findFileInfos(infos).map(parseFileInfo)
    val byFile = parsed.groupBy(_._1).toList.sortBy(_._1)
    byFile.map{ case (filename, e) => filename -> e.map(_._2).toSet.toList.sorted }
  }

  def findFileInfos(infos: Seq[ir.Info]): Seq[ir.FileInfo] = infos.flatMap(findFileInfos)
  def findFileInfos(info: ir.Info): Seq[ir.FileInfo] = info match {
    case ir.MultiInfo(infos) => findFileInfos(infos)
    case f : ir.FileInfo => List(f)
    case _ => List()
  }

  def findClock(m: ir.Module): ir.Expression = {
    val clockIO = m.ports.filter(_.tpe == ir.ClockType)
    val clockInputs = clockIO.filter(_.direction == ir.Input)
    assert(clockInputs.length == 1, s"This transformation only works if there is exactly one clock: $clockInputs")
    ir.Reference(clockInputs.head)
  }

  def findReset(m: ir.Module): ir.Expression = {
    val inputs = m.ports.filter(_.direction == ir.Input)
    val ofResetType = inputs.filter(p => p.tpe == ir.AsyncResetType || p.tpe == ir.ResetType)
    val boolWithCorrectName = inputs.filter(p => p.tpe == ir.UIntType(ir.IntWidth(1)) && p.name == "reset")
    val resetInputs = ofResetType ++ boolWithCorrectName
    assert(resetInputs.length == 1, s"This transformation only works if there is exactly one reset: $resetInputs")
    ir.Reference(resetInputs.head)
  }
}
