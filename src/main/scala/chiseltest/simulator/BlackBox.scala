package chiseltest.simulator

import chiseltest.simulator.jna.JNAUtils

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import scala.io.Source

private object BlackBox {

  /** add the `.f` file containing the names of all blackbox verilog files, if it exists */
  def fFileFlags(targetDir: os.Path): Seq[String] = {
    val fFile = targetDir / firrtl.transforms.BlackBoxSourceHelper.defaultFileListName
    if (os.exists(fFile)) {
      if (JNAUtils.isWindows) {
        // The canonical path on Windows contains backslashes that
        // verilator does not recognize
        // We need to transform backslashes to slashes
        val windowsfFile = s"${fFile.toString()}.win"
        val writer = new PrintWriter(windowsfFile)
        val source = Source.fromFile(fFile.toString())
        source.getLines().foreach(line => writer.println(line.replace("\\", "/")))
        source.close()
        writer.close()
        Seq("-f", windowsfFile)
      } else {
        Seq("-f", fFile.toString())
      }
    } else { Seq() }
  }
}
