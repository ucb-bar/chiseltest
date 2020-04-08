package chiseltest.legacy.backends.vcs

import java.io.{File, IOException}
import java.nio.file.{FileAlreadyExistsException, Files, Paths}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

/**
  * Copies the necessary header files used for verilator compilation to the specified destination folder
  */
object CopyVpiFiles {
  def apply(destinationDirPath: String): Unit = {
    new File(destinationDirPath).mkdirs()
    val simApiHFilePath = Paths.get(destinationDirPath + "/sim_api.h")
    val vpiHFilePath = Paths.get(destinationDirPath + "/vpi.h")
    val vpiCppFilePath = Paths.get(destinationDirPath + "/vpi.cpp")
    val vpiTabFilePath = Paths.get(destinationDirPath + "/vpi.tab")

    for(fileName <- Seq(simApiHFilePath, vpiHFilePath, vpiCppFilePath, vpiTabFilePath)) {
      try {
        Files.createFile(fileName)
      } catch {
        case _: FileAlreadyExistsException =>
        case x: IOException =>
          System.err.format("createFile error: %s%n", x)
      }
    }

    val resourceDir = "/chisel3/tester/legacy/backends/verilator"
    Files.copy(getClass.getResourceAsStream(s"$resourceDir/sim_api.h"), simApiHFilePath, REPLACE_EXISTING)
    Files.copy(getClass.getResourceAsStream(s"$resourceDir/vpi.h"), vpiHFilePath, REPLACE_EXISTING)
    Files.copy(getClass.getResourceAsStream(s"$resourceDir/vpi.cpp"), vpiCppFilePath, REPLACE_EXISTING)
    Files.copy(getClass.getResourceAsStream(s"$resourceDir/vpi.tab"), vpiTabFilePath, REPLACE_EXISTING)
  }
}
