package chiseltest.simulator.jni

import scala.io.Source

object JniAPI {
  /* Borrowed from JNAUtils */
  val isWindows: Boolean = System.getProperty("os.name").toLowerCase().contains("win")

  def javaHome: String = System.getProperty("java.home") match {
    case s: String if s.endsWith("/jre") => s.dropRight(4)
    case s: String                       => s
  }

  def osIncludeName: String = if (isWindows) {
    "win32"
  } else {
    System.getProperty("os.name") match {
      case "Mac OS X" => "darwin"
      case "Linux"    => "linux"
      case s: String => s
    }
  }

  val tmpDir = os.Path(System.getProperty("java.io.tmpdir"))
  val jniSoDir = tmpDir / "chiseltest_jni_bridge"
  if (!os.exists(jniSoDir)) {
    os.makeDir(jniSoDir)
  }
  val jniSo = jniSoDir / "libjnibridge.so"

  // TODO: save a hash of the sources used to compile the .so, to force recompilation of the bridge library if the sources change
  if (!os.exists(jniSo)) {
    val jniFile = "chiseltest_simulator_jni_JniAPI_00024"
    os.write(jniSoDir / s"$jniFile.c", Source.fromResource(s"jni/$jniFile.c").getLines().mkString("\n"))
    os.write(jniSoDir / s"$jniFile.h", Source.fromResource(s"jni/$jniFile.h").getLines().mkString("\n"))

    val gccFlags: Seq[String] = Seq(
      "-fPIC",
      "-shared",
      // "-dynamiclib", // OSX only option
      s"""-I$javaHome/include""",
      s"""-I$javaHome/include/$osIncludeName"""
    )

    val gccCmd = Seq("gcc") ++ gccFlags :+ s"$jniFile.c" :+ "-o" :+ jniSo.toString
    val gccCmdStatus = os.proc(gccCmd).call(cwd = jniSoDir)
    assert(gccCmdStatus.exitCode == 0, s"While compiling the JNI bridge library, gcc exited with a non-zero exit code (${gccCmdStatus.exitCode}) and stdout: ${gccCmdStatus.out.toString}")
  }

  System.load(jniSo.toString)

  /* Takes in a path and returns a unique shared object id that corresponds with the shared object at that path */
  @native def load_so(path: String): Int

  @native def call_sim_init(so_id: Int): Long

  @native def call_step(so_id: Int, s: Long, cycles: Int): Long

  @native def call_update(so_id: Int, s: Long): Unit

  @native def call_finish(so_id: Int, s: Long): Unit

  @native def call_resetCoverage(so_id: Int, s: Long): Unit

  @native def call_writeCoverage(so_id: Int, s: Long, filename: String): Unit

  @native def call_poke(so_id: Int, s: Long, id: Int, value: Long): Unit

  @native def call_peek(so_id: Int, s: Long, id: Int): Long

  @native def call_poke_wide(so_id: Int, s: Long, id: Int, offset: Int, value: Long): Unit

  @native def call_peek_wide(so_id: Int, s: Long, id: Int, offset: Int): Long

  @native def call_set_args(so_id: Int, s: Long, argc: Int, argv: Array[String]): Unit
}
