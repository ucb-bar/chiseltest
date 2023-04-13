// package declaration
package chiseltest.simulator.jni


// Using Java I/O and Nio
import java.io.{File}
import java.nio.file.{Paths, Path, Files}

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

  /* Using os.Path */
  // val tmpDir = os.Path(System.getProperty("java.io.tmpdir"))
  // println(tmpDir.toString)
  // val jniDir = tmpDir / "jni"
  // println(jniDir.toString)
  // val bridgePath = jniDir / "libjnibridge.so"
  // val libSoFile = new java.io.File(bridgePath.toString)


  val tmpDir = Paths.get(System.getProperty("java.io.tmpdir"))
  val name = tmpDir.getFileSystem.getPath("jni")
	if (name.getParent != null) throw new IllegalArgumentException("Invalid prefix or suffix")
	val tempJniDir = tmpDir.resolve(name).toString
  
  if (!(new File(tempJniDir).exists())) {
    // TODO - do we want a persistence directory or a temp directory)
    Files.createDirectory(Paths.get(tempJniDir))
  }

  val libSoFile = new File(tempJniDir + "/libjnibridge.so")
  println(libSoFile.toString())

  if (!libSoFile.exists()) {
    // Compiling flags depends on OS?
    val jniResourcesPath = os.pwd / "src" / "main" / "resources" / "jni"
  
    println(s"""-I'$javaHome/include'""")
    println(s"""-I'$javaHome/include/$osIncludeName'""")
    val fail = os.proc("gcc", 
      "-fPIC", 
      "-shared", 
      s"""-I'$javaHome/include'""",
      s"""-I'$javaHome/include/$osIncludeName'""",
      "chiseltest_simulator_jni_JniAPI_00024.c", 
      "-o", 
      libSoFile.toString()).call(
        cwd = jniResourcesPath)
    
    assert(fail.exitCode == 0)

    println(libSoFile.exists())
  } else {
    println("doesn't exist!")
  }

  // val soPath = os.pwd / "src" / "main" / "resources" / "jni" / "libjnibridge.so"
  println(libSoFile.toString)
  System.load(libSoFile.toString)

  /* Takes in a path and returns a unique shared object id that corresponds with the shared object at that path */
  @native def load_so(path: String): Int

  @native def call_sim_init(so_id: Int): Long 

  @native def call_step(so_id: Int, s: Long, cycles: Int): Int

  @native def call_update(so_id: Int, s: Long): Unit

  @native def call_finish(so_id: Int, s: Long): Unit

  @native def call_resetCoverage(so_id: Int, s: Long): Unit

  @native def call_writeCoverage(so_id: Int, s: Long, filename: String): Unit

  @native def call_poke(so_id: Int, s: Long, id: Int, value: Int): Int

  @native def call_peek(so_id: Int, s: Long, id: Int): Int

  @native def call_poke_wide(so_id: Int, s: Long, id: Int, offset: Int, value: Int): Unit

  @native def call_peek_wide(so_id: Int, s: Long, id: Int, offset: Int): Int

  @native def call_set_args(so_id: Int, s: Long, argc: Int, argv: String): Unit

  def main(args: Array[String]): Unit = {

    val soPath = os.pwd / "src" / "main" / "resources" / "jni" / "VFoo"
    val vfoo_id = JniAPI.load_so(soPath.toString())
    val vfoo_id2 = JniAPI.load_so(soPath.toString())

    val ptr = call_sim_init(vfoo_id)
    val ptr2 = call_sim_init(vfoo_id2)

    call_poke(vfoo_id, ptr, 1, 10)
    call_poke(vfoo_id2, ptr2, 1, 20)
    call_update(vfoo_id, ptr) // force reevaluation of the DUT (for combinational paths)

    val output = call_peek(vfoo_id, ptr, 1)
    val output2 = call_peek(vfoo_id2, ptr2, 1)

    println(output)
    println(output2)
    assert(output == 10)
    assert(output2 == 20)

    call_poke(vfoo_id, ptr, 1, 30)
    call_poke(vfoo_id2, ptr2, 1, 40)

    val output_1 = call_peek(vfoo_id, ptr, 1)
    val output2_1 = call_peek(vfoo_id2, ptr2, 1)

    println(output_1)
    println(output2_1)
    assert(output_1 == 30)
    assert(output2_1 == 40)
  }
}

