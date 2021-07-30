// SPDX-License-Identifier: Apache-2.0

package chiseltest.simulator.jni

import net.openhft.compiler.CompilerUtils

object JNIUtils {
  def javaHome: String = System.getProperty("java.home") match {
    case s: String if s.endsWith("/jre") => s.dropRight(4)
    case s: String                       => s
  }
  def osIncludeName: String = System.getProperty("os.name") match {
    case "Mac OS X" => "darwin"
    case "Linux"    => "linux"
    case s: String => s
  }

  /** additional ccFlags that are required in order to compile into a library that can be loaded with JNI */
  def ccFlags: Seq[String] = Seq(
    "-fPIC",
    "-shared",
    s"-I$javaHome/include",
    s"-I$javaHome/include/$osIncludeName",
    // hide all symbols in the shared object by default
    // https://stackoverflow.com/questions/435352/limiting-visibility-of-symbols-when-linking-shared-libraries
    "-fvisibility=hidden"
  )
  def ldFlags: Seq[String] = Seq(
    "-shared",
    "-dynamiclib",
    // hide all symbols in the shared object by default
    // https://stackoverflow.com/questions/435352/limiting-visibility-of-symbols-when-linking-shared-libraries
    "-fvisibility=hidden"
  )

  /** needs to match up with [[TesterSharedLibInterface]]! */
  val Methods = Seq(
    ("void", "step", Seq()),
    ("void", "update", Seq()),
    ("void", "finish", Seq()),
    ("void", "resetCoverage", Seq()),
    ("void", "writeCoverage", Seq("filename" -> "String")),
    ("void", "poke", Seq("id" -> "int", "value" -> "long")),
    ("long", "peek", Seq("id" -> "int"))
  )

  private var idCounter = 123
  private def getUniqueId: Int = synchronized {
    val id = idCounter
    idCounter += 1
    id
  }
  private val PackageName = "chiseltest.simulator.jni"

  private def genJNIJavaCode(libPath: os.Path, className: String): String = {
    val methods = Methods.map { case (ret, name, args) =>
      s"  public native $ret $name(" + args.map { case (n, t) => s"$t $n" }.mkString(", ") + ");"
    }.mkString("\n")

    s"""package $PackageName;
       |
       |public class $className implements TesterSharedLibInterface {
       |  private long state = 0; // used by the JNI C++ code
       |  private native void sim_init();
       |$methods
       |  static {
       |   try { System.load("$libPath"); }
       |   catch (Exception e) { System.out.println("Failed to load $libPath: " + e.toString()); }
       |  }
       |  public $className() {
       |    sim_init();
       |  }
       |}
       |""".stripMargin
  }

  def compileAndLoadJNIClass(libPath: os.Path, className: String): TesterSharedLibInterface = {
    println(s"compileAndLoadJNIClass($libPath, $className)")
    val code = genJNIJavaCode(libPath, className)
    val aClass = CompilerUtils.CACHED_COMPILER.loadFromJava(PackageName + "." + className, code)
    aClass.getDeclaredConstructor().newInstance().asInstanceOf[TesterSharedLibInterface]
  }

  private def jniType(tpe: String): String = if (tpe == "void") { tpe }
  else { "j" + tpe.toLowerCase }

  def genJNICppCode(simState: String): (String, String) = {
    val className = "TesterSharedLib" + getUniqueId.toString

    val header =
      """#ifdef INCLUDE_MAIN
        |#else /* INCLUDE_MAIN */
        |#include <jni.h>
        |""".stripMargin

    // internal functions that allow us to cache the pointer to the "sim_state" object
    val stateCache =
      s"""static jfieldID getPtrId(JNIEnv *env, jobject obj) {
         |  jclass c = env->GetObjectClass(obj);
         |  jfieldID id = env->GetFieldID(c, "state", "J");
         |  env->DeleteLocalRef(c);
         |
         |  return id;
         |}
         |
         |static sim_state* get_state(JNIEnv *env, jobject obj) {
         |  static sim_state* cached = NULL;
         |  if (cached == NULL) {
         |    cached = (sim_state*) env->GetLongField(obj, getPtrId(env, obj));
         |  }
         |  return cached;
         |}
         |// besides the regular JNIEXPORT we also need to set the symbol visibility since symbols
         |// are hidden by default in the linker
         |#define _EXPORT JNIEXPORT __attribute__((visibility("default")))
         |""".stripMargin

    val ApiPrefix = "JNICALL Java_chiseltest_simulator_jni_" + className

    // define common functions that will be called from Java
    val initFoo =
      s"""extern "C" {
         |_EXPORT void JNICALL ${ApiPrefix}_sim_1init(JNIEnv *env, jobject obj) {
         |  sim_state *s = create_sim_state();
         |  env->SetLongField(obj, getPtrId(env, obj), (jlong)s);
         |}
         |
         |""".stripMargin
    val methods = Methods.map { case (ret, name, args) =>
      val argDecl = (Seq("obj" -> "object") ++ args).map { case (n, t) => jniType(t) + " " + n }.mkString(", ")
      val retTpe = jniType(ret)
      val decl = s"_EXPORT $retTpe JNICALL ${ApiPrefix}_$name(JNIEnv *env, $argDecl) {\n"
      val callRet = if (ret == "void") "" else "return "
      val call = "  " + callRet + s"get_state(env, obj)->$name(" + ("env" +: args.map(_._1)).mkString(", ") + ");\n"
      decl + call + "}\n"
    }.mkString("\n")

    val footer =
      s"""} /* extern C */
         |#endif /* INCLUDE_MAIN */
         |""".stripMargin

    val code = header + simState + stateCache + initFoo + methods + footer
    (code, className)
  }
}

trait TesterSharedLibInterface {
  def step():          Unit
  def update():        Unit
  def finish():        Unit
  def resetCoverage(): Unit
  def writeCoverage(filename: String): Unit
  def poke(id:                Int, value: Long): Unit
  def peek(id:                Int): Long
}
