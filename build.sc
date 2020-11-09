// SPDX-License-Identifier: Apache-2.0

import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._
import mill.scalalib.publish._

object chiseltest extends mill.Cross[chiseltestCrossModule]("2.11.12", "2.12.10")

val defaultVersions = Map(
  "chisel3" -> "3.4-SNAPSHOT",
  "treadle" -> "1.3-SNAPSHOT"
)

def getVersion(dep: String, org: String = "edu.berkeley.cs") = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  ivy"$org::$dep:$version"
}

class chiseltestCrossModule(val crossScalaVersion: String) extends CrossSbtModule with PublishModule with ScalafmtModule {
  def chisel3Module: Option[PublishModule] = None

  def chisel3IvyDeps = if (chisel3Module.isEmpty) Agg(
    getVersion("chisel3")
  ) else Agg.empty[Dep]

  def treadleModule: Option[PublishModule] = None

  def treadleIvyDeps = if (treadleModule.isEmpty) Agg(
    getVersion("treadle")
  ) else Agg.empty[Dep]

  override def millSourcePath = super.millSourcePath / os.up

  // 2.12.12 -> Array("2", "12", "12") -> "12" -> 12
  private def majorVersion = crossScalaVersion.split('.')(1).toInt

  def publishVersion = "0.3-SNAPSHOT"

  private def javacCrossOptions = majorVersion match {
    case i if i < 12 => Seq("-source", "1.7", "-target", "1.7")
    case _ => Seq("-source", "1.8", "-target", "1.8")
  }

  private def scalacCrossOptions = majorVersion match {
    case i if i < 12 => Seq()
    case _ => Seq("-Xsource:2.11")
  }

  override def scalacOptions = T {
    super.scalacOptions() ++ Seq(
      "-deprecation",
      "-feature",
      "-language:reflectiveCalls" // required by SemanticDB compiler plugin
    ) ++ scalacCrossOptions
  }

  override def javacOptions = T {
    super.javacOptions() ++ javacCrossOptions
  }

  override def moduleDeps = super.moduleDeps ++ chisel3Module ++ treadleModule

  override def ivyDeps = T {
    Agg(
      ivy"org.scalatest::scalatest:3.2.0",
      ivy"com.lihaoyi::utest:0.7.4"
    ) ++ chisel3IvyDeps ++ treadleIvyDeps
  }

  object test extends Tests with ScalafmtModule {
    override def ivyDeps = T {
      Agg(
        ivy"org.scalatest::scalatest:3.0.8",
        ivy"com.lihaoyi::utest:0.7.4"
      ) ++ chisel3IvyDeps ++ treadleIvyDeps
    }

    def testFrameworks = T {
      Seq(
        "org.scalatest.tools.Framework",
        "utest.runner.Framework"
      )
    }

    // a sbt-like testOnly command.
    // for example, mill -i "chiseltest[2.12.12].test.testOnly" "chiseltest.tests.AsyncClockTest"
    def testOnly(args: String*) = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }

  def pomSettings = T {
    PomSettings(
      description = artifactName(),
      organization = "edu.berkeley.cs",
      url = "https://github.com/freechipsproject/chisel-testers2",
      licenses = Seq(License.`BSD-3-Clause`),
      versionControl = VersionControl.github("ucb-bar", "chisel-testers2"),
      developers = Seq(
        Developer("ducky64", "Richard Lin", "https://aspire.eecs.berkeley.edu/author/rlin/")
      )
    )
  }

  // make mill publish sbt compatible package
  override def artifactName = "chiseltest"
}
