// SPDX-License-Identifier: Apache-2.0

import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._
import mill.scalalib.publish._
import coursier.maven.MavenRepository

object chiseltest extends mill.Cross[chiseltestCrossModule]("2.12.13")

val defaultVersions = Map(
  "chisel3" -> "3.6-SNAPSHOT",
  "treadle" -> "1.6-SNAPSHOT"
)

def getVersion(dep: String, org: String = "edu.berkeley.cs") = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  ivy"$org::$dep:$version"
}

class chiseltestCrossModule(val crossScalaVersion: String)
    extends CrossSbtModule
    with PublishModule
    with ScalafmtModule {
  override def repositoriesTask = T.task {
    super.repositoriesTask() ++ Seq(
      MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
    )
  }

  def chisel3Module: Option[PublishModule] = None

  def chisel3IvyDeps = if (chisel3Module.isEmpty)
    Agg(
      getVersion("chisel3")
    )
  else Agg.empty[Dep]

  def chisel3PluginIvyDeps = if (chisel3Module.isEmpty) {
    Agg(ivy"edu.berkeley.cs:::chisel3-plugin:${defaultVersions("chisel3")}")
  } else Agg.empty[Dep]

  def treadleModule: Option[PublishModule] = None

  def treadleIvyDeps = if (treadleModule.isEmpty)
    Agg(
      getVersion("treadle")
    )
  else Agg.empty[Dep]

  override def millSourcePath = super.millSourcePath / os.up

  // 2.12.12 -> Array("2", "12", "12") -> "12" -> 12
  private def majorVersion = crossScalaVersion.split('.')(1).toInt

  def publishVersion = "0.6-SNAPSHOT"

  override def scalacOptions = T {
    super.scalacOptions() ++ Seq(
      "-deprecation",
      "-feature",
      "-language:reflectiveCalls" // required by SemanticDB compiler plugin
    )
  }

  override def javacOptions = T {
    super.javacOptions() ++ Seq("-source", "1.8", "-target", "1.8")
  }

  override def moduleDeps = super.moduleDeps ++ chisel3Module ++ treadleModule

  override def ivyDeps = T {
    Agg(
      ivy"org.scalatest::scalatest:3.2.12",
      ivy"com.lihaoyi::utest:0.7.11",
      ivy"net.java.dev.jna:jna:5.11.0"
    ) ++ chisel3IvyDeps ++ treadleIvyDeps
  }

  override def scalacPluginIvyDeps = T { chisel3PluginIvyDeps }

  def test = T {
    T.sequence(Seq(test_1.test(), test_2.test()))
  }

  object test_1 extends Tests with TestModule.ScalaTest with ScalafmtModule {
    override def ivyDeps = T { chisel3IvyDeps ++ treadleIvyDeps }
  }

  object test_2 extends Tests with TestModule.Utest with ScalafmtModule {
    override def ivyDeps = T { chisel3IvyDeps ++ treadleIvyDeps }
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
