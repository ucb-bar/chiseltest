// SPDX-License-Identifier: Apache-2.0

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  scalaVersion := "2.13.10",
  crossScalaVersions := Seq("2.13.10")
)

val chiselVersion = "5.1.0"
val firrtlVersion = "5.0.0"

lazy val chiseltestSettings = Seq(
  name := "chiseltest",
  // we keep in sync with chisel version names
  version := "5.1.0",
  scalacOptions := Seq(
    "-deprecation",
    "-feature",
    "-language:reflectiveCalls",
    // do not warn about firrtl imports, once the firrtl repo is removed, we will need to import the code
    "-Wconf:cat=deprecation&msg=Importing from firrtl is deprecated:s",
    // do not warn about firrtl deprecations
    "-Wconf:cat=deprecation&msg=will not be supported as part of the migration to the MLIR-based FIRRTL Compiler:s"
  ),
  // Always target Java8 for maximum compatibility
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  libraryDependencies ++= Seq(
    "org.chipsalliance" %% "chisel" % chiselVersion,
    "edu.berkeley.cs" %% "firrtl" % firrtlVersion,
    "org.scalatest" %% "scalatest" % "3.2.17",
    "com.lihaoyi" %% "utest" % "0.8.1",
    "net.java.dev.jna" % "jna" % "5.13.0",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "com.lihaoyi" %% "os-lib" % "0.8.1",
    compilerPlugin(("org.chipsalliance" % "chisel-plugin" % chiselVersion).cross(CrossVersion.full))
  ),
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  resolvers ++= Resolver.sonatypeOssRepos("releases"),
  testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { x => false },
  // scm is set by sbt-ci-release
  pomExtra :=
    <url>https://github.com/ucb-bar/chiseltest/</url>
      <licenses>
        <license>
          <name>Apache-2.0</name>
          <url>https://opensource.org/licenses/Apache-2.0</url>
          <distribution>repo</distribution>
        </license>
        <license>
          <name>BSD-3-Clause</name>
          <url>https://opensource.org/licenses/BSD-3-Clause</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <developers>
        <developer>
          <id>ekiwi</id>
          <name>Kevin Laeufer</name>
          <email>laeufer@berkeley.edu</email>
        </developer>
        <developer>
          <id>ducky64</id>
          <name>Richard Lin</name>
        </developer>
      </developers>,
  publishTo := {
    val v = version.value
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) {
      Some("snapshots".at(nexus + "content/repositories/snapshots"))
    } else {
      Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
    }
  }
)

lazy val chiseltest = (project in file("."))
  .settings(commonSettings)
  .settings(chiseltestSettings)
  .settings(publishSettings)
