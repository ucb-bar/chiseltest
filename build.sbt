// SPDX-License-Identifier: Apache-2.0

organization := "edu.berkeley.cs"
name := "chiseltest"

version := "0.6-SNAPSHOT"

scalaVersion := "2.13.10"

crossScalaVersions := Seq("2.12.17", "2.13.10")

resolvers ++= Resolver.sonatypeOssRepos("snapshots")
resolvers ++= Resolver.sonatypeOssRepos("releases")

testFrameworks += new TestFramework("utest.runner.Framework")

publishMavenStyle := true

Test / publishArtifact := false
pomIncludeRepository := { x => false }

// scm is set by sbt-ci-release
pomExtra := (
  <url>http://chisel.eecs.berkeley.edu/</url>
  <licenses>
    <license>
      <name>apache_v2</name>
      <url>https://opensource.org/licenses/Apache-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
<developers>
  <developer>
    <id>ducky64</id>
    <name>Richard Lin</name>
  </developer>
</developers>
)

publishTo := {
  val v = version.value
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots".at(nexus + "content/repositories/snapshots"))
  } else {
    Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
  }
}

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "chisel3" -> "3.6-SNAPSHOT",
  "treadle" -> "1.6-SNAPSHOT"
)

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-Xcheckinit",
  "-Wconf:msg=firrtl:s", // do not warn about firrtl imports
) ++ {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 13 => Seq("-Ymacro-annotations")
    case _                       => Nil
  }
}

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % defaultVersions("chisel3"),
  "edu.berkeley.cs" %% "treadle" % defaultVersions("treadle"),
  "org.scalatest" %% "scalatest" % "3.2.14",
  "com.lihaoyi" %% "utest" % "0.8.1",
  "net.java.dev.jna" % "jna" % "5.12.1",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  compilerPlugin(("edu.berkeley.cs" % "chisel3-plugin" % defaultVersions("chisel3")).cross(CrossVersion.full))
) ++ {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 13 => Nil
    case _ =>
      Seq(
        compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full))
      )
  }
}
