// SPDX-License-Identifier: Apache-2.0

organization := "edu.berkeley.cs"
name := "chiseltest"

// we keep in sync with chisel version names
version := "5.0.1"

scalaVersion := "2.13.10"

crossScalaVersions := Seq("2.13.10")

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
  "chisel3" -> "5.0.0",
  "firrtl" -> "5.0.0",
)

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-Xcheckinit",
  // do not warn about firrtl imports, once the firrtl repo is removed, we will need to import the code
  "-Wconf:cat=deprecation&msg=Importing from firrtl is deprecated:s",
  // do not warn about firrtl deprecations
  "-Wconf:cat=deprecation&msg=will not be supported as part of the migration to the MLIR-based FIRRTL Compiler:s",
) ++ {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 13 => Seq("-Ymacro-annotations")
    case _                       => Nil
  }
}

libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel" % defaultVersions("chisel3"),
  "edu.berkeley.cs" %% "firrtl" % defaultVersions("firrtl"),
  "org.scalatest" %% "scalatest" % "3.2.15",
  "com.lihaoyi" %% "utest" % "0.8.1",
  "net.java.dev.jna" % "jna" % "5.13.0",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "com.lihaoyi" %% "os-lib" % "0.8.1",
  compilerPlugin(("org.chipsalliance" % "chisel-plugin" % defaultVersions("chisel3")).cross(CrossVersion.full))
) ++ {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 13 => Nil
    case _ =>
      Seq(
        compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full))
      )
  }
}
