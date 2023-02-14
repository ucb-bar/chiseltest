// SPDX-License-Identifier: Apache-2.0

organization := "edu.berkeley.cs"
name := "chiseltest"

version := "0.5-SNAPSHOT"

scalaVersion := "2.12.15"

crossScalaVersions := Seq("2.12.15", "2.13.7")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

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
  "chisel3" -> "3.5-SNAPSHOT",
  "treadle" -> "1.5-SNAPSHOT"
)

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
<<<<<<< HEAD
  "-Xcheckinit"
=======
  "-Xcheckinit",
  // do not warn about firrtl imports, once the firrtl repo is removed, we will need to import the code
  "-Wconf:cat=deprecation&msg=Importing from firrtl is deprecated:s",
  // do not warn about firrtl deprecations
  "-Wconf:cat=deprecation&msg=will not be supported as part of the migration to the MLIR-based FIRRTL Compiler:s",
  // TODO: remove FixedPoint support after 3.6 release
  "-Wconf:cat=deprecation&msg=class FixedPoint:s",
  "-Wconf:cat=deprecation&msg=class BinaryPoint:s",
  "-Wconf:cat=deprecation&msg=object FixedPoint:s",
  "-Wconf:cat=deprecation&msg=class fromDoubleToLiteral:s",
  "-Wconf:cat=deprecation&msg=class fromBigDecimalToLiteral:s",
  "-Wconf:cat=deprecation&msg=trait HasBinaryPoint:s",
  "-Wconf:cat=deprecation&msg=object UnknownBinaryPoint:s",
  // TODO: remove Interval support after 3.6 release
  "-Wconf:cat=deprecation&msg=class Interval:s",
  "-Wconf:cat=deprecation&msg=object Interval:s",
  "-Wconf:cat=deprecation&msg=class fromBigDecimalToLiteralInterval:s",
  "-Wconf:cat=deprecation&msg=class fromBigIntToLiteralInterval:s"
>>>>>>> fed4822 (Address chisel3 changes to DataMirror, AutoCloneType and new deprecation warnings (#619))
) ++ {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 13 => Seq("-Ymacro-annotations")
    case _                       => Nil
  }
}

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % defaultVersions("chisel3"),
  "edu.berkeley.cs" %% "treadle" % defaultVersions("treadle"),
  "org.scalatest" %% "scalatest" % "3.1.4",
  "com.lihaoyi" %% "utest" % "0.7.9",
  "net.java.dev.jna" % "jna" % "5.10.0",
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
