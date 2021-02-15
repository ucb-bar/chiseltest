// SPDX-License-Identifier: Apache-2.0

organization := "edu.berkeley.cs"
name := "chiseltest"

version := "0.5-SNAPSHOT"

scalaVersion := "2.12.13"

crossScalaVersions := Seq("2.12.13")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2",
  "com.lihaoyi" %% "utest" % "latest.integration"
)

testFrameworks += new TestFramework("utest.runner.Framework")

publishMavenStyle := true

publishArtifact in Test := false
pomIncludeRepository := { x => false }

pomExtra := (
  <url>http://chisel.eecs.berkeley.edu/</url>
  <licenses>
    <license>
      <name>apache_v2</name>
      <url>https://opensource.org/licenses/Apache-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
<scm>
  <url>https://github.com/ucb-bar/chisel-testers2.git</url>
  <connection>scm:git:github.com/ucb-bar/chisel-testers2.git</connection>
</scm>
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
val defaultVersions = Seq(
  "chisel3" -> "3.5-SNAPSHOT",
  "treadle" -> "1.5-SNAPSHOT"
)

libraryDependencies ++= defaultVersions.map { case (dep, ver) =>
  "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", ver)
}


scalacOptions ++= Seq("-deprecation", "-feature", "-language:reflectiveCalls")

// Scala 2.12 requires Java 8.
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
