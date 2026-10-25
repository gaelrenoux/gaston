import sbt.Keys._

organization := "gael.renoux"
name := "gaston"
version := "1.2.0"

/* Allows to overwrite with publishLocal */
isSnapshot := true

scalaVersion := "3.9.0"

lazy val gaston = (project in file("."))
    .configs(TestSlow)
    .settings(inConfig(TestSlow)(Defaults.testSettings) *)

/* Those tests are much slower */
lazy val TestSlow = config("test-slow") extend (Test)

scalacOptions ++= Seq(
  "-no-indent", // Remove the significant-indentation syntax

  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-Werror", // Fail the compilation if there are any warnings.
  "-explain", // Explain type errors in more detail.

  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.

  "-Wsafe-init", // Wrap field accessors to throw an exception on uninitialized access.

  "-Wunused:explicits", // Warn if an explicit parameter is unused.
  "-Wunused:implicits", // Warn if an implicit parameter is unused.
  "-Wunused:imports", // Warn when imports are unused.
  "-Wunused:locals", // Warn if a local definition is unused.
  // "-Wunused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Wunused:privates", // Warn if a private member is unused.
  "-Wvalue-discard" // Warn when non-Unit expression results are unused.

  // "-XX:MaxInlineLevel=18", // see https://github.com/scala/bug/issues/11627#issuecomment-514619316 // check if still valid for Scala 3
)

/* Adds Sonatype snapshots, required for Iron's snapshot version */
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

val catsVersion = "2.13.0"
val ironVersion = "3.4.1"

libraryDependencies ++= Seq(

  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback" % "logback-classic" % "1.6.5",

  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "alleycats-core" % catsVersion,
  "org.typelevel" %% "mouse" % "1.4.0",

  "com.typesafe" % "config" % "1.4.9",
  "com.github.pureconfig" %% "pureconfig-core" % "0.17.11",
  "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.11",
  "org.typelevel" %% "shapeless3-deriving" % "3.6.0",
  "com.github.scopt" %% "scopt" % "4.2.0",

  "io.github.iltotore" %% "iron" % ironVersion,
  "io.github.iltotore" %% "iron-pureconfig" % ironVersion,

  "com.softwaremill.quicklens" %% "quicklens" % "1.9.15",

  /* Tests */
  "com.github.jatcwang" %% "difflicious-scalatest" % "0.7.1" % Test,
  "org.scalatest" %% "scalatest" % "3.2.20" % Test
)


assembly / mainClass := Some("fr.renoux.gaston.command.Main")
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _ => MergeStrategy.first
}

Test / fork := true
Test / testOptions += Tests.Argument("-oD") // show test duration

/* Stays inside the sbt console when we press "ctrl-c" in tests" */
Global / cancelable := true

/* Exposes the project's version to the code (displayed by the command line) */
Compile / sourceGenerators += Def.task {
  val file = (Compile / sourceManaged).value / "fr" / "renoux" / "gaston" / "BuildInfo.scala"
  IO.write(file,
    s"""package fr.renoux.gaston
       |
       |object BuildInfo {
       |  val Version: String = "${version.value}"
       |}
       |""".stripMargin)
  Seq(file)
}.taskValue
