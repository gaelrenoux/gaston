import sbt.Keys._

organization := "gael.renoux"
name := "gaston"
version := "1.1.0"

/* Allows to overwrite with publishLocal */
isSnapshot := true

scalaVersion := "3.5.2"

lazy val gaston = (project in file("."))
  .configs(SlowTest)
  .settings(inConfig(SlowTest)(Defaults.testSettings)*)

/* Those tests are much slower */
lazy val SlowTest = config("test-slow") extend (Test)

scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",

  // "-Werror", // Fail the compilation if there are any warnings. // TODO restore once Scala 3 migration is done
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
  "-Wvalue-discard", // Warn when non-Unit expression results are unused.

  // "-XX:MaxInlineLevel=18", // see https://github.com/scala/bug/issues/11627#issuecomment-514619316 // check if still valid for Scala 3
)

/* Adds Sonatype snapshots, required for Iron's snapshot version */
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

val catsVersion = "2.12.0"
val ironVersion = "2.6.0-12-a077d1-SNAPSHOT" // TODO Move to 2.7 as soon as it's released

libraryDependencies ++= Seq(

  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback" % "logback-classic" % "1.5.8",

  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "alleycats-core" % catsVersion,
  "org.typelevel" %% "mouse" % "1.3.2",

  "com.typesafe" % "config" % "1.4.3",
  "com.github.pureconfig" %% "pureconfig-core" % "0.17.7",
  "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.7",
  "org.typelevel" %% "shapeless3-deriving" % "3.4.3",
  "com.github.scopt" %% "scopt" % "4.1.0",

  "io.github.iltotore" %% "iron" % ironVersion,
  "io.github.iltotore" %% "iron-pureconfig" % ironVersion,

  "com.softwaremill.quicklens" %% "quicklens" % "1.9.9",

  /* Tests */
  "com.github.jatcwang" %% "difflicious-scalatest" % "0.4.3" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)


assembly / mainClass := Some("fr.renoux.gaston.command.Main")
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}

Test / fork := true
Test / testOptions += Tests.Argument("-oD") // show test duration

/* Stays inside the sbt console when we press "ctrl-c" in tests" */
Global / cancelable := true
