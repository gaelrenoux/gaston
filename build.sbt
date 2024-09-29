import sbt.Keys._

organization := "gael.renoux"
name := "gaston"
version := "1.0.0"

/* Allows to overwrite with publishLocal */
isSnapshot := true

scalaVersion := "2.13.15"

lazy val gaston = (project in file("."))
  .configs(SlowTest)
  .settings(inConfig(SlowTest)(Defaults.testSettings): _*)

/* Those tests are much slower */
lazy val SlowTest = config("test-slow") extend (Test)

scalacOptions ++= {
  if (scalaVersion.value.startsWith("3.")) scala3Options
  else scala2Options
}

lazy val scala2Options = Seq(
  "-Xsource:3",

  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",

  // "-XX:MaxInlineLevel=18", // see https://github.com/scala/bug/issues/11627#issuecomment-514619316

  "-explaintypes", // Explain type errors in more detail.
  "-Werror", // Fail the compilation if there are any warnings.

  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  //  "-Xdev", // Indicates user is a developer - issue warnings about anything which seems amiss

  "-Wunused:explicits", // Warn if an explicit parameter is unused.
  "-Wunused:implicits", // Warn if an implicit parameter is unused.
  "-Wunused:imports", // Warn when imports are unused.
  "-Wunused:locals", // Warn if a local definition is unused.
  "-Wunused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Wunused:privates", // Warn if a private member is unused.
  "-Wvalue-discard", // Warn when non-Unit expression results are unused.
)

lazy val scala3Options = Seq(
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",

  "-Werror", // Fail the compilation if there are any warnings.
  "-explain", // Explain type errors in more detail.

  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.

  "-Ysafe-init", // Wrap field accessors to throw an exception on uninitialized access.

  "-Wunused:explicits", // Warn if an explicit parameter is unused.
  "-Wunused:implicits", // Warn if an implicit parameter is unused.
  "-Wunused:imports", // Warn when imports are unused.
  "-Wunused:locals", // Warn if a local definition is unused.
  "-Wunused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Wunused:privates", // Warn if a private member is unused.
  "-Wvalue-discard", // Warn when non-Unit expression results are unused.

  // "-XX:MaxInlineLevel=18", // see https://github.com/scala/bug/issues/11627#issuecomment-514619316 // check if still valid for Scala 3
)

val catsVersion = "2.12.0"
val refinedVersion = "0.11.2"

libraryDependencies ++= Seq(

  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback" % "logback-classic" % "1.5.8",

  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "alleycats-core" % catsVersion,
  "org.typelevel" %% "mouse" % "1.3.2",

  "com.typesafe" % "config" % "1.4.3",
  // "com.github.pureconfig" %% "pureconfig" % "0.17.7",
  "com.github.scopt" %% "scopt" % "4.1.0",

  "eu.timepit" %% "refined" % refinedVersion,
  "eu.timepit" %% "refined-pureconfig" % refinedVersion,
  "eu.timepit" %% "refined-scopt" % refinedVersion,

  "com.softwaremill.quicklens" %% "quicklens" % "1.9.9",

  /* Tests */
  "com.softwaremill.diffx" %% "diffx-core" % "0.9.0" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

libraryDependencies += {
  if (scalaVersion.value.startsWith("3.")) "com.github.pureconfig" %% "pureconfig-core" % "0.17.7"
  else "com.github.pureconfig" %% "pureconfig" % "0.17.7"
}

assembly / mainClass := Some("fr.renoux.gaston.command.Main")

Test / fork := true
Test / testOptions += Tests.Argument("-oD") // show test duration

/* Stays inside the sbt console when we press "ctrl-c" in tests" */
Global / cancelable := true
