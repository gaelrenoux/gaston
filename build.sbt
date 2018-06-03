import sbt.Keys._

name := "gaston"
version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"
scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs
  "-feature", // Emit warning and location for usages of features that should be imported explicitly
  "-unchecked", // Enable additional warnings where generated code depends on assumptions
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-Ywarn-numeric-widen", // Warn when numerics are widened
  "-Ywarn-unused", // Warn when local and private vals, vars, defs, and types are are unused
  "-Ywarn-unused-import", // Warn when imports are unused
  "-Ywarn-value-discard" // Warn when non-Unit expression results are unused
)

libraryDependencies ++= Seq(

  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7",

  "com.typesafe" % "config" % "1.3.1",
  "com.github.pureconfig" %% "pureconfig" % "0.8.0",
  "com.github.scopt" %% "scopt" % "3.7.0",

  "org.scalaz" %% "scalaz-core" % "7.2.19",

  //"org.scalactic" %% "scalactic" % "3.0.1",
  "ai.x" %% "diff" % "2.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

mainClass in assembly := Some("fr.renoux.gaston.runner.Main")