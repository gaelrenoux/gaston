import sbt.Keys._

organization := "gael.renoux"
name := "gaston"
version := "0.3.0"

scalaVersion := "2.12.8" //waiting on artima supersafe and refined (and a little ai.x.diff, but it's not a big deal)

scalacOptions ++= Seq(

  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-Ypartial-unification", // Enable partial unification in type constructor inference. Needed for Scalaz/Cats.
  "-explaintypes", // Explain type errors in more detail.
  "-Xfatal-warnings", // Fail the compilation if there are any warnings.

  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.

  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn when imports are unused.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.

  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:private-shadow", // Warn when a private field (or class parameter) shadows a superclass field.
  "-Xlint:type-parameter-shadow", // Warn when a local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Warn when a pattern is not typesafe.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
)

libraryDependencies ++= Seq(

  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",

  "com.typesafe" % "config" % "1.3.4",
  "com.github.pureconfig" %% "pureconfig" % "0.11.1",
  "com.github.scopt" %% "scopt" % "3.7.1",

  "org.scalaz" %% "scalaz-core" % "7.2.28",

  "eu.timepit" %% "refined" % "0.9.8",
  "eu.timepit" %% "refined-scalaz" % "0.9.8",
  "eu.timepit" %% "refined-pureconfig" % "0.9.8",
  "eu.timepit" %% "refined-scopt" % "0.9.8",

  "ai.x" %% "diff" % "2.0.1",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

mainClass in assembly := Some("fr.renoux.gaston.command.Main")

/* Stays inside the sbt console when we press "ctrl-c" in tests" */
fork in Test := true
Test / run / fork := true
Global / cancelable := true