import sbt.Keys._

name := "gaston"
version := "0.1-SNAPSHOT"

scalaVersion := "2.12.8"
scalacOptions ++= Seq(
  "-Ypartial-unification", // Enable partial unification in type constructor inference
  "-explaintypes", // Explain type errors in more detail.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.

  "-feature", // Emit warning and location for usages of features that should be imported explicitly
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",

  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened
  "-Ywarn-unused", // Warn when local and private vals, vars, defs, and types are are unused
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused-import", // Warn when imports are unused
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  
  "-deprecation", // Emit warning and location for usages of deprecated APIs
  "-unchecked" // Enable additional warnings where generated code depends on assumptions

)

libraryDependencies ++= Seq(

  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",

  "com.typesafe" % "config" % "1.3.3",
  "com.github.pureconfig" %% "pureconfig" % "0.10.1",
  "com.github.scopt" %% "scopt" % "3.7.1",

  "org.scalaz" %% "scalaz-core" % "7.2.27",
  
  "ai.x" %% "diff" % "2.0.1",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

mainClass in assembly := Some("fr.renoux.gaston.command.Main")