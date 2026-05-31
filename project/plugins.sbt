logLevel := Level.Warn

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.2.0")

/* Quality control */
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.3.1")
addSbtPlugin("io.stryker-mutator" % "sbt-stryker4s" % "0.20.4")

// addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "1.0.9")
