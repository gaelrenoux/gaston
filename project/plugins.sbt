logLevel := Level.Warn

// Required in your global.sbt: `resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"`
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.10")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")

/* Quality control */
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
// addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "1.0.9")

/* Debugging utilities */
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")
