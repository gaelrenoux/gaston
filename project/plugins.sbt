logLevel := Level.Warn

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.7")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")