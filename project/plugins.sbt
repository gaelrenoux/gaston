logLevel := Level.Warn

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.2")
//remember to add Artima resolver to ~/.sbt/013/global.sbt: resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.6")