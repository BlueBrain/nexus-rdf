resolvers += Resolver.bintrayRepo("bbp", "nexus-releases")

addSbtPlugin("ch.epfl.bluebrain.nexus" % "sbt-nexus" % "0.10.6")
addSbtPlugin("ch.epfl.scala"           % "sbt-bloop" % "1.0.0-M8")
