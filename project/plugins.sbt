resolvers += Resolver.bintrayRepo("bbp", "nexus-releases")

addSbtPlugin("ch.epfl.bluebrain.nexus" % "sbt-nexus" % "0.12.1")
addSbtPlugin("pl.project13.scala"      % "sbt-jmh"   % "0.3.7")
