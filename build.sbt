/*
scalafmt: {
  style = defaultWithAlign
  maxColumn = 150
  align.tokens = [
    { code = "=>", owner = "Case" }
    { code = "?", owner = "Case" }
    { code = "extends", owner = "Defn.(Class|Trait|Object)" }
    { code = "//", owner = ".*" }
    { code = "{", owner = "Template" }
    { code = "}", owner = "Template" }
    { code = ":=", owner = "Term.ApplyInfix" }
    { code = "++=", owner = "Term.ApplyInfix" }
    { code = "+=", owner = "Term.ApplyInfix" }
    { code = "%", owner = "Term.ApplyInfix" }
    { code = "%%", owner = "Term.ApplyInfix" }
    { code = "%%%", owner = "Term.ApplyInfix" }
    { code = "->", owner = "Term.ApplyInfix" }
    { code = "?", owner = "Term.ApplyInfix" }
    { code = "<-", owner = "Enumerator.Generator" }
    { code = "?", owner = "Enumerator.Generator" }
    { code = "=", owner = "(Enumerator.Val|Defn.(Va(l|r)|Def|Type))" }
  ]
}
 */

// Dependency versions
val akkaHttpVersion   = "10.1.1"
val akkaStreamVersion = "2.5.12"
val catsVersion       = "1.1.0"
val circeVersion      = "0.9.3"
val parboiledVersion  = "2.1.4"
val jenaVersion       = "3.7.0"
val scalaGraphVersion = "1.12.5"
val scalaTestVersion  = "3.0.5"

// Dependency modules
lazy val akkaHttpCore = "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion
lazy val akkaStream   = "com.typesafe.akka" %% "akka-stream"    % akkaStreamVersion
lazy val catsCore     = "org.typelevel"     %% "cats-core"      % catsVersion
lazy val circeCore    = "io.circe"          %% "circe-core"     % circeVersion
lazy val jenaCore     = "org.apache.jena"   % "jena-core"       % jenaVersion
lazy val jenaArq      = "org.apache.jena"   % "jena-arq"        % jenaVersion
lazy val parboiled2   = "org.parboiled"     %% "parboiled"      % parboiledVersion
lazy val scalaGraph   = "org.scala-graph"   %% "graph-core"     % scalaGraphVersion
lazy val scalaTest    = "org.scalatest"     %% "scalatest"      % scalaTestVersion

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name                := "rdf-core",
    moduleName          := "rdf-core",
    libraryDependencies ++= Seq(catsCore, parboiled2, scalaGraph, scalaTest % Test)
  )

lazy val circe = project
  .in(file("modules/circe"))
  .dependsOn(core)
  .settings(
    name                := "rdf-circe",
    moduleName          := "rdf-circe",
    libraryDependencies ++= Seq(circeCore, scalaTest % Test)
  )

lazy val jena = project
  .in(file("modules/jena"))
  .dependsOn(circe)
  .settings(
    name                := "rdf-jena",
    moduleName          := "rdf-jena",
    libraryDependencies ++= Seq(jenaCore, jenaArq, scalaTest % Test)
  )

lazy val akka = project
  .in(file("modules/akka"))
  .dependsOn(core)
  .settings(
    name                := "rdf-akka",
    moduleName          := "rdf-akka",
    libraryDependencies ++= Seq(akkaHttpCore, akkaStream, scalaTest % Test)
  )

lazy val root = project
  .in(file("."))
  .settings(noPublish)
  .settings(
    name       := "rdf",
    moduleName := "rdf"
  )
  .aggregate(core, circe, jena, akka)

/* ********************************************************
 ******************** Grouped Settings ********************
 **********************************************************/

lazy val noPublish = Seq(
  publishLocal    := {},
  publish         := {},
  publishArtifact := false,
)

inThisBuild(
  List(
    homepage := Some(url("https://github.com/BlueBrain/nexus-rdf")),
    licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    scmInfo  := Some(ScmInfo(url("https://github.com/BlueBrain/nexus-rdf"), "scm:git:git@github.com:BlueBrain/nexus-rdf.git")),
    developers := List(
      Developer("bogdanromanx", "Bogdan Roman", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("hygt", "Henry Genet", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("umbreak", "Didac Montero Mendez", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("wwajerowicz", "Wojtek Wajerowicz", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
    ),
    // These are the sbt-release-early settings to configure
    releaseEarlyWith              := BintrayPublisher,
    releaseEarlyNoGpg             := true,
    releaseEarlyEnableSyncToMaven := false,
  ))

addCommandAlias("review", ";clean;scalafmtCheck;test:scalafmtCheck;scalafmtSbtCheck;coverage;scapegoat;test;coverageReport;coverageAggregate")
