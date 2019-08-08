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
val akkaHttpVersion      = "10.1.9"
val catsVersion          = "1.6.1"
val circeVersion         = "0.11.1"
val parboiledVersion     = "2.1.8"
val jenaVersion          = "3.12.0"
val scalaGraphVersion    = "1.12.5"
val scalaGraphDotVersion = "1.12.1"
val scalaTestVersion     = "3.0.8"

// Dependency modules
lazy val akkaHttpCore  = "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion
lazy val catsCore      = "org.typelevel"     %% "cats-core"      % catsVersion
lazy val circeCore     = "io.circe"          %% "circe-core"     % circeVersion
lazy val circeParser   = "io.circe"          %% "circe-parser"   % circeVersion
lazy val jenaCore      = "org.apache.jena"   % "jena-core"       % jenaVersion
lazy val jenaArq       = "org.apache.jena"   % "jena-arq"        % jenaVersion
lazy val parboiled2    = "org.parboiled"     %% "parboiled"      % parboiledVersion
lazy val scalaGraph    = "org.scala-graph"   %% "graph-core"     % scalaGraphVersion
lazy val scalaGraphDot = "org.scala-graph"   %% "graph-dot"      % scalaGraphDotVersion
lazy val scalaTest     = "org.scalatest"     %% "scalatest"      % scalaTestVersion

lazy val root = project
  .in(file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    name       := "rdf",
    moduleName := "rdf",
    libraryDependencies ++= Seq(
      catsCore,
      circeCore,
      circeParser,
      jenaCore,
      jenaArq,
      parboiled2,
      scalaGraph,
      scalaGraphDot,
      akkaHttpCore % Test,
      scalaTest    % Test
    ),
    sourceDirectory in Jmh     := (sourceDirectory in Test).value,
    classDirectory in Jmh      := (classDirectory in Test).value,
    dependencyClasspath in Jmh := (dependencyClasspath in Test).value,
    compile in Jmh             := (compile in Jmh).dependsOn(compile in Test).value,
    run in Jmh                 := (run in Jmh).dependsOn(Keys.compile in Jmh).evaluated
  )

/* ********************************************************
 ******************** Grouped Settings ********************
 **********************************************************/

inThisBuild(
  List(
    homepage := Some(url("https://github.com/BlueBrain/nexus-rdf")),
    licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    scmInfo  := Some(ScmInfo(url("https://github.com/BlueBrain/nexus-rdf"), "scm:git:git@github.com:BlueBrain/nexus-rdf.git")),
    developers := List(
      Developer("bogdanromanx", "Bogdan Roman", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("hygt", "Henry Genet", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("umbreak", "Didac Montero Mendez", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/")),
      Developer("wwajerowicz", "Wojtek Wajerowicz", "noreply@epfl.ch", url("https://bluebrain.epfl.ch/"))
    ),
    // These are the sbt-release-early settings to configure
    releaseEarlyWith              := BintrayPublisher,
    releaseEarlyNoGpg             := true,
    releaseEarlyEnableSyncToMaven := false
  )
)

addCommandAlias("review", ";clean;scalafmtCheck;test:scalafmtCheck;scalafmtSbtCheck;coverage;scapegoat;test;coverageReport;coverageAggregate")
