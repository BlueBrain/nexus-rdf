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
val akkaActorVersion = "2.6.1"
val akkaHttpVersion  = "10.1.11"
val catsVersion      = "2.1.0"
val circeVersion     = "0.12.3"
val jenaVersion      = "3.13.1"
val magnoliaVersion  = "0.12.6"
val parboiledVersion = "2.1.8"
val scalaTestVersion = "3.1.0"
val topBraidVersion  = "1.3.1"

// Dependency modules
lazy val akkaActor     = "com.typesafe.akka" %% "akka-actor"     % akkaActorVersion
lazy val akkaHttpCore  = "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion
lazy val alleycatsCore = "org.typelevel"     %% "alleycats-core" % catsVersion
lazy val catsCore      = "org.typelevel"     %% "cats-core"      % catsVersion
lazy val circeCore     = "io.circe"          %% "circe-core"     % circeVersion
lazy val circeParser   = "io.circe"          %% "circe-parser"   % circeVersion
lazy val circeLiteral  = "io.circe"          %% "circe-literal"  % circeVersion
lazy val jenaArq       = "org.apache.jena"   % "jena-arq"        % jenaVersion
lazy val magnolia      = "com.propensive"    %% "magnolia"       % magnoliaVersion
lazy val parboiled2    = "org.parboiled"     %% "parboiled"      % parboiledVersion
lazy val scalaTest     = "org.scalatest"     %% "scalatest"      % scalaTestVersion
lazy val topBraidShacl = "org.topbraid"      % "shacl"           % topBraidVersion

lazy val core = project
  .in(file("modules/core"))
  .settings(
    name       := "rdf-core",
    moduleName := "rdf-core",
    libraryDependencies ++= Seq(
      alleycatsCore,
      catsCore,
      parboiled2,
      circeCore    % Test,
      circeParser  % Test,
      circeLiteral % Test,
      jenaArq      % Test,
      scalaTest    % Test
    )
  )

lazy val derivation = project
  .in(file("modules/derivation"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name       := "rdf-derivation",
    moduleName := "rdf-derivation",
    libraryDependencies ++= Seq(
      magnolia,
      scalaTest % Test
    )
  )

lazy val jsonld = project
  .in(file("modules/jsonld"))
  .dependsOn(core % "compile->compile;test->test", jena)
  .settings(
    name       := "rdf-jsonld",
    moduleName := "rdf-jsonld",
    libraryDependencies ++= Seq(
      circeCore,
      circeParser,
      circeLiteral % Test,
      scalaTest    % Test
    )
  )

lazy val jena = project
  .in(file("modules/jena"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    name                     := "rdf-jena-compat",
    moduleName               := "rdf-jena-compat",
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      jenaArq,
      scalaTest % Test
    )
  )

lazy val shacl = project
  .in(file("modules/shacl"))
  .dependsOn(jena)
  .settings(
    name       := "rdf-shacl",
    moduleName := "rdf-shacl",
    libraryDependencies ++= Seq(
      topBraidShacl,
      scalaTest % Test
    )
  )

lazy val akka = project
  .in(file("modules/akka"))
  .dependsOn(core % "test->test;compile->compile")
  .settings(
    name       := "rdf-akka-compat",
    moduleName := "rdf-akka-compat",
    libraryDependencies ++= Seq(
      akkaActor,
      akkaHttpCore,
      scalaTest % Test
    )
  )

lazy val bench = project
  .in(file("modules/bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(core, jsonld, jena)
  .settings(noPublish)
  .settings(
    name                       := "rdf-bench",
    moduleName                 := "rdf-bench",
    sourceDirectory in Jmh     := (sourceDirectory in Test).value,
    classDirectory in Jmh      := (classDirectory in Test).value,
    dependencyClasspath in Jmh := (dependencyClasspath in Test).value,
    compile in Jmh             := (compile in Jmh).dependsOn(compile in Test).value,
    run in Jmh                 := (run in Jmh).dependsOn(Keys.compile in Jmh).evaluated,
    libraryDependencies ++= Seq(
      scalaTest % Test
    )
  )

lazy val rdf = project
  .in(file("."))
  .aggregate(core, derivation, jsonld, jena, shacl, akka, bench)
  .settings(noPublish)
  .settings(
    name       := "rdf",
    moduleName := "rdf"
  )

lazy val noPublish = Seq(publishLocal := {}, publish := {}, publishArtifact := false)

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
