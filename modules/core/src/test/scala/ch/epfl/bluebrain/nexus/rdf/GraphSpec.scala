package ch.epfl.bluebrain.nexus.rdf

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.BNode
import ch.epfl.bluebrain.nexus.rdf.implicits._

class GraphSpec extends RdfSpec {

  "Graph" should {

    "create correct DOT representation" in {

      val jDoe  = url"http://nexus.example.com/john-doe"
      val other = url"http://nexus.example.com/other"
      val graph = Graph(
        jDoe,
        Set(
          (jDoe, url"http://schema.org/name", "John Doe"),
          (jDoe, url"http://example.com/stringProperty", "Some property"),
          (jDoe, url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "http://schema.org/Person"),
          (jDoe, url"http://schema.org/birthDate", "1999-04-09T20:00Z"),
          (jDoe, url"http://schema.org/deprecated", false),
          (other, url"http://schema.org/birthDate", "2000-04-12T20:00Z"),
          (other, url"http://schema.org/height", 9.223),
          (other, url"http://schema.org/birthYear", 1999),
          (other, url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "http://schema.org/Other"),
          (other, url"http://schema.org/birthHour", 9),
          (jDoe, url"http://example.com/sibling", other)
        )
      )
      val expected =
        """digraph "http://nexus.example.com/john-doe" {
            |  "http://nexus.example.com/john-doe" -> "John Doe" [label = "http://schema.org/name"]
            |  "http://nexus.example.com/john-doe" -> "Some property" [label = "http://example.com/stringProperty"]
            |  "http://nexus.example.com/john-doe" -> "http://schema.org/Person" [label = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
            |  "http://nexus.example.com/john-doe" -> "1999-04-09T20:00Z" [label = "http://schema.org/birthDate"]
            |  "http://nexus.example.com/john-doe" -> false [label = "http://schema.org/deprecated"]
            |  "http://nexus.example.com/other" -> "2000-04-12T20:00Z" [label = "http://schema.org/birthDate"]
            |  "http://nexus.example.com/other" -> 9.223 [label = "http://schema.org/height"]
            |  "http://nexus.example.com/other" -> 1999 [label = "http://schema.org/birthYear"]
            |  "http://nexus.example.com/other" -> "http://schema.org/Other" [label = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
            |  "http://nexus.example.com/other" -> 9 [label = "http://schema.org/birthHour"]
            |  "http://nexus.example.com/john-doe" -> "http://nexus.example.com/other" [label = "http://example.com/sibling"]
            |}""".stripMargin
      graph.dot().split("\n").sorted shouldEqual expected.split("\n").sorted
    }

    "create correct DOT representation with prefix mappings" in {

      val jDoe  = url"http://nexus.example.com/john-doe"
      val other = url"http://nexus.example.com/other"

      val mappings: Map[AbsoluteIri, String] = Map(
        url"http://schema.org/deprecated"                    -> "deprecated",
        url"http://schema.org/Person"                        -> "Person",
        url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type" -> "@type",
        url"http://schema.org/"                              -> "schema"
      )

      val graph = Graph(
        jDoe,
        Set(
          (jDoe, url"http://schema.org/name", "John Doe"),
          (jDoe, url"http://example.com/stringProperty", "Some property"),
          (jDoe, url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", url"http://schema.org/Person"),
          (jDoe, url"http://schema.org/birthDate", "1999-04-09T20:00Z"),
          (jDoe, url"http://schema.org/deprecated", false),
          (other, url"http://schema.org/birthDate", "2000-04-12T20:00Z"),
          (other, url"http://schema.org/height", 9.223),
          (other, url"http://schema.org/birthYear", 1999),
          (other, url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "http://schema.org/Other"),
          (other, url"http://schema.org/birthHour", 9),
          (jDoe, url"http://example.com/sibling", other)
        )
      )
      val expected =
        """digraph "http://nexus.example.com/john-doe" {
          |  "http://nexus.example.com/john-doe" -> "John Doe" [label = "schema:name"]
          |  "http://nexus.example.com/john-doe" -> "Some property" [label = "http://example.com/stringProperty"]
          |  "http://nexus.example.com/john-doe" -> Person [label = "@type"]
          |  "http://nexus.example.com/john-doe" -> "1999-04-09T20:00Z" [label = "schema:birthDate"]
          |  "http://nexus.example.com/john-doe" -> false [label = deprecated]
          |  "http://nexus.example.com/other" -> "2000-04-12T20:00Z" [label = "schema:birthDate"]
          |  "http://nexus.example.com/other" -> 9.223 [label = "schema:height"]
          |  "http://nexus.example.com/other" -> 1999 [label = "schema:birthYear"]
          |  "http://nexus.example.com/other" -> "http://schema.org/Other" [label = "@type"]
          |  "http://nexus.example.com/other" -> 9 [label = "schema:birthHour"]
          |  "http://nexus.example.com/john-doe" -> "http://nexus.example.com/other" [label = "http://example.com/sibling"]
          |}""".stripMargin

      graph.dot(prefixMappings = mappings).split("\n").sorted shouldEqual expected.split("\n").sorted
    }

    "create correct DOT representation with sequential blank node ids" in {

      val jDoe = url"http://nexus.example.com/john-doe"

      val other  = BNode()
      val other2 = BNode()
      val graph = Graph(
        jDoe,
        Set(
          (jDoe, url"http://schema.org/name", "John Doe"),
          (jDoe, url"http://example.com/stringProperty", "Some property"),
          (jDoe, url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "http://schema.org/Person"),
          (jDoe, url"http://schema.org/birthDate", "1999-04-09T20:00Z"),
          (jDoe, url"http://schema.org/deprecated", false),
          (other, url"http://schema.org/birthDate", "2000-04-12T20:00Z"),
          (other, url"http://schema.org/height", 9.223),
          (other, url"http://schema.org/birthYear", 1999),
          (other, url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "http://schema.org/Other"),
          (other, url"http://schema.org/birthHour", 9),
          (jDoe, url"http://example.com/sibling", other),
          (other, url"http://example.com/sibling", other2)
        )
      )
      val expected =
        """digraph "http://nexus.example.com/john-doe" {
          |  "http://nexus.example.com/john-doe" -> "John Doe" [label = "http://schema.org/name"]
          |  "http://nexus.example.com/john-doe" -> "Some property" [label = "http://example.com/stringProperty"]
          |  "http://nexus.example.com/john-doe" -> "http://schema.org/Person" [label = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
          |  "http://nexus.example.com/john-doe" -> "1999-04-09T20:00Z" [label = "http://schema.org/birthDate"]
          |  "http://nexus.example.com/john-doe" -> false [label = "http://schema.org/deprecated"]
          |  "_:b1" -> "2000-04-12T20:00Z" [label = "http://schema.org/birthDate"]
          |  "_:b1" -> 9.223 [label = "http://schema.org/height"]
          |  "_:b1" -> 1999 [label = "http://schema.org/birthYear"]
          |  "_:b1" -> "http://schema.org/Other" [label = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
          |  "_:b1" -> 9 [label = "http://schema.org/birthHour"]
          |  "http://nexus.example.com/john-doe" -> "_:b1" [label = "http://example.com/sibling"]
          |  "_:b1" -> "_:b2" [label = "http://example.com/sibling"]
          |}""".stripMargin

      graph.dot().split("\n").sorted shouldEqual expected.split("\n").sorted
    }

    "create correct DOT representation with prefix mappings and shortened URLs" in {

      val jDoe  = url"http://nexus.example.com/john-doe"
      val other = url"http://nexus.example.com/other/another"

      val mappings: Map[AbsoluteIri, String] = Map(
        url"http://schema.org/deprecated" -> "deprecated",
        url"http://schema.org/Person"     -> "Person",
        url"http://schema.org/"           -> "schema"
      )

      val graph = Graph(
        jDoe,
        Set(
          (jDoe, url"http://schema.org/name", "John Doe"),
          (jDoe, url"http://example.com/stringProperty", "Some property"),
          (jDoe, url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", url"http://schema.org/Person"),
          (jDoe, url"http://schema.org/birthDate", "1999-04-09T20:00Z"),
          (jDoe, url"http://schema.org/deprecated", false),
          (other, url"http://schema.org/birthDate", "2000-04-12T20:00Z"),
          (other, url"http://schema.org/height", 9.223),
          (other, url"http://schema.org/birthYear", 1999),
          (other, url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", url"http://schema.org/Other"),
          (other, url"http://schema.org/birthHour", 9),
          (jDoe, url"http://example.com/sibling", other)
        )
      )
      val expected =
        """digraph "john-doe" {
          |  "john-doe" -> "John Doe" [label = "schema:name"]
          |  "john-doe" -> "Some property" [label = stringProperty]
          |  "john-doe" -> Person [label = type]
          |  "john-doe" -> "1999-04-09T20:00Z" [label = "schema:birthDate"]
          |  "john-doe" -> false [label = deprecated]
          |  another -> "2000-04-12T20:00Z" [label = "schema:birthDate"]
          |  another -> 9.223 [label = "schema:height"]
          |  another -> 1999 [label = "schema:birthYear"]
          |  another -> "schema:Other" [label = type]
          |  another -> 9 [label = "schema:birthHour"]
          |  "john-doe" -> another [label = sibling]
          |}""".stripMargin

      graph.dot(prefixMappings = mappings, stripPrefixes = true).split("\n").sorted shouldEqual expected
        .split("\n")
        .sorted
    }

  }
}
