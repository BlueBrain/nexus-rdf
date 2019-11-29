package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.{Dot, RdfSpec}

class GraphDotSyntaxSpec extends RdfSpec {
  "A dot syntax" should {
    val json = jsonContentOf("/embed.json")

    "generate a dot output" in {
      val graph = json.asGraph(url"http://nexus.example.com/john-doe").rightValue
      val expected =
        """digraph {
          |  "http://nexus.example.com/john-doe" -> "John Doe" [label = "http://schema.org/name"]
          |  "http://nexus.example.com/john-doe" -> "Some property" [label = "http://example.com/stringProperty"]
          |  "http://nexus.example.com/john-doe" -> " " [label = "http://example.com/stringProperty2"]
          |  "http://nexus.example.com/john-doe" -> "http://schema.org/Person" [label = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
          |  "http://nexus.example.com/john-doe" -> "1999-04-09T20:00Z" [label = "http://schema.org/birthDate"]
          |  "http://nexus.example.com/john-doe" -> false [label = "http://schema.org/deprecated"]
          |  "http://nexus.example.com/other" -> "2000-04-12T20:00Z" [label = "http://schema.org/birthDate"]
          |  "http://nexus.example.com/other" -> "9.223E0" [label = "http://schema.org/height"]
          |  "http://nexus.example.com/other" -> 1999 [label = "http://schema.org/birthYear"]
          |  "http://nexus.example.com/other" -> "http://schema.org/Other" [label = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"]
          |  "http://nexus.example.com/other" -> 9 [label = "http://schema.org/birthHour"]
          |  "http://nexus.example.com/john-doe" -> "http://nexus.example.com/other" [label = "http://example.com/sibling"]
          |}""".stripMargin
      graph.as[Dot]().value.split("\n").sorted shouldEqual expected.split("\n").sorted
    }
  }

}
