package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.dot._
import org.scalatest.{EitherValues, Matchers, WordSpecLike}

class GraphDotSyntaxSpec extends WordSpecLike with Matchers with EitherValues {
  "A dot syntax" should {
    val json  = jsonContentOf("/embed.json")
    val graph = json.asGraph.right.value

    "generate a dot output" in {
      val expected =
        """digraph {
          |  "http://nexus.example.com/john-doe" -> "John Doe" [label = "http://schema.org/name"]
          |  "http://nexus.example.com/john-doe" -> "Some property" [label = "http://example.com/stringProperty"]
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
      graph.asDot.split("\n").sorted shouldEqual expected.split("\n").sorted
    }
  }

}
