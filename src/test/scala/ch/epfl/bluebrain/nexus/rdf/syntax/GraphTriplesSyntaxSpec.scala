package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.blank
import ch.epfl.bluebrain.nexus.rdf.{NTriples, Resources}
import org.scalatest.{EitherValues, Matchers, WordSpecLike}

class GraphTriplesSyntaxSpec extends WordSpecLike with Matchers with EitherValues with Resources {
  "A triples syntax" should {
    val json  = jsonContentOf("/embed.json")
    val graph = json.asGraph(blank).right.value

    "generate n triples output" in {
      val expected =
        """<http://nexus.example.com/other> <http://schema.org/birthDate> "2000-04-12T20:00Z"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
          |<http://nexus.example.com/john-doe> <http://example.com/stringProperty> "Some property" .
          |<http://nexus.example.com/other> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Other> .
          |<http://nexus.example.com/other> <http://schema.org/birthYear> "1999"^^<http://www.w3.org/2001/XMLSchema#long> .
          |<http://nexus.example.com/john-doe> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
          |<http://nexus.example.com/other> <http://schema.org/birthHour> "9"^^<http://www.w3.org/2001/XMLSchema#integer> .
          |<http://nexus.example.com/john-doe> <http://schema.org/birthDate> "1999-04-09T20:00Z"^^<http://www.w3.org/2001/XMLSchema#dateTime> .
          |<http://nexus.example.com/john-doe> <http://schema.org/deprecated> "false"^^<http://www.w3.org/2001/XMLSchema#boolean> .
          |<http://nexus.example.com/john-doe> <http://example.com/sibling> <http://nexus.example.com/other> .
          |<http://nexus.example.com/john-doe> <http://schema.org/name> "John Doe" .
          |<http://nexus.example.com/other> <http://schema.org/height> "9.223E0"^^<http://www.w3.org/2001/XMLSchema#double> .
          |""".stripMargin
      graph.as[NTriples].right.value.value.split("\n").sorted shouldEqual expected.split("\n").sorted
    }
  }

}
