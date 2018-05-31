package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.BNode
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.nexus._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.{Matchers, OptionValues, TryValues, WordSpecLike}

import scala.io.Source

class GraphSyntaxSpec extends WordSpecLike with Matchers with TryValues with OptionValues {

  "A GraphSyntax" should {
    val json               = jsonContentOf("/no_id.json")
    val selfReferencedJson = jsonContentOf("/self-referenced.json")
    val typedJson          = jsonContentOf("/id_and_types.json")

    "find @id from a Json-LD without it" in {
      json.asGraph.id.value shouldBe a[BNode]
    }

    "find @id from Json-LD with it" in {
      typedJson.asGraph.id.value shouldEqual url"http://example.org/cars/for-sale#tesla"
    }

    "find @id from a self referenced Json-LD" in {
      selfReferencedJson.asGraph.id.value shouldEqual url"https://bbp-nexus.epfl.ch/dev/v0/contexts/nexus/core/distribution/v0.1.0"
    }

    "find the @type from the Json-LD without @id" in {
      json.asGraph.types shouldEqual Set(url"http://google.com/a")

    }

    "find the @type from the Json-LD with @id" in {
      typedJson.asGraph.types shouldEqual Set(url"http://purl.org/goodrelations/v1#Offering",
                                              url"http://www.w3.org/2002/07/owl#Ontology")

    }
  }

  final def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toTry.success.value

}
