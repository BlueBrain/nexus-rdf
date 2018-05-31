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
    val json      = jsonContentOf("/no_id.json")
    val typedJson = jsonContentOf("/id_and_types.json")

    "find @id from a Json-LD without it" in {
      json.asGraph.primaryNode.value shouldBe a[BNode]
    }

    "find @id from Json-LD with it" in {
      typedJson.asGraph.primaryNode.value shouldEqual url"http://example.org/cars/for-sale#tesla"
    }

    "find the @type from the Json-LD without @id" in {
      json.asGraph.primaryTypes shouldEqual Set(url"http://google.com/a")

    }

    "find the @type from the Json-LD with @id" in {
      typedJson.asGraph.primaryTypes shouldEqual Set(url"http://purl.org/goodrelations/v1#Offering",
                                                     url"http://www.w3.org/2002/07/owl#Ontology")

    }
  }

  final def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toTry.success.value

}
