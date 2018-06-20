package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node
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
    val self      = jsonContentOf("/self-reference.json")
    val json      = jsonContentOf("/no_id.json")
    val typedJson = jsonContentOf("/id_and_types.json")

    "find @id from a Json-LD without it" in {
      json.asGraph.primaryNode.value shouldBe a[BNode]
      json.asGraph.primaryBNode.value shouldBe a[BNode]
    }

    "find @id from Json-LD with it" in {
      typedJson.asGraph.primaryNode.value shouldEqual url"http://example.org/cars/for-sale#tesla"
      typedJson.asGraph.primaryIriNode.value shouldEqual url"http://example.org/cars/for-sale#tesla"
    }

    "fail to find an @id when it is self-referenced" in {
      self.asGraph.primaryNode shouldEqual None
    }

    "find the @type from the Json-LD without @id" in {
      json.asGraph.primaryTypes shouldEqual Set(url"http://google.com/a")

    }

    "find no types when it is self-referenced" in {
      self.asGraph.primaryTypes shouldEqual Set.empty
    }

    "find the @type from the Json-LD with @id" in {
      typedJson.asGraph.primaryTypes shouldEqual Set(url"http://purl.org/goodrelations/v1#Offering",
                                                     url"http://www.w3.org/2002/07/owl#Ontology")

    }

    "navigate to an element" in {
      json.asGraph
        .cursor()
        .downField(url"http://schema.org/image")
        .focus
        .value shouldEqual (url"http://www.civil.usherbrooke.ca/cours/gci215a/empire-state-building.jpg": Node)
    }

    "return a failed cursor when @id is not found" in {
      self.asGraph.cursor().failed shouldEqual true
    }

    "convert back to json" in {
      val other = jsonContentOf("/id_and_type.json")
      other.asGraph.asJson(context(other)).success.value shouldEqual other
    }

  }

  def context(json: Json): Json = Json.obj("@context" -> json.hcursor.get[Json]("@context").getOrElse(Json.obj()))

  final def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toTry.success.value

}
