package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.{IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder
import ch.epfl.bluebrain.nexus.rdf.syntax.CirceSyntaxSpec._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe.context._
import ch.epfl.bluebrain.nexus.rdf.syntax.node._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Iri, Node}
import io.circe.Json
import io.circe.parser._
import org.scalatest.EitherValues._
import org.scalatest._

class CirceSyntaxSpec extends WordSpecLike with Matchers with TryValues with OptionValues with Inspectors {

  "CirceSyntax" should {

    implicit val enc: GraphEncoder[Item] = GraphEncoder { e =>
      e.bNode -> Graph((e.bNode, predicate.description, e.description), (e.bNode, predicate.step, e.step))
    }
    // format: off
    val triples = Set[Graph.Triple](
      (url"http://nexus.example.com/john-doe", url"http://schema.org/name", "John Doe"),
      (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate", Literal("1999-04-09T20:00Z", url"http://www.w3.org/2001/XMLSchema#dateTime".value)),
      (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", url"http://schema.org/Person")
    )
    // format: on

    "convert valid JSON-LD into Graph" in {
      jsonContentOf("/simple.json").asGraph.triples shouldEqual triples
    }

    "convert Graph to Json-LD without context" in {

      // format: off
      val graph = Graph(triples)
      // format: on
      val json = graph.asJson.asObject.value
      json("@id").value.asString.value shouldEqual "http://nexus.example.com/john-doe"
      json("birthDate").value.asString.value shouldEqual "1999-04-09T20:00Z"
      json("name").value.asString.value shouldEqual "John Doe"
    }

    "convert Graph to Json-LD ignoring the IRI context" in {
      // format: off
      val graph = Graph(triples)
      // format: on
      val context = parse("{\"@context\": \"http://schema.org/\"}").right.value

      graph.asJson(context, None).success.value shouldEqual Json.obj(
        "@context"                    -> Json.obj(),
        "@id"                         -> Json.fromString("http://nexus.example.com/john-doe"),
        "@type"                       -> Json.fromString("http://schema.org/Person"),
        "http://schema.org/birthDate" -> Json.fromString("1999-04-09T20:00Z"),
        "http://schema.org/name"      -> Json.fromString("John Doe")
      )
    }

    "convert Graph to Json-LD ignoring the IRI context in an array" in {
      val json  = jsonContentOf("/context/simple-iri-context.json")
      val id    = url"http://nexus.example.com/john-doe"
      val ctx   = context(json)
      val graph = json.asGraph
      graph.asJson(ctx, Some(id)).success.value deepMerge ctx shouldEqual json
    }

    "convert Graph with nested relationships to Json-LD  with context" in {
      val json = jsonContentOf("/embed.json")
      val id   = url"http://nexus.example.com/john-doe"
      json.asGraph.asJson(context(json), Some(id)).success.value shouldEqual json
    }

    "convert Graph with multiple entities to Json-LD  with context" in {
      val json   = jsonContentOf("/graph.json")
      val output = json.asGraph.asJson(context(json), None).success.value
      graphArray(output) should contain theSameElementsAs graphArray(json)
    }

    "convert Graph with sorted list" in {
      val list = List(Item(1, "description elem 1"), Item(2, "description elem 2"), Item(3, "description elem 3"))
      val json = jsonContentOf("/list.json")

      val id: IriOrBNode = url"http://example.com/id"
      val graph          = Graph().add(id, url"http://example.com/items", list)
      graph.asJson(context(json), Some(id)).success.value shouldEqual json
    }

    "attempt to fetch the @id from the Json" in {
      val list = List(
        Iri.absolute("http://nexus.example.com/john-doe").right.value -> jsonContentOf("/embed.json"),
        Iri.absolute("http://example.com/id").right.value             -> jsonContentOf("/list.json"),
        Iri.absolute("http://schema.org/john-doe").right.value        -> jsonContentOf("/aliased.json")
      )
      forAll(list) {
        case (iri, json) => json.id.value shouldEqual iri
      }
    }

  }

  def context(json: Json): Json =
    Json.obj("@context" -> json.contextValue)

  def graphArray(json: Json): Vector[Json] =
    json.hcursor.downField("@graph").focus.flatMap(_.asArray).value

}

object CirceSyntaxSpec {

  final case class Item(step: Int, description: String) {
    val bNode: IriOrBNode = Node.blank(s"BNode$step").right.value
  }

  object predicate {
    val base        = "http://vocab/elem"
    val description = url"$base/description"
    val step        = url"$base/step"

  }

}
