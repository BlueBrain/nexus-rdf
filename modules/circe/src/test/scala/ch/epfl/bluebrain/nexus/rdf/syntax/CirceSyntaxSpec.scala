package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Node.Literal
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.node._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import io.circe.Json
import io.circe.parser._
import org.scalatest._

import scala.io.Source

class CirceSyntaxSpec extends WordSpecLike with Matchers with TryValues with EitherValues with OptionValues {

  "CirceSyntax" should {

    // format: off
    val triples = Set[Graph.Triple](
      (url"http://nexus.example.com/john-doe", url"http://schema.org/name", "John Doe"),
      (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate", Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
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

      val json = graph.asJson(context, None).success.value.asObject.value
      json("@id").value.asString.value shouldEqual "http://nexus.example.com/john-doe"
      json("birthDate").value.asString.value shouldEqual "1999-04-09T20:00Z"
      json("name").value.asString.value shouldEqual "John Doe"
    }

    "convert Graph to Json-LD ignoring the IRI context in an array" in {
      val json = jsonContentOf("/simple-iri-context.json")
      val id   = url"http://nexus.example.com/john-doe"
      val ctx  = context(json)
      json.asGraph.asJson(ctx, Some(id)).success.value deepMerge ctx shouldEqual json
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

  }

  def context(json: Json): Json =
    Json.obj("@context" -> json.hcursor.get[Json]("@context").getOrElse(Json.obj()))

  def graphArray(json: Json): Vector[Json] =
    json.hcursor.downField("@graph").focus.flatMap(_.asArray).value

  final def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toTry.success.value

}
