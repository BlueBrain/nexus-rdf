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
    "convert valid JSON-LD into Graph" in {
      jsonContentOf("/simple.json").asGraph.triples shouldEqual Set[Graph.Triple](
        (url"http://nexus.example.com/john-doe", url"http://schema.org/name",                           "John Doe"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate",                      Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person")
      )
    }
    // format: on

    "convert Graph to Json-LD without context" in {
      // format: off
      val graph = Graph(
        (url"http://nexus.example.com/john-doe", url"http://schema.org/name",                           "John Doe"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate",                      Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person")
      )
      // format: on
      val json = graph.asJson.asObject.value
      json("@id").value.asString.value shouldEqual "http://nexus.example.com/john-doe"
      json("birthDate").value.asString.value shouldEqual "1999-04-09T20:00Z"
      json("name").value.asString.value shouldEqual "John Doe"
    }

    "convert Graph to Json-LD ignoring the IRI context" in {
      // format: off
      val graph = Graph(
        (url"http://nexus.example.com/john-doe", url"http://schema.org/name",                           "John Doe"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate",                      Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person")
      )
      // format: on
      val context = parse("{\"@context\": \"http://schema.org/\"}").right.value

      val json = graph.asJson(context).success.value.asObject.value
      json("@id").value.asString.value shouldEqual "http://nexus.example.com/john-doe"
      json("birthDate").value.asString.value shouldEqual "1999-04-09T20:00Z"
      json("name").value.asString.value shouldEqual "John Doe"
    }

    "convert Graph to Json-LD ignoring the IRI context in an array" in {
      // format: off
      val graph = Graph(
        (url"http://nexus.example.com/john-doe", url"http://schema.org/name",                           "John Doe"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate",                      Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person")
      )
      // format: on
      val context = jsonContentOf("/test-context-with-iri.json")

      val json = graph.asJson(context).success.value.asObject.value
      json("@id").value.asString.value shouldEqual "http://nexus.example.com/john-doe"
      json("birthDate").value.asString.value shouldEqual "1999-04-09T20:00Z"
      json("name").value.asString.value shouldEqual "John Doe"
    }

    "convert Graph to Json-LD  with context" in {
      // format: off
      val graph = Graph(
        (url"http://nexus.example.com/john-doe", url"http://schema.org/name",                           "John Doe"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate",                      Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/john-doe", url"http://example.com/stringProperty",                "Some property"),
        (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person")
      )
      // format: on
      val context = jsonContentOf("/test-context.json")

      val json = graph.asJson(context).success.value.asObject.value
      json("@id").value.asString.value shouldEqual "http://nexus.example.com/john-doe"
      json("birthDate").value.asString.value shouldEqual "1999-04-09T20:00Z"
      json("name").value.asString.value shouldEqual "John Doe"
      json("sp").value.asString.value shouldEqual "Some property"
    }

    "convert Graph with multiple entities to Json-LD  with context" in {
      // format: off
      val graph = Graph(
        (url"http://nexus.example.com/john-doe", url"http://schema.org/name",                           "John Doe"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate",                      Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/john-doe", url"http://example.com/stringProperty",                "Some property"),
        (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/knows",                          url"http://nexus.example.com/jack-doe"),
        (url"http://nexus.example.com/jack-doe", url"http://schema.org/name",                           "Jack Doe"),
        (url"http://nexus.example.com/jack-doe", url"http://schema.org/birthDate",                      Literal("1989-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/jack-doe", url"http://example.com/stringProperty",                "other property"),
        (url"http://nexus.example.com/jack-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person"),
        (url"http://nexus.example.com/jack-doe", url"http://schema.org/knows",                          url"http://nexus.example.com/john-doe")
      )
      // format: on
      val context = jsonContentOf("/test-context.json")

      val json = graph.asJson(context).success.value
      val jack = json.hcursor.downField("@graph").downN(0)
      val john = json.hcursor.downField("@graph").downN(1)

      jack.get[String]("@id").right.value shouldEqual "http://nexus.example.com/jack-doe"
      jack.get[String]("birthDate").right.value shouldEqual "1989-04-09T20:00Z"
      jack.get[String]("name").right.value shouldEqual "Jack Doe"
      jack.get[String]("sp").right.value shouldEqual "other property"
      jack.get[String]("knows").right.value shouldEqual "http://nexus.example.com/john-doe"

      john.get[String]("@id").right.value shouldEqual "http://nexus.example.com/john-doe"
      john.get[String]("birthDate").right.value shouldEqual "1999-04-09T20:00Z"
      john.get[String]("name").right.value shouldEqual "John Doe"
      john.get[String]("sp").right.value shouldEqual "Some property"
      john.get[String]("knows").right.value shouldEqual "http://nexus.example.com/jack-doe"
    }

  }

  final def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toTry.success.value

}
