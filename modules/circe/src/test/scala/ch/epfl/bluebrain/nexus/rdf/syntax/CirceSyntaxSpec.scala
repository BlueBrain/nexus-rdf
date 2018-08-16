package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf.circe.JenaModel
import ch.epfl.bluebrain.nexus.rdf.circe.JenaModel.JenaModelErr.InvalidJsonLD
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder
import ch.epfl.bluebrain.nexus.rdf.syntax.CirceSyntaxSpec._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe.context._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe.encoding._
import ch.epfl.bluebrain.nexus.rdf.syntax.jena._
import ch.epfl.bluebrain.nexus.rdf.syntax.node._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Iri, Node}
import io.circe.Json
import io.circe.parser._
import org.apache.jena.rdf.model.Model
import org.scalatest.EitherValues._
import org.scalatest._

import scala.collection.JavaConverters._

class CirceSyntaxSpec extends WordSpecLike with Matchers with TryValues with OptionValues with Inspectors {

  "CirceSyntax" should {

    implicit val enc: GraphEncoder[Item] = GraphEncoder { e =>
      e.bNode -> Graph((e.bNode, predicate.description, e.description), (e.bNode, predicate.step, e.step))
    }

    implicit val encExample = GraphEncoder[Example] { example =>
      IriNode(example.id) -> Graph(
        Set[Graph.Triple](
          (IriNode(example.id), url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", example.tpe),
          (IriNode(example.id), url"http://schema.org/name", example.name),
          (IriNode(example.id),
           url"http://schema.org/birthDate",
           Literal(example.birthDate, url"http://www.w3.org/2001/XMLSchema#dateTime".value))
        ))
    }
    // format: off
    val triples = Set[Graph.Triple](
      (url"http://nexus.example.com/john-doe", url"http://schema.org/name", "John Doe"),
      (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate", Literal("1999-04-09T20:00Z", url"http://www.w3.org/2001/XMLSchema#dateTime".value)),
      (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", url"http://schema.org/Person")
    )
    // format: on

    "convert valid JSON-LD into Graph" in {
      jsonContentOf("/simple.json").asGraph.right.value.triples shouldEqual triples
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
      val graph = json.asGraph.right.value
      graph.asJson(ctx, Some(id)).success.value deepMerge ctx shouldEqual json
    }

    "convert Graph with nested relationships to Json-LD  with context" in {
      val json = jsonContentOf("/embed.json")
      val id   = url"http://nexus.example.com/john-doe"
      json.asGraph.right.value.asJson(context(json), Some(id)).success.value shouldEqual json
    }

    "convert Graph to Json-LD from a root node that is a blank node" in {
      val json = jsonContentOf("/embed-no-id.json")
      val g    = json.asGraph.right.value
      val id   = g.subjects(rdf.tpe, url"http://schema.org/Person").headOption.value
      g.asJson(context(json), Some(id)).success.value shouldEqual json
    }

    "convert Graph with multiple entities to Json-LD  with context" in {
      val json   = jsonContentOf("/graph.json")
      val output = json.asGraph.right.value.asJson(context(json), None).success.value
      graphArray(output) should contain theSameElementsAs graphArray(json)
    }

    "convert Graph with sorted list" in {
      val list = List(Item(1, "description elem 1"), Item(2, "description elem 2"), Item(3, "description elem 3"))
      val json = jsonContentOf("/list.json")

      val id: IriOrBNode = url"http://example.com/id"
      val graph          = Graph().add(id, url"http://example.com/items", list)
      graph.asJson(context(json), Some(id)).success.value shouldEqual json
    }

    "convert to Json from entity with GraphEncoder" in {
      val json     = jsonContentOf("/context/simple-iri-context.json")
      val ctx      = context(json)
      val emptyCtx = Json.obj("@context" -> Json.obj())
      val example = Example(url"http://nexus.example.com/john-doe".value,
                            url"http://schema.org/Person".value,
                            "John Doe",
                            "1999-04-09T20:00Z")
      example.asJson(ctx).deepMerge(emptyCtx) shouldEqual json.deepMerge(emptyCtx)
    }

    "fetch the @id from the Json" in {
      val list = List(
        Iri.absolute("http://nexus.example.com/john-doe").right.value     -> jsonContentOf("/embed.json"),
        Iri.absolute("http://example.com/id").right.value                 -> jsonContentOf("/list.json"),
        Iri.absolute("http://schema.org/john-doe").right.value            -> jsonContentOf("/aliased.json"),
        Iri.absolute("http://nexus.example.com/graph").right.value        -> jsonContentOf("/graph-simple.json"),
        Iri.absolute("http://nexus.example.com/array-simple").right.value -> jsonContentOf("/array-simple.json"),
        Iri.absolute("http://nexus.example.com/array-graph").right.value  -> jsonContentOf("/array-graph.json"),
        Iri.absolute("http://example.com/other").right.value              -> jsonContentOf("/array-graph-top-id.json")
      )
      forAll(list) {
        case (iri, json) =>
          json.id.value shouldEqual iri
      }
    }
    "fail to fetch the @id when not present" in {
      Json.obj("type" -> Json.fromString("Person")).id shouldEqual None
      Json.fromString("something").id shouldEqual None
    }

    "fail to fetch the @id when there is a @graph with several objects" in {
      jsonContentOf("/graph.json").id shouldEqual None
    }

    "deal with invalid ID's" in {
      val list = List(jsonContentOf("/wrong-id.json"), Json.obj("@type" -> Json.fromString("Value")))
      forAll(list)(json => JenaModel(json).left.value shouldBe a[InvalidJsonLD])
    }

    "convert model to graph and reverse" in {
      val json            = jsonContentOf("/simple-model2.json")
      val result: Model   = JenaModel(json).right.value.asGraph.asJenaModel
      val expected: Model = JenaModel(json).right.value
      result.listStatements().asScala.toList should contain theSameElementsAs expected.listStatements().asScala.toList
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

  final case class Example(id: AbsoluteIri, tpe: AbsoluteIri, name: String, birthDate: String)

  object predicate {
    val base        = "http://vocab/elem"
    val description = url"$base/description"
    val step        = url"$base/step"

  }

}
