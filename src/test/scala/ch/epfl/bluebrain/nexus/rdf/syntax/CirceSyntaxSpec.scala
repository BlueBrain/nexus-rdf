package ch.epfl.bluebrain.nexus.rdf.syntax

import cats.Id
import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.GraphSpec.Item
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{blank, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf._
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.EncoderResult
import ch.epfl.bluebrain.nexus.rdf.MarshallingError._
import ch.epfl.bluebrain.nexus.rdf.encoder.{GraphEncoder, RootNode}
import ch.epfl.bluebrain.nexus.rdf.instances._
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel.JenaModelErr.InvalidJsonLD
import ch.epfl.bluebrain.nexus.rdf.syntax.CirceSyntaxSpec._
import io.circe.Json
import io.circe.parser._
import org.apache.jena.graph.NodeFactory
import org.apache.jena.rdf.model.{Model, ResourceFactory}
import org.scalatest.EitherValues._
import org.scalatest._

import scala.collection.JavaConverters._

class CirceSyntaxSpec
    extends WordSpecLike
    with Matchers
    with TryValues
    with OptionValues
    with Inspectors
    with Resources {

  "CirceSyntax" should {

    implicit val enc: GraphEncoder[Id, Item] = GraphEncoder { (id, e) =>
      Graph((id, predicate.description, e.description), (id, predicate.step, e.step))
    }

    implicit val rootNodeItem: RootNode[Item] = _.bNode
    implicit val encExample = GraphEncoder[Id, Example] { (id, example) =>
      Graph(
        Set[Graph.Triple](
          (id, rdf.tpe, example.tpe),
          (id, url"http://schema.org/name", example.name),
          (id, url"http://schema.org/birthDate", Literal(example.birthDate, xsd.dateTime.value))
        ))
    }

    implicit val encExampleEither: GraphEncoder[EncoderResult, Example] = encExample.toEither

    implicit val rootNodeExample: RootNode[Example] = _.id

    // format: off
    val triples = Set[Graph.Triple](
      (url"http://nexus.example.com/john-doe", url"http://schema.org/name", "John Doe"),
      (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate", Literal("1999-04-09T20:00Z", url"http://www.w3.org/2001/XMLSchema#dateTime".value)),
      (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type", url"http://schema.org/Person")
    )
    // format: on

    "convert valid JSON-LD into Graph" in {
      jsonContentOf("/simple.json").asGraph(blank).right.value.triples shouldEqual triples
    }

    "convert Graph to Json-LD with context" in {

      // format: off
      val graph = RootedGraph(url"http://nexus.example.com/john-doe", triples)
      // format: on
      val json = graph.as[Json]().right.value.asObject.value
      json("@id").value.asString.value shouldEqual "http://nexus.example.com/john-doe"
      json("http://schema.org/birthDate").value.asString.value shouldEqual "1999-04-09T20:00Z"
      json("http://schema.org/name").value.asString.value shouldEqual "John Doe"
    }

    "convert Graph to Json-LD ignoring the IRI context" in {
      val graph   = RootedGraph(url"http://nexus.example.com/john-doe", triples)
      val context = parse("{\"@context\": \"http://schema.org/\"}").right.value

      graph.as[Json](context).right.value shouldEqual Json.obj(
        "@context"                    -> Json.obj(),
        "@id"                         -> Json.fromString("http://nexus.example.com/john-doe"),
        "@type"                       -> Json.fromString("http://schema.org/Person"),
        "http://schema.org/birthDate" -> Json.fromString("1999-04-09T20:00Z"),
        "http://schema.org/name"      -> Json.fromString("John Doe")
      )
    }

    "convert an empty graph back to json" in {
      RootedGraph(url"http://nexus.example.com/john-doe", Graph()).as[Json]().right.value shouldEqual Json.obj()
    }

    "convert Graph to Json-LD ignoring the IRI context in an array" in {
      val json  = jsonContentOf("/context/simple-iri-context.json")
      val ctx   = context(json)
      val graph = RootedGraph(url"http://nexus.example.com/john-doe", triples)
      graph.as[Json](ctx).right.value deepMerge ctx shouldEqual json
    }

    "convert Graph with nested relationships to Json-LD  with context" in {
      val json                              = jsonContentOf("/embed.json")
      val id                                = url"http://nexus.example.com/john-doe"
      val graph: EncoderResult[RootedGraph] = json.asGraph(id)
      graph.right.value.as[Json](context(json)).right.value shouldEqual json
    }

    "convert Graph to Json-LD from a root node that is a blank node" in {
      val json = jsonContentOf("/embed-no-id.json")
      val g = json
        .asGraph(
          _.subjects(rdf.tpe, url"http://schema.org/Person").headOption
            .toRight(rootNotFound()))
        .right
        .value
      g.as[Json](context(json)).right.value shouldEqual json
    }

    "convert Graph with sorted list" in {
      val list = List(Item(1, "description elem 1"), Item(2, "description elem 2"), Item(3, "description elem 3"))
      val json = jsonContentOf("/list.json")

      val id: IriOrBNode   = url"http://example.com/id"
      val graph: Id[Graph] = Graph().add(id, url"http://example.com/items", list)
      RootedGraph(id, graph).as[Json](context(json)).right.value shouldEqual json
    }

    "convert to compacted Json from entity with GraphEncoder" in {
      val json = jsonContentOf("/context/simple-iri-context.json")
      val ctx  = context(json)
      val example = Example(url"http://nexus.example.com/john-doe".value,
                            url"http://schema.org/Person".value,
                            "John Doe",
                            "1999-04-09T20:00Z")
      example.as[Json](ctx).right.value shouldEqual json.replaceContext(json.removeContextIris)
    }

    "convert to expanded Json from entity with GraphEncoder" in {
      val example = Example(url"http://nexus.example.com/john-doe".value,
                            url"http://schema.org/Person".value,
                            "John Doe",
                            "1999-04-09T20:00Z")
      example.as[Json]().right.value shouldEqual Json.obj(
        "@id"                         -> Json.fromString(example.id.asString),
        "@type"                       -> Json.fromString(example.tpe.asString),
        "http://schema.org/birthDate" -> Json.fromString(example.birthDate),
        "http://schema.org/name"      -> Json.fromString(example.name)
      )
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
      val baseList = List.range(1, 4).map(i => jsonContentOf(s"/wrong-id-with-base-$i.json"))
      val list     = jsonContentOf("/wrong-id.json") :: baseList
      forAll(list)(json => JenaModel(json).left.value shouldBe a[InvalidJsonLD])
    }

    "create a model with valid @base" in {
      val json = jsonContentOf("/id-with-base.json")
      val m    = JenaModel(json).right.value
      m.listStatements().asScala.toList shouldEqual List(
        m.createStatement(
          ResourceFactory.createResource("http://nexus.example.com/john-doe"),
          m.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
          m.asRDFNode(NodeFactory.createURI("http://example.com/Person"))
        ))
    }

    "convert model to graph and reverse" in {
      val json              = jsonContentOf("/simple-model2.json")
      val result: Id[Model] = JenaModel(json).right.value.asGraph(blank).right.value.as[Model]()
      val expected: Model   = JenaModel(json).right.value
      result.listStatements().asScala.toList should contain theSameElementsAs expected.listStatements().asScala.toList
    }

    "deal with invalid ID's at the graph level" in {
      val json = jsonContentOf("/wrong-id-2.json")
      JenaModel(json).left.value shouldBe a[InvalidJsonLD]
    }

    "convert json with @base to graph and back" in {
      val json     = jsonContentOf("/simple-with-base.json")
      val id       = url"https://example.nexus.com/nexus/v1/resources/org/project/_/Movie_Test"
      val graph    = json.asGraph(id).right.value
      val expected = jsonContentOf("/simple-with-base-output.json")
      graph.as[Json](Json.obj("@context" -> json.contextValue)).right.value shouldEqual expected
    }

    "convert to graph and back to expanded Json-LD" in {
      // format: off
      val jsons = List(
        (jsonContentOf("/simple-model2.json"),    jsonContentOf("/simple-model2-expanded.json"),    url"http://nexus.example.com/john-doe".value),
        (jsonContentOf("/simple-with-base.json"), jsonContentOf("/simple-with-base-expanded.json"), url"https://example.nexus.com/nexus/v1/resources/org/project/_/Movie_Test".value
        )
      )
      // format: on
      forAll(jsons) {
        case (json, expected, id) =>
          val graph = json.asGraph(id).right.value
          graph.as[Json]().right.value shouldEqual expected
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

  final case class Example(id: AbsoluteIri, tpe: AbsoluteIri, name: String, birthDate: String)

  object predicate {
    val base        = "http://vocab/elem"
    val description = url"$base/description"
    val step        = url"$base/step"

  }

}
