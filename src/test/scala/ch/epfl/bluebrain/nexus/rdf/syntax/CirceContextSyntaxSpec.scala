package ch.epfl.bluebrain.nexus.rdf.syntax

import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.{Iri, Resources}
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class CirceContextSyntaxSpec extends WordSpecLike with Matchers with Inspectors with EitherValues with Resources {

  private val context: AbsoluteIri =
    Iri.absolute("https://bbp-nexus.epfl.ch/dev/v0/contexts/bbp/core/context/v0.1.0").right.value

  "injecting context" should {
    val contextString = Json.fromString(context.show)

    val mapping = List(
      Json.obj("@id"        -> Json.fromString("foo-id"), "nxv:rev" -> Json.fromLong(1)) ->
        Json.obj("@context" -> contextString, "@id" -> Json.fromString("foo-id"), "nxv:rev" -> Json.fromLong(1)),
      Json.obj("@context"   -> Json.fromString("http://foo.domain/some/context"),
               "@id"        -> Json.fromString("foo-id"),
               "nxv:rev"    -> Json.fromLong(1)) ->
        Json.obj(
          "@context" -> Json.arr(Json.fromString("http://foo.domain/some/context"), contextString),
          "@id"      -> Json.fromString("foo-id"),
          "nxv:rev"  -> Json.fromLong(1)
        ),
      Json.obj(
        "@context" -> Json.arr(Json.fromString("http://foo.domain/some/context"),
                               Json.fromString("http://bar.domain/another/context")),
        "@id"     -> Json.fromString("foo-id"),
        "nxv:rev" -> Json.fromLong(1)
      ) ->
        Json.obj(
          "@context" -> Json.arr(Json.fromString("http://foo.domain/some/context"),
                                 Json.fromString("http://bar.domain/another/context"),
                                 contextString),
          "@id"     -> Json.fromString("foo-id"),
          "nxv:rev" -> Json.fromLong(1)
        ),
      Json.obj(
        "@context" -> Json.obj("foo" -> Json.fromString("http://foo.domain/some/context"),
                               "bar" -> Json.fromString("http://bar.domain/another/context")),
        "@id"     -> Json.fromString("foo-id"),
        "nxv:rev" -> Json.fromLong(1)
      ) ->
        Json.obj(
          "@context" -> Json.arr(Json.obj("foo" -> Json.fromString("http://foo.domain/some/context"),
                                          "bar" -> Json.fromString("http://bar.domain/another/context")),
                                 contextString),
          "@id"     -> Json.fromString("foo-id"),
          "nxv:rev" -> Json.fromLong(1)
        )
    )

    "properly add or merge context into JSON payload" in {
      forAll(mapping) {
        case (in, out) =>
          in.addContext(context) shouldEqual out
      }
    }

    "added context uri to empty context value" in {
      val list = List(
        Json.obj("@context" -> Json.obj()).addContext(context),
        Json.obj("@context" -> Json.arr()).addContext(context),
        Json.obj("@context" -> Json.fromString("")).addContext(context)
      )
      forAll(list)(_ shouldEqual Json.obj("@context" -> Json.fromString(context.asString)))
    }

    "be idempotent" in {
      forAll(mapping) {
        case (in, _) =>
          in.addContext(context) shouldEqual in.addContext(context).addContext(context)
      }
    }

    "extract context" in {
      val context1          = jsonContentOf("/context/context1.json")
      val context1Extracted = jsonContentOf("/context/context1_extracted.json")

      context1.contextValue shouldEqual context1Extracted
    }

    "extract empty json when @context key missing" in {
      Json.obj("one" -> Json.fromInt(1)).contextValue shouldEqual Json.obj()
    }

    "merge two contexts" in {
      val context1 = jsonContentOf("/context/context1.json")
      val context2 = jsonContentOf("/context/context2.json")

      context1 mergeContext context2 shouldEqual jsonContentOf("/context/context_merged.json")
    }

    "append context" in {
      val context1 = jsonContentOf("/context/context1.json")
      val json     = context1 deepMerge Json.obj("one" -> Json.fromInt(1), "two" -> Json.fromInt(2))
      val context2 = jsonContentOf("/context/context2.json")
      val json2    = context2 deepMerge Json.obj("three" -> Json.fromInt(3), "four" -> Json.fromInt(4))

      json appendContextOf json2 shouldEqual (jsonContentOf("/context/context_merged.json") deepMerge Json.obj(
        "one" -> Json.fromInt(1),
        "two" -> Json.fromInt(2)))
    }

    "extract the context IRI" in {
      val context = parse("{\"@context\": \"http://schema.org/\"}").right.value
      context.removeContextIris shouldEqual Json.obj("@context" -> Json.obj())
    }

    "extract the context iri from an array" in {
      val json     = jsonContentOf("/context/simple-iri-context.json")
      val expected = jsonContentOf("/context/simple-iri-context-without-iri.json")
      json.removeContextIris shouldEqual expected
    }
  }
}
