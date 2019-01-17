package ch.epfl.bluebrain.nexus.rdf.instances

import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Path, RelativeIri, Url, Urn}
import io.circe.Json
import org.scalatest._
import io.circe.syntax._

class IriMarshallingSpec extends WordSpecLike with Matchers with Inspectors with EitherValues with OptionValues {
  private val iriString = "http://example.com/a/b?c=d"
  private val iri: Iri  = Iri.absolute(iriString).right.value

  "An Iri" should {
    "be encoded" in {
      iri.asJson shouldEqual Json.fromString(iriString)
    }
    "be decoded" in {
      Json.fromString(iriString).as[Iri].right.value shouldEqual iri
    }
    "fail to decode" in {
      Json.fromString("").as[Iri].left
    }
  }

  "An AbsoluteIri" should {
    "be encoded" in {
      iri.asAbsolute.value.asJson shouldEqual Json.fromString(iriString)
    }
    "be decoded" in {
      Json.fromString(iriString).as[AbsoluteIri].right.value shouldEqual iri.asAbsolute.value
    }
    "fail to decode" in {
      Json.fromString("/a/b/c").as[AbsoluteIri].left
    }
  }

  "A Path" should {
    val pathString = "/a/b"
    val path       = iri.asAbsolute.value.path
    "be encoded" in {
      path.asJson shouldEqual Json.fromString(pathString)
    }
    "be decoded" in {
      Json.fromString(pathString).as[Path].right.value shouldEqual path
    }
    "fail to decode" in {
      Json.fromString("https://example.com").as[Path].left
    }
  }

  "A Url" should {
    val urlString = "http:://example.com/a"
    val url       = Iri.url("http:://example.com/a").right.value
    "be encoded" in {
      url.asJson shouldEqual Json.fromString(urlString)
    }
    "be decoded" in {
      Json.fromString(urlString).as[Url].right.value shouldEqual url
    }
    "fail to decode" in {
      Json.fromString("urn:example:a£/bÆc//:://?=a=b#").as[Url].left
    }
  }

  "A Urn" should {
    val urnString = "urn:example:a£/bÆc//:://?=a=b#"
    val urn       = Iri.urn("urn:example:a£/bÆc//:://?=a=b#").right.value
    "be encoded" in {
      urn.asJson shouldEqual Json.fromString(urnString)
    }
    "be decoded" in {
      Json.fromString(urnString).as[Urn].right.value shouldEqual urn
    }
    "fail to decode" in {
      Json.fromString("https://example.com").as[Urn].left
    }
  }

  "A RelativeIri" should {
    val relativeIriString = "../../../"
    val relativeIri       = Iri.relative("../../../").right.value
    "be encoded" in {
      relativeIri.asJson shouldEqual Json.fromString(relativeIriString)
    }
    "be decoded" in {
      Json.fromString(relativeIriString).as[RelativeIri].right.value shouldEqual relativeIri
    }
    "fail to decode" in {
      Json.fromString("https://example.com").as[RelativeIri].left
    }
  }
}
