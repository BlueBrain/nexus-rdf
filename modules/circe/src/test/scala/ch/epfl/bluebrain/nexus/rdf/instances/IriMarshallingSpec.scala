package ch.epfl.bluebrain.nexus.rdf.instances

import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Path}
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
  }

  "An AbsoluteIri" should {
    "be encoded" in {
      iri.asAbsolute.value.asJson shouldEqual Json.fromString(iriString)
    }
    "be decoded" in {
      Json.fromString(iriString).as[AbsoluteIri].right.value shouldEqual iri.asAbsolute.value
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
  }

}
