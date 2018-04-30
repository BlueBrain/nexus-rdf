package ch.epfl.bluebrain.nexus.rdf.akka

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Iri
import org.scalatest.{EitherValues, Matchers, WordSpecLike}

class ConversionsSpec extends WordSpecLike with Matchers with EitherValues {

  "An Iri" should {
    import ch.epfl.bluebrain.nexus.rdf.akka.iri._

    "be converted to a Uri" in {
      val expected = Uri("http://user:password@host/test")
      (Iri("http://user:password@host/test").right.value: Uri) shouldEqual expected
      Iri("http://user:password@host/test").right.value.toAkkaUri shouldEqual expected
    }
  }

  "A Uri" should {
    import ch.epfl.bluebrain.nexus.rdf.akka.uri._

    "be converted to an Iri" in {
      val expected = Iri("http://host/path?param1=value1#fragment").right.value
      (Uri("http://host/path?param1=value1#fragment"): Iri) shouldEqual expected
      Uri("http://host/path?param1=value1#fragment").toIri shouldEqual expected
    }
  }
}
