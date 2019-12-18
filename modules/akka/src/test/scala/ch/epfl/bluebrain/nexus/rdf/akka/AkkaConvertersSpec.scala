package ch.epfl.bluebrain.nexus.rdf.akka

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.RdfSpec
import ch.epfl.bluebrain.nexus.rdf.akka.syntax.all._
import ch.epfl.bluebrain.nexus.rdf.implicits._

class AkkaConvertersSpec extends RdfSpec {

  "An Node" should {
    "be converted correctly to URI" when {
      "it's an IriNode with a valid URL" in {
        IriNode(url"http://example.com/path").asAkka.rightValue shouldEqual Uri("http://example.com/path")
      }
    }

    "fail to convert" when {
      "it's not an IriNode" in {
        BNode("1").rightValue.asAkka.leftValue shouldEqual "_:1 cannot be converted to URI."
        Literal(true).asAkka.leftValue shouldEqual "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean> cannot be converted to URI."
      }
      "it's an IriNode with Iri which is not a valid Uri" in {
        IriNode(url"http://example.com/päth").asAkka.leftValue shouldEqual "'http://example.com/päth' is not a valid URI."

      }
    }
  }

  "An AbsoluteIri" should {
    "be converted correctly to Uri" when {
      "it's a valid Uri" in {
        url"http://example.com/path".asAkka.rightValue shouldEqual Uri("http://example.com/path")
      }
    }
    "fail to convert" when {
      "it's not a valid Uri" in {
        url"http://example.com/päth".asAkka.leftValue shouldEqual "'http://example.com/päth' is not a valid URI."

      }
    }
  }

  "An Akka Uri" should {
    "convert to AbsoluteIri" in {
      Uri("http://example.com/path").asAbsoluteIri shouldEqual url"http://example.com/path"
    }
    "convert to IriNode" in {
      Uri("http://example.com/path").asRdfNode shouldEqual IriNode(url"http://example.com/path")
    }
  }

}
