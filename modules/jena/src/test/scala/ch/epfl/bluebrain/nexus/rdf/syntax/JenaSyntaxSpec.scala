package ch.epfl.bluebrain.nexus.rdf.syntax

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Node.Literal.LanguageTag
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.syntax.jena._
import ch.epfl.bluebrain.nexus.rdf.syntax.node._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import org.apache.jena.datatypes.BaseDatatype
import org.apache.jena.rdf.model
import org.apache.jena.rdf.model.{Model, ModelFactory, ResourceFactory}
import org.scalatest.{Matchers, WordSpecLike}

class JenaSyntaxSpec extends WordSpecLike with Matchers {

  "Jena syntax" should {

    "convert string literal to Jena model" in {
      (Literal("testLiteral"): model.Literal) shouldEqual ResourceFactory.createStringLiteral("testLiteral")
    }

    "convert typed literal to Jena model" in {
      val jenaLiteral: model.Literal = Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)
      jenaLiteral.getLexicalForm shouldEqual "1999-04-09T20:00Z"
      jenaLiteral.getDatatypeURI shouldEqual "http://schema.org/Date"
    }

    "convert literal with lang to Jena model" in {
      (Literal("bonjour", LanguageTag("fr").toOption.get): model.Literal) shouldEqual ResourceFactory.createLangLiteral(
        "bonjour",
        "fr")
    }

    "convert IRI to Jena resource" in {
      (url"http://nexus.example.com/example-uri": model.Resource) shouldEqual ResourceFactory.createResource(
        "http://nexus.example.com/example-uri")
    }

    "convert blank node to Jena model" in {
      val id = UUID.randomUUID().toString
      (b"$id": model.Resource).getId.getLabelString shouldEqual id
    }

    "convert property to Jena model" in {
      (url"http://nexus.example.com/example-property": model.Property) shouldEqual ResourceFactory.createProperty(
        "http://nexus.example.com/example-property")
    }

    // format: off
    "convert Graph to Jena Model" in {
      val graph: Model = Graph(
        (url"http://nexus.example.com/john-doe", url"http://schema.org/name",                           "John Doe"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate",                      Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person")
      )
      val model = ModelFactory.createDefaultModel()
      model.read(getClass.getResourceAsStream("/simple-model.json"), "http://nexus.example.com/", "JSONLD")

      graph.triples shouldEqual model.triples
    }
    // format: on

    "convert string literal from Jena model" in {
      (ResourceFactory.createStringLiteral("testLiteral"): Literal) shouldEqual Literal("testLiteral")
    }

    "convert typed literal from Jena model" in {
      val convertedLiteral: Literal =
        ResourceFactory.createTypedLiteral("1999-04-09T20:00Z", new BaseDatatype("http://schema.org/Date"))
      convertedLiteral shouldEqual Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)
    }

    "convert literal with lang from Jena model" in {
      (ResourceFactory.createLangLiteral("bonjour", "fr"): Literal) shouldEqual Literal("bonjour",
                                                                                        LanguageTag("fr").toOption.get)
    }

    "convert IRI from Jena resource" in {
      (ResourceFactory.createResource("http://nexus.example.com/example-uri"): IriOrBNode) shouldEqual url"http://nexus.example.com/example-uri"
    }

    "convert blank node from Jena model" in {
      val jenaResource = ResourceFactory.createResource()
      val id           = jenaResource.getId.getLabelString

      (jenaResource: IriOrBNode) shouldEqual b"$id"
    }

    "convert property from Jena model" in {
      (ResourceFactory.createProperty("http://nexus.example.com/example-property"): IriNode) shouldEqual url"http://nexus.example.com/example-property"
    }

    "convert Jena Model to Graph" in {
      val model = ModelFactory.createDefaultModel()
      model.read(getClass.getResourceAsStream("/simple-model.json"), "http://nexus.example.com/", "JSONLD")

      // format: off
      model.triples shouldEqual Set[Graph.Triple](
        (url"http://nexus.example.com/john-doe", url"http://schema.org/name",                           "John Doe"),
        (url"http://nexus.example.com/john-doe", url"http://schema.org/birthDate",                      Literal("1999-04-09T20:00Z", url"http://schema.org/Date".value)),
        (url"http://nexus.example.com/john-doe", url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",  url"http://schema.org/Person")
      )
      // format: off
    }

  }

}
