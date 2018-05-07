package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.Literal.{rdfsyntax, xsd, LanguageTag}
import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import org.apache.jena.rdf.model.{
  AnonId,
  Model,
  ModelFactory,
  Property,
  RDFNode,
  Resource,
  ResourceFactory,
  Literal => JenaLiteral
}
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import org.apache.jena.datatypes.BaseDatatype
import org.apache.jena.rdf.model.impl.ResourceImpl

import scala.collection.JavaConverters._
object jena {

  final implicit def nodeToJenaRDFNode(node: Node): RDFNode = node match {
    case b @ BNode(_)         => iriOrBNodeToResource(b)
    case i @ IriNode(_)       => iriOrBNodeToResource(i)
    case l @ Literal(_, _, _) => literalToJenaLiteral(l)
  }

  final implicit def iriOrBNodeToResource(iriOrBNode: IriOrBNode): Resource = iriOrBNode match {
    case BNode(id)    => new ResourceImpl(AnonId.create(id))
    case IriNode(iri) => ResourceFactory.createResource(iri.asString)

  }

  final implicit def iriNodeToProperty(iriNode: IriNode): Property = {
    ResourceFactory.createProperty(iriNode.value.asString)
  }

  final implicit def literalToJenaLiteral(literal: Literal): JenaLiteral = literal match {
    case Literal(lf, xsd.string, None)                             => ResourceFactory.createStringLiteral(lf)
    case Literal(lf, rdfsyntax.langString, Some(LanguageTag(tag))) => ResourceFactory.createLangLiteral(lf, tag)
    case Literal(lf, dataType, _)                                  => ResourceFactory.createTypedLiteral(lf, new BaseDatatype(dataType.asString))
  }

  final implicit def resourceToIriOrBNode(resource: Resource): IriOrBNode = {
    Option(resource.getURI)
      .map(uri => url"$uri")
      .getOrElse(b"${resource.getId.getLabelString}")
  }

  final implicit def propertyToIriNode(property: Property): IriNode = {
    url"${property.getURI}"
  }

  final implicit def jenaLiteralToLiteral(literal: JenaLiteral): Literal = {
    if (literal.getLanguage.isEmpty) {
      Option(literal.getDatatypeURI)
        .map(dataType => Literal(literal.getLexicalForm, url"$dataType".value))
        .getOrElse(Literal(literal.getLexicalForm))
    } else {
      LanguageTag(literal.getLanguage)
        .map(Literal(literal.getLexicalForm, _))
        .getOrElse(Literal(literal.getLexicalForm))
    }

  }
  final implicit def jenaRDFNodeToNode(rdfNode: RDFNode): Node = {
    if (rdfNode.isLiteral) {
      jenaLiteralToLiteral(rdfNode.asLiteral())
    } else {
      url"${rdfNode.asResource.getURI}"
    }
  }

  final implicit def toJena(graph: Graph): Model = {
    val model = ModelFactory.createDefaultModel()

    val statements = graph.triples
      .map {
        case (s, o, p) => ResourceFactory.createStatement(s, o, p)
      }
      .toList
      .asJava
    model.add(statements)
    model
  }

  final implicit def toGraph(model: Model): Graph = {
    val triples: Set[Graph.Triple] =
      model.listStatements().asScala.map[Graph.Triple](s => (s.getSubject, s.getPredicate, s.getObject)).toSet
    Graph(triples)
  }

}
