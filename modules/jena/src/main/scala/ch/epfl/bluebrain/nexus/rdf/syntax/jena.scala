package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph.Triple
import ch.epfl.bluebrain.nexus.rdf.Node.Literal.LanguageTag
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}
import org.apache.jena.datatypes.BaseDatatype
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.rdf.model.impl.ResourceImpl
import org.apache.jena.rdf.model.{Literal => JenaLiteral, _}

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

  final implicit def iriNodeToProperty(iriNode: IriNode): Property =
    ResourceFactory.createProperty(iriNode.value.asString)

  final implicit def literalToJenaLiteral(literal: Literal): JenaLiteral = literal match {
    case Literal(lf, xsd.string.value, None)                       => ResourceFactory.createStringLiteral(lf)
    case Literal(lf, xsd.boolean.value, _)                         => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDboolean)
    case Literal(lf, xsd.integer.value, _)                         => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDinteger)
    case Literal(lf, xsd.int.value, _)                             => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDint)
    case Literal(lf, xsd.short.value, _)                           => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDshort)
    case Literal(lf, xsd.double.value, _)                          => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDdouble)
    case Literal(lf, xsd.decimal.value, _)                         => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDdecimal)
    case Literal(lf, xsd.float.value, _)                           => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDfloat)
    case Literal(lf, xsd.dateTime.value, _)                        => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDdateTime)
    case Literal(lf, xsd.byte.value, _)                            => ResourceFactory.createTypedLiteral(lf, XSDDatatype.XSDbyte)
    case Literal(lf, rdf.langString.value, Some(LanguageTag(tag))) => ResourceFactory.createLangLiteral(lf, tag)
    case Literal(lf, dataType, _)                                  => ResourceFactory.createTypedLiteral(lf, new BaseDatatype(dataType.asString))
  }

  final implicit def resourceToIriOrBNode(resource: Resource): IriOrBNode =
    Option(resource.getURI)
      .map(uri => url"$uri")
      .getOrElse(b"${resource.getId.getLabelString}")

  final implicit def propertyToIriNode(property: Property): IriNode =
    url"${property.getURI}"

  final implicit def jenaLiteralToLiteral(literal: JenaLiteral): Literal =
    if (literal.getLanguage.isEmpty)
      Option(literal.getDatatypeURI)
        .map(dataType => Literal(literal.getLexicalForm, url"$dataType".value))
        .getOrElse(Literal(literal.getLexicalForm))
    else
      LanguageTag(literal.getLanguage)
        .map(Literal(literal.getLexicalForm, _))
        .getOrElse(Literal(literal.getLexicalForm))

  final implicit def jenaRDFNodeToNode(rdfNode: RDFNode): Node = {
    if (rdfNode.isLiteral)
      jenaLiteralToLiteral(rdfNode.asLiteral())
    else if (rdfNode.isAnon)
      b"${rdfNode.asResource}"
    else
      url"${rdfNode.asResource.getURI}"

  }

  final implicit def toJena(graph: Graph): Model =
    graph.triples.foldLeft(ModelFactory.createDefaultModel()) {
      case (model, (s, o, p)) => model.add(ResourceFactory.createStatement(s, o, p))
    }

  final implicit def toGraph(model: Model): Graph =
    Graph(model.listStatements().asScala.foldLeft(Set.empty[Triple]) { (acc, s) =>
      acc + ((s.getSubject, s.getPredicate, s.getObject))
    })
}
