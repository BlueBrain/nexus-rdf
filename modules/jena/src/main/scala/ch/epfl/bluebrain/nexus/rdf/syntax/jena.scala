package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url}
import ch.epfl.bluebrain.nexus.rdf.Node.Literal.LanguageTag
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{GraphConfiguration, Iri, Node}
import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.datatypes.xsd.XSDDatatype._
import org.apache.jena.rdf.model.impl.ResourceImpl
import org.apache.jena.rdf.model.{Literal => JenaLiteral, _}

import scala.util.Try

object jena extends JenaSyntax {
  private implicit def eitherUriToNode(maybeUrl: Either[String, Url]): Either[String, IriNode] =
    maybeUrl.map(IriNode(_))

  private val typeMapper = TypeMapper.getInstance()

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
    case Literal(lf, rdf.langString.value, Some(LanguageTag(tag))) => ResourceFactory.createLangLiteral(lf, tag)
    case Literal(lf, dataType, _)                                  => castToDatatype(lf, dataType).getOrElse(ResourceFactory.createStringLiteral(lf))
  }

  private def castToDatatype(lexicalText: String, dataType: AbsoluteIri): Option[JenaLiteral] =
    Try {
      val tpe     = typeMapper.getSafeTypeByName(dataType.asString)
      val literal = ResourceFactory.createTypedLiteral(lexicalText, tpe)
      literal.getValue //It will crash whenever the literal does not match the desired datatype
      literal
    }.toOption

  def jenaToLiteral(literal: JenaLiteral)(implicit config: GraphConfiguration): Either[String, Literal] =
    if (literal.getLanguage == null || literal.getLanguage.isEmpty)
      if (literal.getDatatype == null || literal.getDatatype == XSDstring)
        if (config.castDateTypes)
          (castToDatatype(literal.getLexicalForm, xsd.dateTime.value) orElse
            castToDatatype(literal.getLexicalForm, xsd.date.value) orElse
            castToDatatype(literal.getLexicalForm, xsd.time.value)) match {
            case Some(l) =>
              val tpe = l.getDatatypeURI
              Iri.url(tpe).left.map(errorMsg(tpe, _)).map(Literal(l.getLexicalForm, _))
            case _ =>
              Right(Literal(literal.getLexicalForm))
          } else
          Right(Literal(literal.getLexicalForm))
      else
        Option(literal.getDatatypeURI) match {
          case Some(dataType) =>
            Iri.url(dataType).left.map(errorMsg(dataType, _)).map(Literal(literal.getLexicalForm, _))
          case _ => Right(Literal(literal.getLexicalForm))
        } else
      Right(
        LanguageTag(literal.getLanguage)
          .map(Literal(literal.getLexicalForm, _))
          .getOrElse(Literal(literal.getLexicalForm)))

  def toIriOrBNode(resource: Resource): Either[String, IriOrBNode] =
    Option(resource.getURI) match {
      case Some(uri) if !uri.isEmpty =>
        Iri.url(uri).left.map(errorMsg(uri, _))
      case _ =>
        Right(b"${resource.getId.getLabelString}")
    }

  def propToIriNode(property: Property): Either[String, IriNode] =
    Iri.url(property.getURI).left.map(errorMsg(property.getURI, _))

  def rdfNodeToNode(rdfNode: RDFNode)(implicit config: GraphConfiguration): Either[String, Node] =
    if (rdfNode.isLiteral)
      jenaToLiteral(rdfNode.asLiteral())
    else if (rdfNode.isAnon)
      Right(b"${rdfNode.asResource}")
    else
      Iri.url(rdfNode.asResource.getURI)

  private def errorMsg(iriString: String, err: String): String =
    s"'$iriString' could not be converted to Iri. Reason: '$err'"
}
