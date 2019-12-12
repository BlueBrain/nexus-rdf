package ch.epfl.bluebrain.nexus.rdf.derivation

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url}
import ch.epfl.bluebrain.nexus.rdf.Node.Literal.LanguageTag
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary.rdf
import ch.epfl.bluebrain.nexus.rdf.implicits._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Iri, Node}
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.datatypes.xsd.XSDDatatype._
import org.apache.jena.rdf.model.impl.ResourceImpl
import org.apache.jena.rdf.model.{Literal => JenaLiteral, _}
import org.apache.jena.riot.system.StreamRDFLib
import org.apache.jena.riot.{Lang, RDFParser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.Try

trait DerivationSpec extends AnyWordSpecLike with Matchers {

  final def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString)
      .getOrElse(throw new IllegalArgumentException)

  final def jsonWithContext(resourcePath: String): Json =
    jsonContentOf("/context.json") deepMerge jsonContentOf(resourcePath)

  def toJenaModel(g: Graph): Model = {
    def castToDatatype(lexicalText: String, dataType: AbsoluteIri): Option[JenaLiteral] =
      Try {
        val typeMapper = TypeMapper.getInstance()
        val tpe        = typeMapper.getSafeTypeByName(dataType.asString)
        val literal    = ResourceFactory.createTypedLiteral(lexicalText, tpe)
        literal.getValue //It will crash whenever the literal does not match the desired datatype
        literal
      }.toOption

    val model = ModelFactory.createDefaultModel()
    g.triples.foreach {
      case (s, p, o) =>
        val sr = s match {
          case IriNode(iri) => ResourceFactory.createResource(iri.asUri)
          case BNode(id)    => new ResourceImpl(AnonId.create(id))
        }
        val pp = ResourceFactory.createProperty(p.value.asUri)
        val or = o match {
          case Literal(lf, rdf.langString, Some(LanguageTag(tag))) =>
            ResourceFactory.createLangLiteral(lf, tag)
          case Literal(lf, dataType, _) =>
            castToDatatype(lf, dataType).getOrElse(ResourceFactory.createStringLiteral(lf))
          case IriNode(iri) => ResourceFactory.createResource(iri.asUri)
          case BNode(id)    => new ResourceImpl(AnonId.create(id))
        }
        val stmt = ResourceFactory.createStatement(sr, pp, or)
        model.add(stmt)
    }
    model
  }

  def toJenaModel(j: Json): Model = {
    val model  = ModelFactory.createDefaultModel()
    val stream = StreamRDFLib.graph(model.getGraph)
    RDFParser.create.fromString(j.noSpaces).lang(Lang.JSONLD).parse(stream)
    model
  }

  def fromJenaModel(id: AbsoluteIri, model: Model): Graph = {
    def jenaToLiteral(literal: JenaLiteral): Either[String, Literal] =
      if (literal.getLanguage == null || literal.getLanguage.isEmpty)
        if (literal.getDatatype == null || literal.getDatatype == XSDstring)
          Right(Literal(literal.getLexicalForm))
        else
          Option(literal.getDatatypeURI) match {
            case Some(dataType) =>
              Iri.url(dataType).leftMap(errorMsg(dataType, _)).map(Literal(literal.getLexicalForm, _))
            case _ => Right(Literal(literal.getLexicalForm))
          }
      else
        Right(
          LanguageTag(literal.getLanguage)
            .map(Literal(literal.getLexicalForm, _))
            .getOrElse(Literal(literal.getLexicalForm))
        )

    def toIriOrBNode(resource: Resource): Either[String, IriOrBNode] =
      Option(resource.getURI) match {
        case Some(uri) if !uri.isEmpty =>
          Iri.url(uri).leftMap(errorMsg(uri, _))
        case _ =>
          Right(b"${resource.getId.getLabelString}")
      }

    def propToIriNode(property: Property): Either[String, IriNode] =
      Iri.url(property.getURI).leftMap(errorMsg(property.getURI, _))

    def rdfNodeToNode(rdfNode: RDFNode): Either[String, Node] =
      if (rdfNode.isLiteral)
        jenaToLiteral(rdfNode.asLiteral())
      else if (rdfNode.isAnon)
        Right(b"${rdfNode.asResource}")
      else
        Iri.url(rdfNode.asResource.getURI)

    def errorMsg(iriString: String, err: String): String =
      s"'$iriString' could not be converted to Iri. Reason: '$err'"

    implicit def eitherUriToNode(maybeUrl: Either[String, Url]): Either[String, IriNode] =
      maybeUrl.map(IriNode(_))

    model.listStatements().asScala.foldLeft(Graph(id)) {
      case (g, stmt) =>
        val s      = toIriOrBNode(stmt.getSubject).getOrElse(throw new IllegalArgumentException)
        val p      = propToIriNode(stmt.getPredicate).getOrElse(throw new IllegalArgumentException)
        val o      = rdfNodeToNode(stmt.getObject).getOrElse(throw new IllegalArgumentException)
        val triple = (s, p, o)
        g + triple
    }
  }

  def toString(stmt: Statement): String = {
    s"${stmt.getSubject.toString} ${stmt.getPredicate.toString} ${stmt.getObject.toString} ."
  }

}
