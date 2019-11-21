package ch.epfl.bluebrain.nexus.rdf.jena

import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url}
import ch.epfl.bluebrain.nexus.rdf.Node.Literal.LanguageTag
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary.rdf
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel.JenaModelErr.{InvalidJsonLD, Unexpected}
import ch.epfl.bluebrain.nexus.rdf.syntax.JsonLdSyntax
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Iri, Node}
import io.circe.Json
import org.apache.jena.datatypes.TypeMapper
import org.apache.jena.datatypes.xsd.XSDDatatype._
import org.apache.jena.rdf.model.impl.ResourceImpl
import org.apache.jena.rdf.model.{Literal => JenaLiteral, _}
import org.apache.jena.riot.system.{StreamRDF, StreamRDFLib}
import org.apache.jena.riot.{Lang, RDFParser, RiotException}
import ch.epfl.bluebrain.nexus.rdf.jena.JenaConversions._

import scala.util.Try

object JenaModel extends JsonLdSyntax {

  /**
    * Attempts to build a [[Model]] from a provided json parsed using JSON-LD algorithm.
    * The @base is extracted from the json @context when the ''base'' is None.
    *
    * @param json the json parsed into the [[Model]]
    * @param base the optionally available JSON-LD base
    * @return a Jena Model if there were no errors an a [[JenaModelErr]] when we encounter errors
    */
  def apply(json: Json, base: Option[AbsoluteIri] = None): Either[JenaModelErr, Model] = {
    val model  = ModelFactory.createDefaultModel()
    val stream = StreamRDFLib.graph(model.getGraph)
    base match {
      case Some(_) =>
        apply(json, base, model, stream)
      case _ =>
        json.hcursor.downField("@context").get[String]("@base") match {
          case Left(_) =>
            apply(json, None, model, stream)
          case Right(baseStr) =>
            Iri.absolute(baseStr).left.map(InvalidJsonLD).flatMap(b => apply(json, Some(b), model, stream))
        }
    }
  }

  /**
    * Attempts to convert the provided ''json'' into the provided ''model'' using a JSON-LD parser.
    * The @base is NOT extracted from the json @context when the ''base'' is None.
    *
    * @param json   the json parsed into the [[Model]]
    * @param base   the optionally available JSON-LD base
    * @param model  the Jena [[Model]]
    * @param stream the Jena [[StreamRDF]]. This is used for the Jena pipeline to process and transform the parsed triples
    * @return a Jena Model containing the triples from the provided JSON-LD if there were no errors an a [[JenaModelErr]] when we encounter errors
    */
  def apply(json: Json, base: Option[AbsoluteIri], model: Model, stream: StreamRDF): Either[JenaModelErr, Model] =
    Try {
      val j       = json deepMerge json.removeContextIris
      val baseStr = base.map(_.asString).getOrElse("")
      RDFParser.create.fromString(j.noSpaces).base(baseStr).lang(Lang.JSONLD).resolveURIs(false).parse(stream)
      model
    }.toEither.left.map {
      case err: IllegalArgumentException =>
        InvalidJsonLD(messageOr(err)("Illegal JsonLD character"))
      case err: RiotException =>
        InvalidJsonLD(messageOr(err)("Illegal JsonLD character"))
      case err =>
        Unexpected(messageOr(err)("Unknown error parsing JsonLD"))
    }

  /**
    * Converts the provided ''graph'' into a Jena Model.
    *
    * @param graph the graph
    * @return a Jena Model containing the triples from the provided graph
    */
  def apply(graph: Graph): Model = {
    val model = ModelFactory.createDefaultModel()
    val statements = graph.triples.foldLeft(Array.empty[Statement]) {
      case (acc, (s, o, p)) =>
        acc :+ ResourceFactory.createStatement(iriOrBNodeToResource(s), iriNodeToProperty(o), nodeToJenaRDFNode(p))
    }
    model.add(statements)
  }

  private def messageOr(err: Throwable)(default: => String): String =
    Try(err.getMessage).filter(_ != null).getOrElse(default)

  /**
    * Enumeration type for JenaModel error operations
    */
  sealed trait JenaModelErr extends Product with Serializable {

    /**
      * @return the human readable error details
      */
    def message: String
  }
  object JenaModelErr {

    /**
      * Signals an error while parsing the JSON-LD
      *
      * @param message the human readable error details
      */
    final case class InvalidJsonLD(message: String) extends JenaModelErr

    /**
      * Signals an unexpected error while generating a [[Model]]
      *
      * @param message the human readable error details
      */
    final case class Unexpected(message: String) extends JenaModelErr
  }

}

object JenaConversions {
  private val typeMapper = TypeMapper.getInstance()

  def nodeToJenaRDFNode(node: Node): RDFNode = node match {
    case b @ BNode(_)         => iriOrBNodeToResource(b)
    case i @ IriNode(_)       => iriOrBNodeToResource(i)
    case l @ Literal(_, _, _) => literalToJenaLiteral(l)
  }

  def iriOrBNodeToResource(iriOrBNode: IriOrBNode): Resource = iriOrBNode match {
    case BNode(id)    => new ResourceImpl(AnonId.create(id))
    case IriNode(iri) => ResourceFactory.createResource(iri.asString)
  }

  def iriNodeToProperty(iriNode: IriNode): Property =
    ResourceFactory.createProperty(iriNode.value.asString)

  def literalToJenaLiteral(literal: Literal): JenaLiteral = literal match {
    case Literal(lf, rdf.langString.value, Some(LanguageTag(tag))) => ResourceFactory.createLangLiteral(lf, tag)
    case Literal(lf, dataType, _)                                  => castToDatatype(lf, dataType).getOrElse(ResourceFactory.createStringLiteral(lf))
  }

  def jenaToLiteral(literal: JenaLiteral): Either[String, Literal] =
    if (literal.getLanguage == null || literal.getLanguage.isEmpty)
      if (literal.getDatatype == null || literal.getDatatype == XSDstring)
        Right(Literal(literal.getLexicalForm))
      else
        Option(literal.getDatatypeURI) match {
          case Some(dataType) =>
            Iri.url(dataType).left.map(errorMsg(dataType, _)).map(Literal(literal.getLexicalForm, _))
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
        Iri.url(uri).left.map(errorMsg(uri, _))
      case _ =>
        Right(b"${resource.getId.getLabelString}")
    }

  def propToIriNode(property: Property): Either[String, IriNode] =
    Iri.url(property.getURI).left.map(errorMsg(property.getURI, _))

  def rdfNodeToNode(rdfNode: RDFNode): Either[String, Node] =
    if (rdfNode.isLiteral)
      jenaToLiteral(rdfNode.asLiteral())
    else if (rdfNode.isAnon)
      Right(b"${rdfNode.asResource}")
    else
      Iri.url(rdfNode.asResource.getURI)

  private def errorMsg(iriString: String, err: String): String =
    s"'$iriString' could not be converted to Iri. Reason: '$err'"

  private def castToDatatype(lexicalText: String, dataType: AbsoluteIri): Option[JenaLiteral] =
    Try {
      val tpe     = typeMapper.getSafeTypeByName(dataType.asString)
      val literal = ResourceFactory.createTypedLiteral(lexicalText, tpe)
      literal.getValue //It will crash whenever the literal does not match the desired datatype
      literal
    }.toOption

  private implicit def eitherUriToNode(maybeUrl: Either[String, Url]): Either[String, IriNode] =
    maybeUrl.map(IriNode(_))
}
