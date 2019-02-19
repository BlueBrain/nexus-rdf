package ch.epfl.bluebrain.nexus.rdf.circe

import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.circe.JenaModel.JenaModelErr.{InvalidJsonLD, Unexpected}
import ch.epfl.bluebrain.nexus.rdf.syntax.circe.context._
import io.circe.Json
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.apache.jena.riot.system.{StreamRDF, StreamRDFLib}
import org.apache.jena.riot.{Lang, RDFParser, RiotException}

import scala.util.Try

object JenaModel {

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

  private def messageOr(err: Throwable)(default: => String): String =
    Try(err.getMessage).filter(_ != null).getOrElse(default)

  /**
    * Enumeration type for JenaModel error operations
    */
  sealed trait JenaModelErr extends Product with Serializable
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
