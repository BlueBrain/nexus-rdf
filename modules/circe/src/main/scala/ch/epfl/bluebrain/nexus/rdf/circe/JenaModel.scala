package ch.epfl.bluebrain.nexus.rdf.circe

import ch.epfl.bluebrain.nexus.rdf.circe.JenaModel.JenaModelErr.{InvalidJsonLD, Unexpected}
import ch.epfl.bluebrain.nexus.rdf.syntax.circe.context._
import io.circe.Json
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.apache.jena.riot.system.StreamRDFLib
import org.apache.jena.riot.{Lang, RDFParser, RiotException}

import scala.util.Try

object JenaModel {

  /**
    * @param json the json to be converted into a [[Model]]
    * @return a [[Model]] if there were no errors an a [[JenaModelErr]] when we encounter errors
    *         during model creation.
    */
  def apply(json: Json): Either[JenaModelErr, Model] = {
    val base = json.hcursor.downField("@context").get[String]("@base").getOrElse("")
    apply(json, base)
  }

  /**
    *
    * @param json the json to be converted into a [[Model]]
    * @param base the JSON-LD base
    * @return a Jena Model if there were no errors an a [[JenaModelErr]] when we encounter errors
    *         during model creation.
    */
  def apply(json: Json, base: String): Either[JenaModelErr, Model] = {
    Try {
      val model     = ModelFactory.createDefaultModel()
      val finalJson = json deepMerge json.removeContextIris
      val stream    = StreamRDFLib.graph(model.getGraph)
      RDFParser.create.fromString(finalJson.noSpaces).base(base).lang(Lang.JSONLD).resolveURIs(false).parse(stream)
      model
    }.toEither.left.map {
      case err: IllegalArgumentException =>
        InvalidJsonLD(messageOr(err)("Illegal JsonLD character"))
      case err: RiotException =>
        InvalidJsonLD(messageOr(err)("Illegal JsonLD character"))
      case err =>
        Unexpected(messageOr(err)("Unknown error parsing JsonLD"))
    }
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
