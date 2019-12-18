package ch.epfl.bluebrain.nexus.rdf.akka

import akka.http.scaladsl.model.Uri
import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode

import scala.util.Try

/**
  * Conversions from rdf data types and Akka [[akka.http.scaladsl.model.Uri]].
  */
trait ToAkkaConverters {

  /**
    * Attempts to convert argument [[Node]] to Akka [[Uri]].
    */
  def asAkka(node: Node): Either[String, Uri] = node match {
    case IriNode(iri) => asAkka(iri)
    case other        => Left(s"${other.show} cannot be converted to URI.")
  }

  /**
    * Attempts to convert argument [[AbsoluteIri]] to Akka [[Uri]].
    */
  def asAkka(iri: AbsoluteIri): Either[String, Uri] =
    Try {
      Uri.apply(iri.toString)
    }.toEither.left.map(_ => s"'${iri.toString}' is not a valid URI.")

}
