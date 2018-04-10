package ch.epfl.bluebrain.nexus.rdf

import ch.epfl.bluebrain.nexus.rdf.predicates._
import fastparse.all._

/**
  * Scheme part of an Iri as defined by RFC 3987.
  *
  * @param value the string value of the scheme
  */
final case class Scheme private[rdf] (value: String) {

  /**
    * Whether this scheme identifies an URN.
    */
  def isUrn: Boolean = value equalsIgnoreCase "urn"

  /**
    * Whether this scheme identifies an HTTPS Iri.
    */
  def isHttps: Boolean = value equalsIgnoreCase "https"

  /**
    * Whether this scheme identifies an HTTP Iri.
    */
  def isHttp: Boolean = value equalsIgnoreCase "http"
}

object Scheme {

  /**
    * Attempts to construct a scheme from the argument string value.
    *
    * @param value the string representation of the scheme
    * @return Right(value) if successful or Left(error) if the string does not conform to the RFC 3987 format
    */
  final def apply(value: String): Either[String, Scheme] =
    (Start ~ `scheme` ~ End).parse(value) match {
      case Parsed.Success(_, _)      => Right(new Scheme(value))
      case Parsed.Failure(_, idx, _) => Left(s"Illegal scheme format, idx: '$idx'")
    }
}
