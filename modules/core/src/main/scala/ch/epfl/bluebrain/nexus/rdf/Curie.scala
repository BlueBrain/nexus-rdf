package ch.epfl.bluebrain.nexus.rdf

import cats.syntax.either._
import cats.syntax.show._
import cats.{Eq, Show}
import ch.epfl.bluebrain.nexus.rdf.Curie.Prefix
import ch.epfl.bluebrain.nexus.rdf.Iri.RelativeIri
import org.parboiled2.ErrorFormatter
import org.parboiled2.Parser.DeliveryScheme.{Either => E}

/**
  * A Compact URI as defined by W3C in ''CURIE Syntax 1.0''.
  * A curie is form by a ''prefix'', a '':'' and a ''reference''.
  * Example: xsd:integer
  *
  * @param prefix    the curie prefix
  * @param reference the curie reference
  */
final case class Curie(prefix: Prefix, reference: RelativeIri)

object Curie {

  /**
    * Attempt to construct a new [[Curie]] from the argument validating the structure and the character encodings as per
    * ''CURIE Syntax 1.0''.
    *
    * @param string the string to parse as a Curie.
    * @return Right(Curie) if the string conforms to specification, Left(error) otherwise
    */
  final def apply(string: String): Either[String, Curie] =
    new IriParser(string).`curie`
      .run()
      .leftMap(_.format(string, formatter))

  /**
    * The Compact URI prefix as defined by W3C in ''CURIE Syntax 1.0''.
    *
    * @param value the prefix value
    */
  final case class Prefix private[rdf] (value: String)

  object Prefix {

    /**
      * Attempt to construct a new [[Prefix]] from the argument validating the structure and the character encodings as per
      * ''CURIE Syntax 1.0''.
      *
      * @param string the string to parse as a Prefix.
      * @return Right(prefix) if the string conforms to specification, Left(error) otherwise
      */
    final def apply(string: String): Either[String, Prefix] =
      new IriParser(string).`prefix`
        .run()
        .leftMap(_.format(string, formatter))

    final implicit val prefixShow: Show[Prefix] = Show.show(_.value)
    final implicit val prefixEq: Eq[Prefix]     = Eq.fromUniversalEquals
  }

  final implicit def curieShow(implicit p: Show[Prefix], r: Show[RelativeIri]): Show[Curie] =
    Show.show { case Curie(prefix, reference) => prefix.show + ":" + reference.show }

  final implicit val curieEq: Eq[Curie] = Eq.fromUniversalEquals

  private val formatter = new ErrorFormatter(showExpected = false, showTraces = false)
}
