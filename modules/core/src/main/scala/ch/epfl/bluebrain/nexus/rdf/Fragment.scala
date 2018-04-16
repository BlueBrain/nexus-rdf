package ch.epfl.bluebrain.nexus.rdf

import cats.syntax.either._
import cats.{Eq, Show}
import org.parboiled2.ErrorFormatter
import org.parboiled2.Parser.DeliveryScheme.Either

/**
  * Fragment part of an Iri as defined by RFC 3987.
  *
  * @param value the string value of the fragment
  */
final case class Fragment private[rdf] (value: String)

object Fragment {

  /**
    * Attempts to parse the argument string as an `ifragment` as defined by RFC 3987.
    *
    * @param string the string to parse as a Fragment
    * @return Right(Fragment) if the parse succeeds, Left(error) otherwise
    */
  final def apply(string: String): Either[String, Fragment] =
    new IriParser(string).`ifragment`
      .run()
      .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

  final implicit val fragmentShow: Show[Fragment] = Show.show(_.value)
  final implicit val fragmentEq: Eq[Fragment]     = Eq.fromUniversalEquals
}
