package ch.epfl.bluebrain.nexus.rdf

import cats.syntax.either._
import cats.{Eq, Show}
import org.parboiled2.Parser.DeliveryScheme.Either
import org.parboiled2._

import scala.collection.SortedMap
import scala.collection.immutable.SortedSet

/**
  * Query part of an Iri as defined in RFC 3987.
  *
  * @param value a sorted multi map that represents all key -> value pairs
  */
final case class Query private[rdf] (value: SortedMap[String, SortedSet[String]])

object Query {

  /**
    * Attempts to parse the argument string as an `iquery` as defined by RFC 3987 and evaluate the key=value pairs.
    *
    * @param string the string to parse as a Query
    * @return Right(Query) if the parse succeeds, Left(error) otherwise
    */
  final def apply(string: String): Either[String, Query] =
    new IriParser(string).`iquery`
      .run()
      .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

  final implicit val queryShow: Show[Query] =
    Show.show(
      _.value
        .map {
          case (k, s) =>
            s.map {
                case v if v.isEmpty => k
                case v              => s"$k=$v"
              }
              .mkString("&")
        }
        .mkString("&"))

  final implicit val queryEq: Eq[Query] =
    Eq.fromUniversalEquals
}
