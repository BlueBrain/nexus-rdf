package ch.epfl.bluebrain.nexus.rdf

import cats.{Eq, Show}
import org.parboiled2.ErrorFormatter

/**
  * Port part of an Iri as defined by RFC 3987.
  *
  * @param value the underlying int value
  */
final case class Port private[rdf] (value: Int)

object Port {

  /**
    * Attempts to construct a port from the argument int value.  Valid values are [0, 65535].
    *
    * @param value the underlying port value
    * @return Right(port) if successful, Left(error) otherwise
    */
  final def apply(value: Int): Either[String, Port] =
    if (value >= 0 && value < 65536) Right(new Port(value))
    else Left("Port value must in range [0, 65535]")

  /**
    * Attempts to construct a port from the argument string value.  Valid values are [0, 65535] without leading zeroes.
    *
    * @param string the string representation of the port
    * @return Right(port) if successful, Left(error) otherwise
    */
  final def apply(string: String): Either[String, Port] = {
    import org.parboiled2.Parser.DeliveryScheme.Either
    new IriParser(string).`port`.run() match {
      case Left(pe)         => Left(pe.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))
      case Right(Left(err)) => Left(err)
      case Right(r)         => r
    }
  }

  final implicit val schemeShow: Show[Port] = Show.show(_.value.toString)
  final implicit val schemeEq: Eq[Port]     = Eq.fromUniversalEquals
}
