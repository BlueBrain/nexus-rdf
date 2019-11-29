package ch.epfl.bluebrain.nexus.rdf

import org.scalactic.source
import org.scalatest.{EitherValues, Inspectors, Matchers, OptionValues, TryValues, WordSpecLike}
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

trait RdfSpec
    extends WordSpecLike
    with Matchers
    with Inspectors
    with EitherValues
    with OptionValues
    with TryValues
    with Resources {

  class EitherValuable[L, R](either: Either[L, R], pos: source.Position) {
    def rightValue: R = either match {
      case Right(value) => value
      case Left(_) =>
        throw new TestFailedException((_: StackDepthException) => Some("The Either value is not a Right(_)"), None, pos)
    }

    def leftValue: L = either match {
      case Left(value) => value
      case Right(_) =>
        throw new TestFailedException((_: StackDepthException) => Some("The Either value is not a Left(_)"), None, pos)
    }
  }

  implicit def convertEitherToValuable[L, R](either: Either[L, R])(implicit p: source.Position): EitherValuable[L, R] =
    new EitherValuable(either, p)

}
