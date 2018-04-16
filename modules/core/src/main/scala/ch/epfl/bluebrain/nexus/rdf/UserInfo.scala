package ch.epfl.bluebrain.nexus.rdf

import cats.syntax.either._
import cats.{Eq, Show}
import org.parboiled2.ErrorFormatter

/**
  * A user info representation as specified by RFC 3987.
  *
  * @param value the underlying string representation
  */
final case class UserInfo private[rdf] (value: String) {

  /**
    * As per the specification the user info is case sensitive.  This method allows comparing two user info values
    * disregarding the character casing.
    *
    * @param that the user info to compare to
    * @return true if the underlying values are equal (diregarding their case), false otherwise
    */
  def equalsIgnoreCase(that: UserInfo): Boolean =
    this.value equalsIgnoreCase that.value
}

object UserInfo {

  /**
    * Attempt to construct a new UserInfo from the argument validating the character encodings as per RFC 3987.
    *
    * @param string the string to parse as a user info.
    * @return Right(UserInfo(value)) if the string conforms to specification, Left(error) otherwise
    */
  final def apply(string: String): Either[String, UserInfo] = {
    import org.parboiled2.Parser.DeliveryScheme.Either
    new IriParser(string).`iuserinfo`
      .run()
      .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))
  }

  final implicit val schemeShow: Show[UserInfo] = Show.show(_.value)
  final implicit val schemeEq: Eq[UserInfo]     = Eq.fromUniversalEquals
}
