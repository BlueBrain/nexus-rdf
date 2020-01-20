package ch.epfl.bluebrain.nexus.rdf

/**
  * A simple non empty string implementation
  *
  * @param head the head character
  * @param rest the rest of the string
  */
final case class NonEmptyString(head: Char, rest: String) {

  /**
    * Returns the underlying string representation
    */
  val asString: String = s"$head$rest"

  override def toString: String = asString

}

object NonEmptyString {

  /**
    * Attempts to create a [[NonEmptyString]]
    *
    * @param string the passed string value
    * @return Some(nonEmptyString) if the passed string is not empty, None otherwise
    */
  def apply(string: String): Option[NonEmptyString] =
    if (string.isBlank) None
    else Some(NonEmptyString(string.head, string.drop(1)))
}
