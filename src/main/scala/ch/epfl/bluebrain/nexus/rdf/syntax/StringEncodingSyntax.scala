package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.PctString.pctEncodedIgnore
import org.parboiled2.CharPredicate

trait StringEncodingSyntax {
  implicit final def stringSyntax(value: String): StringEncodingOpts = new StringEncodingOpts(value)
}
final class StringEncodingOpts(private val value: String) extends AnyVal {
  def pctEncodeIgnore(toIgnore: CharPredicate): String = pctEncodedIgnore(value, toIgnore)
}
