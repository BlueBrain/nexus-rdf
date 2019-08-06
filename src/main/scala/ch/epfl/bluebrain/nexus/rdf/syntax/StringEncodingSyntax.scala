package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.PctString.{pctEncodedIgnore, pctEncodedInclude}

trait StringEncodingSyntax {
  implicit final def stringSyntax(value: String): StringEncodingOpts = new StringEncodingOpts(value)
}
final class StringEncodingOpts(private val value: String) extends AnyVal {
  def pctEncodeInclude(toEncode: Set[Char]): String = pctEncodedInclude(value, toEncode)
  def pctEncodeIgnore(toIgnore: Set[Char]): String  = pctEncodedIgnore(value, toIgnore)
}
