package ch.epfl.bluebrain.nexus.rdf
import java.nio.charset.StandardCharsets.UTF_8

import ch.epfl.bluebrain.nexus.rdf.PctString._
import org.parboiled2.CharUtils.upperHexDigit

trait PctString {

  def value: String

  /**
    * @return the UTF-8 representation
    */
  lazy val asString: String =
    value

  /**
    * @return the string representation using percent-encoding for any character that
    *         is not contained in the Set ''pchar''
    */
  lazy val pctEncoded: String =
    pctEncode(value)
}
object PctString {

  private[rdf] val `sub-delims` = Set('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=').map(_.toByte)
  private[rdf] val unreserved = Set('-', '_', '.', '~').map(_.toByte) ++ ('a' to 'z')
    .map(_.toByte)
    .toSet ++ ('A' to 'Z')
    .map(_.toByte)
    .toSet ++ ('0' to '9').map(_.toByte).toSet
  private[rdf] val pchar = `sub-delims` ++ unreserved + ':'.toByte + '@'.toByte

  private[rdf] def pctEncode(s: String) = {
    val in = s.getBytes(UTF_8)
    in.foldLeft(new StringBuilder()) {
        case (sb, b) if pchar.contains(b) => sb.append(b.toChar)
        case (sb, b)                      => sb.append("%").append(upperHexDigit((b >> 4) & 0xF)).append(upperHexDigit(b & 0xF))
      }
      .toString()
  }
}
