package ch.epfl.bluebrain.nexus.rdf
import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

import org.parboiled2.CharPredicate

object PctString {

  private val UTF8 = UTF_8.displayName()
  private[rdf] def pctEncodedIgnore(s: String, toIgnore: CharPredicate) =
    s.foldLeft(new StringBuilder()) {
        case (sb, b) if toIgnore.matchesAny(b.toString) =>
          sb.append(b)
        case (sb, b) =>
          pctEncode(sb, b)
      }
      .toString()

  private def pctEncode(sb: StringBuilder, char: Char): StringBuilder =
    if (char == ' ') sb.append("%20")
    else sb.append(URLEncoder.encode(char.toString, UTF8))
}
