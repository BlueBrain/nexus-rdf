package ch.epfl.bluebrain.nexus.rdf
import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

object PctString {
  private[rdf] val `sub-delims` = Set('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=')
  private[rdf] val alpha        = ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  private[rdf] val numeric      = ('0' to '9').toSet
  private[rdf] val unreserved   = Set('-', '_', '.', '~') ++ alpha ++ numeric
  private[rdf] val pchar        = `sub-delims` ++ unreserved + ':' + '@'
  private[rdf] val `gen-delims` = Set(':', '/', '?', '#', '[', ']', '@')

  private[rdf] def pctEncodedIgnore(s: String, toIgnore: Set[Char]) =
    s.foldLeft(new StringBuilder()) {
        case (sb, b) if toIgnore.contains(b) => sb.append(b)
        case (sb, b)                         => pctEncode(sb, b)
      }
      .toString()

  private[rdf] def pctEncodedInclude(s: String, toEncode: Set[Char]) =
    s.foldLeft(new StringBuilder()) {
        case (sb, b) if toEncode.contains(b) => pctEncode(sb, b)
        case (sb, b)                         => sb.append(b)
      }
      .toString()

  private def pctEncode(sb: StringBuilder, char: Char): StringBuilder =
    sb.append(URLEncoder.encode(char.toString, UTF_8.displayName()))
}
