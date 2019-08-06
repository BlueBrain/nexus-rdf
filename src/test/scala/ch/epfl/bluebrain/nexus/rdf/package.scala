package ch.epfl.bluebrain.nexus

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

package object rdf {
  def urlEncode(s: String): String = URLEncoder.encode(s, UTF_8.displayName())
}
