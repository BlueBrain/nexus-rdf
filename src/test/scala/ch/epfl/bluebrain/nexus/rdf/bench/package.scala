package ch.epfl.bluebrain.nexus.rdf

import io.circe.Json
import io.circe.parser.parse

import scala.io.Source

package object bench {
  private[bench] def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toOption.get
}
