package ch.epfl.bluebrain.nexus.rdf

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.TryValues

import scala.io.Source

package object syntax extends TryValues {

  final def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toTry.success.value
}
