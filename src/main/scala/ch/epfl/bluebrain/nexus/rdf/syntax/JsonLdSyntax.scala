package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.circe.JsonLd
import io.circe.Json

trait JsonLdSyntax {
  implicit final def contextSyntax(json: Json): JsonLdOps = new JsonLdOps(json)
}

final class JsonLdOps(private val json: Json) extends AnyVal {

  def addContext(context: AbsoluteIri): Json = JsonLd.addContext(json, context)

  def contextValue: Json = JsonLd.contextValue(json)

  def mergeContext(that: Json): Json = JsonLd.mergeContext(json, that)

  def appendContextOf(that: Json): Json = JsonLd.appendContextOf(json, that)

  def removeContextIris: Json = JsonLd.removeContextIris(json)

  def id: Option[AbsoluteIri] = JsonLd.id(json)

  def replaceContext(that: Json): Json = JsonLd.replaceContext(json, that)

  def replaceContext(iri: AbsoluteIri): Json = JsonLd.replaceContext(json, iri)

  def removeKeys(keys: String*): Json = JsonLd.removeKeys(json, keys: _*)
}
