package ch.epfl.bluebrain.nexus.rdf.jsonld.syntax

import cats.Monad
import cats.data.EitherT
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.jsonld.JsonLd
import ch.epfl.bluebrain.nexus.rdf.jsonld.JsonLd.ContextResolutionException
import io.circe.Json

trait JsonLdSyntax {
  implicit final def contextSyntax(json: Json): JsonLdOps = new JsonLdOps(json)
}

final class JsonLdOps(private val json: Json) extends AnyVal {

  /**
    * Resolve all IRIs inside @context.
    *
    * @param resolver context resolver
    * @return [[Json]] with all the context IRIs resolved
    */
  def resolveContext[F[_]: Monad](
      resolver: AbsoluteIri => F[Option[Json]]
  ): EitherT[F, ContextResolutionException, Json] =
    JsonLd.resolveContext(json)(resolver)

  /**
    * @return a new Json with the values of all the ''@context'' keys
    */
  def contextValue: Json = JsonLd.contextValue(json)

  /**
    * Replaces the @context value from the provided json to the one in ''that'' json
    *
    * @param context the json with a @context to override the @context in the provided ''json''
    */
  def replaceContext(context: Json): Json = JsonLd.replaceContext(json, context)

  /**
    * Removes the provided keys from everywhere on the json.
    *
    * @param keys list of ''keys'' to be removed from the top level of the ''json''
    * @return the original json without the provided ''keys''
    */
  def removeNestedKeys(keys: String*): Json = JsonLd.removeNestedKeys(json, keys: _*)

}
