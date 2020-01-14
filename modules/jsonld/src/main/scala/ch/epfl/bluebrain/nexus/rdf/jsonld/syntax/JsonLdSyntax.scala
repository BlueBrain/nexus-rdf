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

}
