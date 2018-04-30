package ch.epfl.bluebrain.nexus.rdf.akka

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.akka.UriSyntax.UriOps

trait UriSyntax {

  /**
    * Implicit conversion for constructing an [[Iri]] from the provided [[Uri]]
    */
  final implicit def uriToIri(uri: Uri): Iri =
    Iri.unsafe(uri.toString())

  final implicit def akkaSyntaxUri(uri: Uri): UriOps =
    new UriOps(uri)

}

object UriSyntax extends UriSyntax {

  /**
    * Syntactic sugar for constructing an [[Iri]] from the [[Uri]]
    */
  private[akka] final class UriOps(private val uri: Uri) extends AnyVal {
    def toIri: Iri = uri
  }
}
