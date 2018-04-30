package ch.epfl.bluebrain.nexus.rdf.akka

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Iri

trait UriOps {

  /**
    * Implicit conversion for constructing an [[Iri]] from the provided [[Uri]]
    */
  final implicit def uriToIri(uri: Uri): Iri = Iri.unsafe(uri.toString())

  /**
    * Syntactic sugar for constructing an [[Iri]] from the [[Uri]]
    */
  implicit final class UriSyntax(uri: Uri) {
    def toIri: Iri = uri
  }
}
