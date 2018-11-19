package ch.epfl.bluebrain.nexus.rdf.syntax

import _root_.akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.syntax.UriSyntax.UriOps

trait UriSyntax {

  final implicit def akkaSyntaxUri(uri: Uri): UriOps =
    new UriOps(uri)

}

object UriSyntax extends UriSyntax {

  /**
    * Syntactic sugar for constructing an [[Iri]] from the [[Uri]]
    */
  private[syntax] final class UriOps(private val uri: Uri) extends AnyVal {
    def toIri: Iri = Iri.unsafe(uri.toString())
  }
}
