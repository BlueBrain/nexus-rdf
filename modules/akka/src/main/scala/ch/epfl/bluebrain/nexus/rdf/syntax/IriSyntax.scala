package ch.epfl.bluebrain.nexus.rdf.syntax

import _root_.akka.http.scaladsl.model.Uri
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.syntax.IriSyntax.IriOps

trait IriSyntax {

  final implicit def akkaSyntaxIri(iri: Iri): IriOps =
    new IriOps(iri)
}

object IriSyntax extends IriSyntax {

  /**
    * Syntactic sugar for constructing a [[Uri]] from the [[Iri]]
    */
  private[syntax] final class IriOps(private val iri: Iri) extends AnyVal {
    def toAkkaUri: Uri = Uri(iri.show)

  }
}
