package ch.epfl.bluebrain.nexus.rdf.akka

import akka.http.scaladsl.model.Uri
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.akka.IriSyntax.IriOps

trait IriSyntax {

  /**
    *
    * Implicit conversion for constructing a [[Uri]] from the provided [[Iri]]
    */
  final implicit def iriToUri(iri: Iri): Uri =
    Uri(iri.show)

  final implicit def akkaSyntaxIri(iri: Iri): IriOps =
    new IriOps(iri)
}

object IriSyntax extends IriSyntax {

  /**
    * Syntactic sugar for constructing a [[Uri]] from the [[Iri]]
    */
  private[akka] final class IriOps(private val iri: Iri) extends AnyVal {
    def toAkkaUri: Uri = iri

  }
}
