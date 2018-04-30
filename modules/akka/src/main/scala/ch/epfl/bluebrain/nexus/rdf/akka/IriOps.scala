package ch.epfl.bluebrain.nexus.rdf.akka

import akka.http.scaladsl.model.Uri
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri

trait IriOps {

  /**
    *
    * Implicit conversion for constructing a [[Uri]] from the provided [[Iri]]
    */
  final implicit def iriToUri(iri: Iri): Uri = Uri(iri.show)

  /**
    * Syntactic sugar for constructing a [[Uri]] from the [[Iri]]
    */
  implicit final class IriSyntax(iri: Iri) {
    def toAkkaUri: Uri = iri
  }
}
