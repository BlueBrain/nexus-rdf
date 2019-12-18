package ch.epfl.bluebrain.nexus.rdf.akka.syntax

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node

trait ToAkkaSyntax {

  import ch.epfl.bluebrain.nexus.rdf.akka.{AkkaConverters => conv}

  implicit class NodeAsAkka(node: Node) {

    /**
      *  Converts a [[Node]] to Akka [[Uri]] if possible.
      */
    def asAkka: Either[String, Uri] = conv.asAkka(node)
  }

  implicit class AbsoluteIriAsAkka(iri: AbsoluteIri) {

    /**
      *  Converts an [[AbsoluteIri]] to Akka [[Uri]] if possible.
      */
    def asAkka: Either[String, Uri] = conv.asAkka(iri)
  }

}
