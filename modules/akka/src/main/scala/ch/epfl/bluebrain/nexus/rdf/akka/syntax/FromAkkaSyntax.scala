package ch.epfl.bluebrain.nexus.rdf.akka.syntax

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode

trait FromAkkaSyntax {

  import ch.epfl.bluebrain.nexus.rdf.akka.{AkkaConverters => conv}

  implicit class AkkaUriAsRdf(uri: Uri) {

    /**
      * Converts a [[Uri]] to RDF [[IriNode]].
      */
    def asRdfNode: IriNode = conv.asRdfIriNode(uri)

    /**
      * Converts a [[Uri]] to [[AbsoluteIri]].
      */
    def asAbsoluteIri: AbsoluteIri = conv.asAbsoluteIri(uri)
  }

}
