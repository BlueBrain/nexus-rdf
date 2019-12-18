package ch.epfl.bluebrain.nexus.rdf.akka

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode

/**
  * Conversions from Akka [[akka.http.scaladsl.model.Uri]] and  rdf data types .
  */
trait FromAkkaConverters {

  /**
    * Converts Akka [[Uri]] to [[AbsoluteIri]]
    */
  def asAbsoluteIri(uri: Uri): AbsoluteIri = AbsoluteIri.unsafe(uri.toString)

  /**
    * Converts Akka [[Uri]] to [[IriNode]]
    */
  def asRdfIriNode(uri: Uri): IriNode = IriNode(AbsoluteIri.unsafe(uri.toString))

}
