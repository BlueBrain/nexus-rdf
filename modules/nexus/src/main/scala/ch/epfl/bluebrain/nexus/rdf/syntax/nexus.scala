package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._

object nexus {

  private final val rdfType = url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  final implicit class NexusGraphOps(graph: Graph) {

    /**
      * @return The optionally available root ''subject'' of the Graph. This is, the subject which is not used as an object
      */
    def primaryNode: Option[IriOrBNode] =
      (graph.subjects() -- graph.objects().collect { case iri: IriOrBNode => iri }).toList match {
        case head :: Nil => Some(head)
        case _           => None
      }

    /**
      * @return The optionally available blank node root ''subject'' of the Graph. This is, the subject which is not used as an object
      */
    def primaryBNode: Option[BNode] =
      primaryNode.flatMap(_.asBlank)

    /**
      * @return the list of objects which have the subject found from the method ''id'' and the predicate rdf:type
      */
    def primaryTypes: Set[IriNode] =
      primaryNode.map(i => graph.objects(i, rdfType).collect { case n: IriNode => n }).getOrElse(Set.empty)

  }
}
