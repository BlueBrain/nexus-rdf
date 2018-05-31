package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}

object nexus {

  private final val rdfType = url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  final implicit class NexusGraphOps(graph: Graph) {

    /**
      * @return The optionally available root ''subject'' of the Graph. This is, the subject which is not used as an object
      */
    def id: Option[IriOrBNode] =
      (graph.subjects -- graph.objects.collect {
        case iri: IriOrBNode => iri
      }).toList match {
        case head :: Nil => Some(head)
        case _           => idWithoutBlankNodes
      }

    private def idWithoutBlankNodes: Option[IriOrBNode] =
      (graph.subjects -- graph.objects(_.isIri).collect {
        case iri: IriOrBNode => iri
      }).toList match {
        case head :: Nil => Some(head)
        case _           => None
      }

    /**
      * @return the list of objects which have the subject found from the method ''id'' and the predicate rdf:type
      */
    def types: Set[Node] = id.map(i => graph.objects(i, rdfType)).getOrElse(Set.empty)

  }
}
