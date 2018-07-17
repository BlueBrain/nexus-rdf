package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursor
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import io.circe.Json

import scala.util.Try

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
      * @return The optionally available iri node root ''subject'' of the Graph. This is, the subject which is not used as an object
      */
    def primaryIriNode: Option[IriNode] =
      primaryNode.flatMap(_.asIri)

    /**
      * @return the list of objects which have the subject found from the method ''id'' and the predicate rdf:type
      */
    def primaryTypes: Set[IriNode] =
      primaryNode.map(i => graph.types(i)).getOrElse(Set.empty)

    /**
      * @param  id the id for which the types should be found
      * @return the list of objects which have the subject ''id'' and the predicate rdf:type
      */
    def types(id: IriOrBNode): Set[IriNode] =
      graph.objects(id, rdfType).collect { case n: IriNode => n }

    /**
      * @return the initial cursor of the ''graph'', centered in the ''primaryNode''
      */
    def cursor(): GraphCursor = primaryNode match {
      case None       => GraphCursor.failed
      case Some(node) => graph.cursor(node)
    }

    /**
      * Convert [[Graph]] into JSON-LD representation using provided context. Beware, that currently IRI contexts are
      * not resolved and will be ignored.
      *
      * @param context context to use when creating JSON-LD representation
      * @return [[Json]] containing JSON-LD representation of the [[Graph]]
      */
    def asJson(context: Json): Try[Json] =
      graph.asJson(context, primaryNode)

  }
}
