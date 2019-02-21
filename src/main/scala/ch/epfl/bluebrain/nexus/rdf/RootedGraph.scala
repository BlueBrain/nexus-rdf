package ch.epfl.bluebrain.nexus.rdf

import ch.epfl.bluebrain.nexus.rdf.Graph.Triple
import ch.epfl.bluebrain.nexus.rdf.Node.{blank, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.RootedGraph._
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursor
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.{Graph => G}

/**
  * An RDF Graph representation where one node has been distinguished as the root.
  *
  * @param rootNode the root node
  */
final class RootedGraph private[rdf] (val rootNode: IriOrBNode, override private[rdf] val underlying: G[Node, LkDiEdge])
    extends Graph(underlying) {

  /**
    * @return the initial cursor of the ''graph'', centered in the ''rootNode''
    */
  def cursor(): GraphCursor = cursor(rootNode)

  /**
    * Generates a new rooted graph if the rootNode and triple values are different from the existing ones.
    *
    * @param rootNode the root node
    * @param triples  the triples to be added to the graph
    */
  def copy(rootNode: IriOrBNode = rootNode, triples: Set[Triple] = triples): RootedGraph =
    if (rootNode == this.rootNode && triples == this.triples) this
    else if (rootNode != this.rootNode && triples == this.triples) new RootedGraph(rootNode, this.underlying)
    else apply(rootNode, triples)

}

object RootedGraph {

  /**
    * Constructs a new rooted graph where the root node is a blank node.
    *
    * @param triples the triples to be added to the graph
    * @return a new rooted graph from the argument triples
    */
  final def anon(triples: Triple*): RootedGraph =
    apply(blank, triples.toSet)

  /**
    * Constructs a new rooted graph from the argument triples.
    *
    * @param rootNode the root node
    * @param triples  the triples to be added to the graph
    * @return a new rooted graph from the argument triples
    */
  final def apply(rootNode: IriOrBNode, triples: Triple*): RootedGraph =
    apply(rootNode, triples.toSet)

  /**
    * Constructs a new rooted graph from an existing graph.
    *
    * @param rootNode the root node
    * @param graph  the existing graph
    */
  final def apply(rootNode: IriOrBNode, graph: Graph): RootedGraph =
    new RootedGraph(rootNode, graph.underlying)

  /**
    * Constructs a new rooted graph from the argument triples.
    *
    * @param rootNode the root node
    * @param triples  the triples to be added to the graph
    * @return a new rooted graph from the argument triples
    */
  final def apply(rootNode: IriOrBNode, triples: Set[Triple]): RootedGraph = {
    val edges = triples.foldLeft(Set.empty[LkDiEdge[Node]]) {
      case (es, (s, p, o)) => es + LkDiEdge(s, o)(p)
    }
    new RootedGraph(rootNode, G.from(Set.empty, edges))
  }
}
