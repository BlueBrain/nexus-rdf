package ch.epfl.bluebrain.nexus.rdf

import cats.{Eq, Show}
import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import scalax.collection.edge.LkDiEdge
import scalax.collection.immutable.{Graph => G}

/**
  * An RDF Graph representation.
  */
@SuppressWarnings(Array("IsInstanceOf"))
final class Graph private[rdf] (private val underlying: G[Node, LkDiEdge]) {

  /**
    * @return the triples of this graph
    */
  def triples: Set[Triple] =
    foldLeft(Set.empty[Triple])(_ + _)

  /**
    * @return true if the graph has at least one cycle, false otherwise
    */
  def isCyclic: Boolean =
    underlying.isCyclic

  /**
    * @return true if the graph has no cycles, false otherwise
    */
  def isAcyclic: Boolean =
    underlying.isAcyclic

  /**
    * @return true if all the triples are connected through the nodes, false otherwise
    */
  def isConnected: Boolean =
    underlying.isConnected

  /**
    * @param s the triple subject used to test matches
    * @param p the triple predicate used to test matches
    * @param o the triple objects used to test matches
    * @return the triples found from the provided subject, predicate and object functions
    */
  def select(s: IriOrBNode => Boolean = _ => true,
             p: IriNode => Boolean = _ => true,
             o: Node => Boolean = _ => true): Set[(IriOrBNode, IriNode, Node)] =
    underlying.edges.filter(e => s(e.s) && p(e.p) && o(e.o)).map(e => (e.s, e.p, e.o)).toSet

  /**
    * @param p the triple predicate
    * @param o the triple object
    * @return the subjects found from the provided predicate and object
    */
  def subjects(p: IriNode => Boolean = _ => true, o: Node => Boolean = _ => true): Set[IriOrBNode] =
    select(_ => true, p, o).map { case (s, _, _) => s }

  /**
    * @param s the triple subject
    * @param o the triple object
    * @return the predicates found from the provided subject and object
    */
  def predicates(s: IriOrBNode => Boolean = _ => true, o: Node => Boolean = _ => true): Set[IriNode] =
    select(s, _ => true, o).map { case (_, p, _) => p }

  /**
    * @param s the triple subject used to test matches
    * @param p the triple predicate used to test matches
    * @return the objects found from the provided subject and predicate
    */
  def objects(s: IriOrBNode => Boolean = _ => true, p: IriNode => Boolean = _ => true): Set[Node] =
    select(s, p, _ => true).map { case (_, _, o) => o }

  /**
    * Adds the triple identified by (s, p, o) arguments to this graph.
    *
    * @param s the triple subject
    * @param p the triple predicate
    * @param o the triple object
    * @return a new graph made up of all of the triples of this graph and the triple created from the arguments
    */
  def add(s: IriOrBNode, p: IriNode, o: Node): Graph =
    new Graph(underlying + LkDiEdge(s, o)(p))

  /**
    * Adds the argument triple to this graph.
    *
    * @param spo the triple to add
    * @return a new graph made up of all of the triples of this graph and the argument triple
    */
  def +(spo: (IriOrBNode, IriNode, Node)): Graph = {
    val (s, p, o) = spo
    add(s, p, o)
  }

  /**
    * Removes the triple identified by (s, p, o) arguments if it's contained.
    *
    * @param s the triple subject
    * @param p the triple predicate
    * @param o the triple object
    * @return a new graph made up of all of the triples of this graph except the triple created from the arguments
    */
  def remove(s: IriOrBNode, p: IriNode, o: Node): Graph = {
    val edge = LkDiEdge(s, o)(p)
    new Graph(underlying -! edge)
  }

  /**
    * Removes the argument triple from this graph if it's contained.
    *
    * @param spo the triple to remove
    * @return a new graph made up of all of the triples of this graph except the argument triple
    */
  def -(spo: (IriOrBNode, IriNode, Node)): Graph = {
    val (s, p, o) = spo
    remove(s, p, o)
  }

  /**
    * Joins this graph with that graph.
    *
    * @param that the graph to join
    * @return the union of this and that graphs
    */
  def union(that: Graph): Graph =
    new Graph(underlying ++ that.underlying)

  /**
    * Joins this graph with that graph.
    *
    * @param that the graph to join
    * @return the union of this and that graphs
    */
  def ++(that: Graph): Graph =
    union(that)

  /**
    * Removes all the triples of that graph that are contained in this graph.
    *
    * @param that the graph containing the triples to remove
    * @return a new graph that contains only the triples in this graph that are not contained in that graph
    */
  def subtract(that: Graph): Graph =
    new Graph(underlying --! that.underlying.edges)

  /**
    * Removes all the triples of that graph that are contained in this graph.
    *
    * @param that the graph containing the triples to remove
    * @return a new graph that contains only the triples in this graph that are not contained in that graph
    */
  def --(that: Graph): Graph =
    subtract(that)

  private def foldLeft[Z](z: Z)(f: (Z, (IriOrBNode, IriNode, Node)) => Z): Z =
    underlying.edges.foldLeft(z) {
      case (acc, e) => f(acc, (e.s, e.p, e.o))
    }

  private implicit class EdgeOps(e: underlying.EdgeT) {
    def s: IriOrBNode = e.from.toOuter.asInstanceOf[IriOrBNode]
    def p: IriNode    = e.label.asInstanceOf[IriNode]
    def o: Node       = e.to
  }

  override def toString: String = underlying.toString()
  override def hashCode(): Int  = underlying.hashCode()
  override def equals(obj: Any): Boolean =
    obj != null && obj.isInstanceOf[Graph] && obj.asInstanceOf[Graph].underlying == underlying
}

object Graph {
  type Triple = (IriOrBNode, IriNode, Node)

  /**
    * Constructs a new graph from the argument triples.
    *
    * @param triples the triples to be added to the graph
    * @return a new graph from the argument triples
    */
  final def apply(triples: Triple*): Graph =
    apply(triples.toSet)

  /**
    * Constructs a new graph from the argument triples.
    *
    * @param triples the triples to be added to the graph
    * @return a new graph from the argument triples
    */
  final def apply(triples: Set[Triple]): Graph = {
    val edges = triples.foldLeft(Set.empty[LkDiEdge[Node]]) {
      case (es, (s, p, o)) => es + LkDiEdge(s, o)(p)
    }
    new Graph(G.from(Set.empty, edges))
  }

  final implicit def graphShow(implicit N: Show[Node]): Show[Graph] =
    Show.show(
      _.triples
        .map {
          case (s, p, o) => s"(${N.show(s)} ${N.show(p)} ${N.show(o)})"
        }
        .mkString("\n"))

  final implicit val graphEq: Eq[Graph] = Eq.fromUniversalEquals

  final implicit def sToEq(s: IriOrBNode): IriOrBNode => Boolean = _ == s
  final implicit def oToEq(o: Node): Node => Boolean             = _ == o
  final implicit def pToEq(p: IriNode): IriNode => Boolean       = _ == p
}
