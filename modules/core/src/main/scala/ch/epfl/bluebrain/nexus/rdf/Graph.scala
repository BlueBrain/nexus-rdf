package ch.epfl.bluebrain.nexus.rdf

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode}

sealed abstract class Graph extends Product with Serializable {

  def node: Node

  def triples: Set[Triple]

  def ::(prepend: (IriOrBNode, IriNode)): Graph = {
    val (s, p) = prepend
    val triple = (s, p, node)
    Graph(s, triples + triple)
  }

  def prepend(g: Graph, predicate: IriNode): Graph = this match {
    case SetGraph(_, graphs) =>
      g.node match {
        case s: IriOrBNode =>
          Graph(s, g.triples ++ triples ++ graphs.map(e => (s, predicate, e.node)))
        case _ =>
          Graph(g.node, g.triples ++ triples)
      }
    case OptionalGraph(None) => g
    case OptionalGraph(Some(graph)) =>
      graph.prepend(g, predicate)
    case _ =>
      g.node match {
        case s: IriOrBNode =>
          val link = (s, predicate, node)
          Graph(g.node, g.triples ++ triples + link)
        case _ =>
          Graph(g.node, g.triples ++ triples)
      }
  }

  def append(g: Graph, predicate: IriNode): Graph =
    g.prepend(this, predicate)

  def append(p: IriNode, o: Node): Graph =
    node match {
      case ibn: IriOrBNode => this + ((ibn, p, o))
      case _               => this
    }

  def +(triple: Triple): Graph =
    Graph(node, triples + triple)

  def ++(triples: Set[Triple]): Graph =
    Graph(node, this.triples ++ triples)

  def ++(that: Graph): Graph =
    ++(that.triples)

  def -(triple: Triple): Graph =
    Graph(node, triples - triple)

  def --(triples: Set[Triple]): Graph =
    Graph(node, this.triples -- triples)

  def --(that: Graph): Graph =
    --(that.triples)

  def subjects: Set[IriOrBNode] =
    triples.map(_._1)

  def predicates: Set[IriNode] =
    triples.map(_._2)

  def objects: Set[Node] =
    triples.map(_._3)

  def filter(p: Triple => Boolean): Graph =
    Graph(node, triples.filter(p))

  def withNode(node: Node): Graph =
    Graph(node, triples)

  def replaceNode(target: IriOrBNode, value: IriOrBNode): Graph = this match {
    case _: SingleNodeGraph if target == node => SingleNodeGraph(value)
    case _: SingleNodeGraph                   => this
    case _ =>
      val newNode = if (node == target) value else node
      MultiNodeGraph(newNode, triples.map {
        case (`target`, p, `target`) => (value, p, value)
        case (`target`, p, o)        => (value, p, o)
        case (s, p, `target`)        => (s, p, value)
        case default                 => default
      })
  }

  def foldLeft[Z](z: Z)(f: (Z, (IriOrBNode, IriNode, Node)) => Z): Z =
    triples.foldLeft(z)(f)

  def selectAs[A](
      sp: IriOrBNode => Boolean,
      pp: IriNode => Boolean,
      op: Node => Boolean,
      f: Triple => A
  ): Set[A] =
    foldLeft(Set.empty[A]) {
      case (acc, (s, p, o)) if sp(s) && pp(p) && op(o) => acc + f((s, p, o))
      case (acc, _)                                    => acc
    }

  def select(
      s: IriOrBNode => Boolean = _ => true,
      p: IriNode => Boolean = _ => true,
      o: Node => Boolean = _ => true
  ): Set[Triple] =
    selectAs(s, p, o, identity)

  def select(s: IriOrBNode, p: IriNode): Set[Node] =
    spToO.getOrElse((s, p), Set.empty)

  def selectReverse(o: Node, p: IriNode): Set[IriOrBNode] =
    opToS.getOrElse((o, p), Set.empty)

  def cursor: Cursor =
    Cursor(this)

  def ntriples: String =
    triples
      .foldLeft(new StringBuilder) {
        case (b, (s, p, o)) =>
          b.append(s.show).append(" ").append(p.show).append(" ").append(o.show).append(" .\n")
      }
      .toString

  private[rdf] lazy val spToO: Map[(IriOrBNode, IriNode), Set[Node]] =
    triples.groupMap({ case (s, p, _) => (s, p) })({ case (_, _, o) => o })

  private[rdf] lazy val opToS: Map[(Node, IriNode), Set[IriOrBNode]] =
    triples.groupMap({ case (_, p, o) => (o, p) })({ case (s, _, _) => s })
}

object Graph {

  final def apply(node: Node, triples: Set[Triple] = Set.empty): Graph =
    if (triples.isEmpty) SingleNodeGraph(node)
    else MultiNodeGraph(node, triples)

  private[rdf] final case class SingleNodeGraph(node: Node) extends Graph {
    override val triples: Set[(IriOrBNode, IriNode, Node)] = Set.empty
  }

  private[rdf] final case class SetGraph(node: Node, graphs: Set[Graph]) extends Graph {
    override lazy val triples: Set[Triple] =
      graphs.foldLeft(Set.empty[Triple])(_ ++ _.triples)
  }

  private[rdf] final case class OptionalGraph(graph: Option[Graph]) extends Graph {
    override lazy val node: Node = graph.map(_.node).getOrElse(BNode())
    override lazy val triples: Set[Triple] =
      graph.map(_.triples).getOrElse(Set.empty)
  }

  private[rdf] final case class MultiNodeGraph(node: Node, triples: Set[Triple]) extends Graph

  type Triple = (IriOrBNode, IriNode, Node)

}
