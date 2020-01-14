package ch.epfl.bluebrain.nexus.rdf

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Iri.Path.Segment
import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url}
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode}

sealed abstract class Graph extends Product with Serializable {

  def node: Node

  def triples: Set[Triple]

  def ::(prepend: (IriOrBNode, IriNode)): Graph = this match {
    case SetGraph(_, graphs) =>
      val (s, p) = prepend
      Graph(s, graphs.map(g => (s, p, g.node)) ++ triples)
    case OptionalGraph(None)        => this
    case OptionalGraph(Some(graph)) => prepend :: graph
    case _ =>
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

  private val dotNonEscapedStringRegex = {
    val alphanum = "a-zA-Z\u0080-\u00ff_"
    s"[${alphanum}][${alphanum}0-9]*".r
  }
  private val dotNumeralRegex = "[-]?(.[0-9]+|[0-9]+(.[0-9]*)?)".r

  /**
    * Returns DOT representation of [[Graph]].
    *
    * @param prefixMappings     prefix mappings to apply to IRIs.
    * @param sequenceBlankNodes whether to replace blank node IDs with sequential identifiers.
    * @param stripPrefixes      whether to strip prefixes from IRIs.
    * @return the DOT representation as [[String]].
    */
  def dot(
      prefixMappings: Map[AbsoluteIri, String] = Map.empty,
      sequenceBlankNodes: Boolean = true,
      stripPrefixes: Boolean = false
  ): String = {

    def escapeChar(c: Char): String = c match {
      case '"' => "\\\""
      case x   => x.toString
    }

    def escape(str: String): String =
      str.flatMap(escapeChar(_: Char))

    def applyOrStripPrefix(iri: AbsoluteIri): String =
      prefixMappings
        .get(iri)
        .orElse(
          prefixMappings
            .find {
              case (prefix, _) if iri.toString.startsWith(prefix.toString) => true
              case _                                                       => false
            }
            .map {
              case (prefix, mapping) => s"$mapping:${iri.toString.stripPrefix(prefix.toString)}"
            }
        )
        .getOrElse {
          if (stripPrefixes)
            iri match {
              case Url(_, _, _, _, Some(fragment))  => fragment.asString
              case Url(_, _, Segment(seg, _), _, _) => seg
              case _                                => iri.toString
            }
          else iri.toString

        }

    def escapeAndQuote(node: Node, bNodeIds: Map[String, String] = Map.empty) = {
      val id = node match {
        case IriNode(iri)                     => applyOrStripPrefix(iri)
        case BNode(bId) if sequenceBlankNodes => bNodeIds.get(bId).map(i => s"_:b$i").getOrElse(bId)
        case _                                => node.toString
      }
      if (dotNonEscapedStringRegex.matches(id) || dotNumeralRegex.matches(id))
        id
      else
        s""""${escape(id)}""""

    }

    def updateBNodeId(bNodeIds: Map[String, String], lastBNodeId: Int, bIds: String*): (Map[String, String], Int) =
      bIds.foldLeft((bNodeIds, lastBNodeId)) {
        case ((bNIds, lastId), bId) =>
          bNIds.get(bId) match {
            case Some(_) => (bNIds, lastId)
            case None    => (bNIds.updated(bId, (lastId + 1).toString), lastId + 1)
          }

      }

    def updateBNodeIds(triple: Triple, bNodeIds: Map[String, String], lastBNodeId: Int): (Map[String, String], Int) =
      (sequenceBlankNodes, triple) match {
        case (false, _)                         => (bNodeIds, lastBNodeId)
        case (_, (BNode(bId1), _, BNode(bId2))) => updateBNodeId(bNodeIds, lastBNodeId, bId1, bId2)
        case (_, (BNode(bId), _, _))            => updateBNodeId(bNodeIds, lastBNodeId, bId)
        case (_, (_, _, BNode(bId)))            => updateBNodeId(bNodeIds, lastBNodeId, bId)
        case _                                  => (bNodeIds, lastBNodeId)
      }

    triples
      .foldLeft((new StringBuilder(s"""digraph ${escapeAndQuote(node)} {\n"""), Map.empty[String, String], 0)) {
        case ((b, bNodeIds, lastBNodeId), (s, p, o)) =>
          val (updatedBNodeIds, updatedLastBNodeId) = updateBNodeIds((s, p, o), bNodeIds, lastBNodeId)
          b.append("  ")
            .append(escapeAndQuote(s, updatedBNodeIds))
            .append(" -> ")
            .append(escapeAndQuote(o, updatedBNodeIds))
            .append(" [label = ")
            .append(escapeAndQuote(p))
            .append("]\n")
          (b, updatedBNodeIds, updatedLastBNodeId)
      }
      ._1
      .append("}")
      .toString
  }

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
