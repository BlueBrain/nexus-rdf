package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.syntax.GraphDotSyntax._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}
import scalax.collection.edge.LkDiEdge
import scalax.collection.io.dot.Indent._
import scalax.collection.io.dot._
import scalax.collection.{Graph => G}

trait GraphDotSyntax {

  implicit final def graphDotSyntax(graph: Graph): GraphDotOps = new GraphDotOps(graph)
}

private[syntax] object GraphDotSyntax {
  implicit class EdgeGTOps(e: LkDiEdge[G[Node, LkDiEdge]#NodeT]) {
    def s: IriOrBNode = e.from.toOuter.asInstanceOf[IriOrBNode]
    def p: IriNode    = e.label.asInstanceOf[IriNode]
    def o: Node       = e.to.value
  }

  val root = DotRootGraph(directed = true, id = None)

  val spacing = Spacing(indent = TwoSpaces, graphAttrSeparator = new AttrSeparator("\n|".stripMargin) {})

}

final class GraphDotOps(private val graph: Graph) extends AnyVal {

  /**
    * Converts the graph to DOT language
    */
  def asDot: String = {
    def transformer(innerEdge: G[Node, LkDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
      val edge: LkDiEdge[G[Node, LkDiEdge]#NodeT] = innerEdge.edge
      val (from, label, to)                       = (edge.s.toString, edge.p.toString, edge.to.toString)
      Some(root -> DotEdgeStmt(NodeId(from), NodeId(to), List(DotAttr(Id("label"), Id(label)))))
    }
    graph.underlying.toDot(dotRoot = root, edgeTransformer = transformer, cNodeTransformer = None, spacing = spacing)

  }
}
