package ch.epfl.bluebrain.nexus.rdf.cursor

import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.cursor.CursorOp._
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursor.SCursor
import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}

import scala.annotation.tailrec

@SuppressWarnings(Array("NullParameter"))
sealed abstract class GraphCursor(private val lastCursor: SCursor, private val lastOp: CursorOp) extends Serializable {

  /**
    * The current node in the graph
    */
  def focus: Option[Node]

  /**
    * The operations that have been performed so far from the first to the more recent.
    */
  def history: List[CursorOp] = {
    @tailrec
    def inner(cursor: GraphCursor, acc: List[CursorOp]): List[CursorOp] =
      if (cursor.lastCursor == null) acc
      else inner(cursor.lastCursor, cursor.lastOp :: acc)

    inner(this, List.empty)
  }

  /**
    * Indicate whether this cursor represents the result of a successful
    * operation.
    */
  def succeeded: Boolean

  /**
    * Indicate whether this cursor represents the result of an unsuccessful
    * operation.
    */
  final def failed: Boolean = !succeeded

  /**
    * If the focus is a Node array, return its elements.
    */
  def values: Option[Iterable[Node]]

  /**
    * The top cursor
    */
  def top: GraphCursor

  /**
    * Move the focus to the parent.
    */
  def up: GraphCursor

  /**
    * If the focus is a Node array, move to the node that satisfies the given function.
    */
  def downAt(o: Node => Boolean): GraphCursor

  /**
    * If the focus is a Node, move to a sibling that satisfies the given predicate.
    */
  def field(p: IriNode => Boolean): GraphCursor

  /**
    * If the focus is a Node, move to the value that satisfies the given predicate.
    */
  def downField(p: IriNode => Boolean): GraphCursor

}

@SuppressWarnings(Array("NullParameter"))
object GraphCursor {

  private[cursor] sealed abstract class SCursor(val lastCursor: SCursor, lastOp: CursorOp)
      extends GraphCursor(lastCursor, lastOp) {

    protected[this] final def fail(op: CursorOp): GraphCursor = new FailedCursor(this, op)

    def addOp(cursor: SCursor, op: CursorOp): SCursor

    def succeeded: Boolean = true

    def downAt(o: Node => Boolean): GraphCursor = fail(DownAt(o))

    private def fetchTop: GraphCursor = {
      @tailrec
      def inner(cursor: GraphCursor): GraphCursor =
        if (cursor == null)
          fail(MoveTop)
        else
          cursor match {
            case c: TopCursor => c
            case _            => inner(cursor.lastCursor)
          }

      inner(this)
    }

    def top: GraphCursor = fetchTop match {
      case t: TopCursor => t.addOp(this, MoveTop)
      case o            => o
    }
  }

  private[cursor] sealed abstract class DownFieldCursor(lastCursor: SCursor, lastOp: CursorOp, g: Graph)
      extends SCursor(lastCursor, lastOp) {

    protected[this] def downField(obj: Node, p: IriNode => Boolean): GraphCursor = {
      val objects = g.select(obj, p)
      objects.toList match {
        case (_, _, o) :: Nil => new NodeCursor(obj, o, this)(this, DownField(p), g)
        case Nil              => fail(DownField(p))
        case _ =>
          new ArrayNodeCursorSel(obj, objects.map { case (_, _, o) => o }, this)(this, DownField(p), g)
      }
    }
  }

  private[cursor] final class TopCursor(obj: Node)(lastCursor: SCursor, lastOp: CursorOp, graph: Graph)
      extends DownFieldCursor(lastCursor, lastOp, graph) {

    def focus: Option[Node]                           = Some(obj)
    def values: Option[Iterable[Node]]                = None
    def up: GraphCursor                               = fail(MoveUp)
    def field(p: IriNode => Boolean): GraphCursor     = fail(Field(p))
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new TopCursor(obj)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = downField(obj, p)
    override def top: GraphCursor                     = fail(MoveTop)
  }
  private[cursor] final class NodeCursor(subject: Node, obj: Node, parent: SCursor)(lastCursor: SCursor,
                                                                                    lastOp: CursorOp,
                                                                                    graph: Graph)
      extends DownFieldCursor(lastCursor, lastOp, graph) {

    def field(p: IriNode => Boolean): GraphCursor = {
      val objects = graph.select(subject, p)
      objects.toList match {
        case (_, _, o) :: Nil => new NodeCursor(obj, o, parent)(this, Field(p), graph)
        case Nil              => fail(Field(p))
        case _ =>
          new ArrayNodeCursorSel(obj, objects.map { case (_, _, o) => o }, parent)(this, Field(p), graph)
      }
    }

    def focus: Option[Node]                           = Some(obj)
    def values: Option[Iterable[Node]]                = None
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new NodeCursor(subject, obj, parent)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = downField(obj, p)
    def up: GraphCursor                               = parent.addOp(this, MoveUp)

  }

  private[cursor] final class ArrayNodeCursorSel(subject: Node, obj: Set[Node], parent: SCursor)(lastCursor: SCursor,
                                                                                                 lastOp: CursorOp,
                                                                                                 graph: Graph)
      extends SCursor(lastCursor, lastOp) {

    override def downAt(o: Node => Boolean): GraphCursor =
      obj.find(o) match {
        case None       => fail(DownAt(o))
        case Some(node) => new ArrayNodeCursor(subject, node, this)(this, DownAt(o), graph)
      }

    def focus: Option[Node]                           = None
    def values: Option[Iterable[Node]]                = Some(obj)
    def field(p: IriNode => Boolean): GraphCursor     = fail(Field(p))
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new ArrayNodeCursorSel(subject, obj, parent)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = fail(DownField(p))
    def up: GraphCursor                               = parent.addOp(this, MoveUp)

  }

  private[cursor] final class ArrayNodeCursor(subject: Node, obj: Node, parent: SCursor)(lastCursor: SCursor,
                                                                                         lastOp: CursorOp,
                                                                                         graph: Graph)
      extends DownFieldCursor(lastCursor, lastOp, graph) {

    def focus: Option[Node]                           = Some(obj)
    def values: Option[Iterable[Node]]                = None
    def field(p: IriNode => Boolean): GraphCursor     = fail(Field(p))
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new ArrayNodeCursor(subject, obj, parent)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = downField(obj, p)
    def up: GraphCursor                               = parent.addOp(this, MoveUp)

  }

  private[cursor] final class FailedCursor(lastCursor: SCursor, lastOp: CursorOp)
      extends GraphCursor(lastCursor, lastOp) {
    def focus: Option[Node]                           = None
    def succeeded: Boolean                            = false
    def values: Option[Iterable[Node]]                = None
    def top: GraphCursor                              = this
    def up: GraphCursor                               = this
    def downAt(o: Node => Boolean): GraphCursor       = this
    def field(p: IriNode => Boolean): GraphCursor     = this
    def downField(p: IriNode => Boolean): GraphCursor = this
  }

  /**
    * Construct an initial cursor from the provided ''node''
    *
    * @param node the node where to start the cursor traversal
    * @param g    the [[Graph]] to traverse
    */
  final def apply(node: IriOrBNode, g: Graph): GraphCursor = new TopCursor(node)(null, null, g)

  /**
    * @return an initial failed cursor
    */
  final def failed: GraphCursor = new FailedCursor(null, null)

}
