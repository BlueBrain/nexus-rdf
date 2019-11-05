package ch.epfl.bluebrain.nexus.rdf.cursor

import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary.rdf
import ch.epfl.bluebrain.nexus.rdf.cursor.CursorOp._
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursor.SCursor
import ch.epfl.bluebrain.nexus.rdf.instances._
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
    * If the focus is a Node set, return the set of [[GraphCursor]] resulting from it
    */
  def downSet: Set[GraphCursor]

  /**
    * If the focus is a Node list, return the set of [[GraphCursor]] resulting from it
    */
  def downList: List[GraphCursor]

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

    def downSet: Set[GraphCursor] = Set.empty

    def downList: List[GraphCursor] = List.empty

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

  private[cursor] sealed abstract class FieldCursor(lastCursor: SCursor, lastOp: CursorOp, g: Graph)
      extends SCursor(lastCursor, lastOp) {

    protected[this] def downField(obj: Node, p: IriNode => Boolean): GraphCursor = {
      val objects = g.select(obj, p)
      objects.toList match {
        case (_, _, o) :: Nil =>
          sortedListCursors(o) match {
            case Some(nodes) => new ListNodeCursorSel(obj, nodes, this)(this, DownField(p), g)
            case None        => new NodeCursor(obj, o, this)(this, DownField(p), g)
          }
        case Nil => fail(DownField(p))
        case _   => new SetNodeCursorSel(obj, objects.map { case (_, _, o) => o }, this)(this, DownField(p), g)
      }
    }

    protected[this] def field(subject: Node, p: IriNode => Boolean, parent: SCursor): GraphCursor = {
      val objects = g.select(subject, p)
      objects.toList match {
        case (_, _, o) :: Nil => new NodeCursor(subject, o, parent)(this, Field(p), g)
        case Nil              => fail(Field(p))
        case _ =>
          new SetNodeCursorSel(subject, objects.map { case (_, _, o) => o }, parent)(this, Field(p), g)
      }
    }

    private def sortedListCursors(current: Node): Option[List[Node]] =
      (g.select(current, rdf.first).toList, g.select(current, rdf.rest).toList) match {
        case (((_, _, o1) :: Nil), ((_, _, o2) :: Nil)) if o2.asIri.contains(rdf.nil) => Some(List(o1))
        case (((_, _, o1) :: Nil), ((_, _, o2) :: Nil))                               => sortedListCursors(o2).map(o1 :: _)
        case _                                                                        => None
      }
  }

  private[cursor] final class TopCursor(obj: Node)(lastCursor: SCursor, lastOp: CursorOp, graph: Graph)
      extends FieldCursor(lastCursor, lastOp, graph) {

    def focus: Option[Node]                           = Some(obj)
    def values: Option[Iterable[Node]]                = focus.map(List(_))
    def up: GraphCursor                               = fail(MoveUp)
    def field(p: IriNode => Boolean): GraphCursor     = fail(Field(p))
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new TopCursor(obj)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = downField(obj, p)
    override def top: GraphCursor                     = fail(MoveTop)
  }
  private[cursor] final class NodeCursor(subject: Node, obj: Node, parent: SCursor)(
      lastCursor: SCursor,
      lastOp: CursorOp,
      graph: Graph
  ) extends FieldCursor(lastCursor, lastOp, graph) {

    def field(p: IriNode => Boolean): GraphCursor     = field(subject, p, parent)
    def focus: Option[Node]                           = Some(obj)
    def values: Option[Iterable[Node]]                = focus.map(List(_))
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new NodeCursor(subject, obj, parent)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = downField(obj, p)
    def up: GraphCursor                               = parent.addOp(this, MoveUp)
    override def downSet: Set[GraphCursor]            = Set(this)

  }

  private[cursor] final class SetNodeCursorSel(subject: Node, obj: Set[Node], parent: SCursor)(
      lastCursor: SCursor,
      lastOp: CursorOp,
      graph: Graph
  ) extends FieldCursor(lastCursor, lastOp, graph) {

    def field(p: IriNode => Boolean): GraphCursor = field(subject, p, parent)

    override def downSet: Set[GraphCursor] =
      obj.map(new NodeCursorArrayElem(subject, _, this)(this, DownSet, graph))

    override def downAt(o: Node => Boolean): GraphCursor =
      obj.find(o) match {
        case None       => fail(DownAt(o))
        case Some(node) => new NodeCursorArrayElem(subject, node, this)(this, DownAt(o), graph)
      }

    def focus: Option[Node]                           = None
    def values: Option[Iterable[Node]]                = Some(obj)
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new SetNodeCursorSel(subject, obj, parent)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = fail(DownField(p))
    def up: GraphCursor                               = parent.addOp(this, MoveUp)

  }

  private[cursor] final class ListNodeCursorSel(subject: Node, obj: List[Node], parent: SCursor)(
      lastCursor: SCursor,
      lastOp: CursorOp,
      graph: Graph
  ) extends FieldCursor(lastCursor, lastOp, graph) {

    def field(p: IriNode => Boolean): GraphCursor = field(subject, p, parent)

    override def downList: List[GraphCursor] =
      obj.map(new NodeCursorArrayElem(subject, _, this)(this, DownList, graph))

    override def downSet: Set[GraphCursor] =
      downList.toSet

    override def downAt(o: Node => Boolean): GraphCursor =
      obj.find(o) match {
        case None       => fail(DownAt(o))
        case Some(node) => new NodeCursorArrayElem(subject, node, this)(this, DownAt(o), graph)
      }

    def focus: Option[Node]                           = None
    def values: Option[Iterable[Node]]                = Some(obj)
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new ListNodeCursorSel(subject, obj, parent)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = fail(DownField(p))
    def up: GraphCursor                               = parent.addOp(this, MoveUp)

  }

  private[cursor] final class NodeCursorArrayElem(subject: Node, obj: Node, parent: SCursor)(
      lastCursor: SCursor,
      lastOp: CursorOp,
      graph: Graph
  ) extends FieldCursor(lastCursor, lastOp, graph) {

    def focus: Option[Node]                           = Some(obj)
    def values: Option[Iterable[Node]]                = focus.map(List(_))
    def field(p: IriNode => Boolean): GraphCursor     = fail(Field(p))
    def addOp(cursor: SCursor, op: CursorOp): SCursor = new NodeCursorArrayElem(subject, obj, parent)(cursor, op, graph)
    def downField(p: IriNode => Boolean): GraphCursor = downField(obj, p)
    def up: GraphCursor                               = parent.addOp(this, MoveUp)
    override def downSet: Set[GraphCursor]            = Set(this)

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
    def downSet: Set[GraphCursor]                     = Set.empty
    def downList: List[GraphCursor]                   = List.empty
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
