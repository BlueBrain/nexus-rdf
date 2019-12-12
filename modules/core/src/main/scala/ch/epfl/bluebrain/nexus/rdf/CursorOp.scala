package ch.epfl.bluebrain.nexus.rdf

import java.io.Serializable

import ch.epfl.bluebrain.nexus.rdf.Node.IriNode

/**
  * Enumeration type for cursor operations
  */
sealed abstract class CursorOp extends Product with Serializable

object CursorOp {

  /**
    * Moves the cursor to the parent node
    */
  final case object MoveUp extends CursorOp

  /**
    * Moves the cursor to the top node (the initial cursor)
    */
  final case object MoveTop extends CursorOp

  /**
    * If the cursor is a [[ch.epfl.bluebrain.nexus.rdf.Cursor.NodeCursor]],
    * moves the cursor to a sibling node that matches the provided ''p''
    *
    * @param p the triple predicate function used to move the cursor
    */
  final case class Field(p: IriNode => Boolean) extends CursorOp

  /**
    * Moves the cursor to the next node which matches the provided ''p''
    *
    * @param p the triple predicate function used to move the cursor
    */
  final case class DownField(p: IriNode => Boolean) extends CursorOp

  /**
    * If the current cursor is an [[ch.epfl.bluebrain.nexus.rdf.Cursor.SetNodeCursorSel]],
    * selects the first array element matching ''o''
    *
    * @param o the triple object function used to move the cursor
    */
  final case class DownAt(o: Node => Boolean) extends CursorOp

  /**
    * If the current cursor is an [[ch.epfl.bluebrain.nexus.rdf.Cursor.SetNodeCursorSel]],
    * attempts to provide a Set[GraphCursor]
    */
  final case object DownSet extends CursorOp

  /**
    * If the current cursor is an [[ch.epfl.bluebrain.nexus.rdf.Cursor.ListNodeCursorSel]],
    * attempts to provide a List[GraphCursor]
    */
  final case object DownList extends CursorOp
}
