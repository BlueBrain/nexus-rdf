package ch.epfl.bluebrain.nexus.rdf.cursor

import java.io.Serializable

import cats.Eq
import ch.epfl.bluebrain.nexus.rdf.Node
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode

sealed abstract class CursorOp extends Product with Serializable {

  /**
    * Does this operation require the current focus (not context) to be an
    * object?
    */
  def requiresObject: Boolean

  /**
    * Does this operation require the current focus (not context) to be an array?
    */
  def requiresArray: Boolean
}

final object CursorOp {
  abstract sealed class ObjectOp extends CursorOp {
    final def requiresObject: Boolean = true
    final def requiresArray: Boolean  = false
  }

  abstract sealed class ArrayOp extends CursorOp {
    final def requiresObject: Boolean = false
    final def requiresArray: Boolean  = true
  }

  abstract sealed class UnconstrainedOp extends CursorOp {
    final def requiresObject: Boolean = false
    final def requiresArray: Boolean  = false
  }

  final case object MoveUp                          extends UnconstrainedOp
  final case object MoveTop                         extends UnconstrainedOp
  final case class Field(p: IriNode => Boolean)     extends UnconstrainedOp
  final case class DownField(p: IriNode => Boolean) extends ObjectOp
  final case class DownAt(o: Node => Boolean)       extends ArrayOp

  implicit final val eqCursorOp: Eq[CursorOp] = Eq.fromUniversalEquals

  val eqCursorOpList: Eq[List[CursorOp]] = cats.instances.list.catsKernelStdEqForList[CursorOp]
}
