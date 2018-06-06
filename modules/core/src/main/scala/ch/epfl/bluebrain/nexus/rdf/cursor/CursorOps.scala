package ch.epfl.bluebrain.nexus.rdf.cursor

import java.io.Serializable

import cats.Eq
import ch.epfl.bluebrain.nexus.rdf.Node
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode

sealed abstract class CursorOp extends Product with Serializable

final object CursorOp {
  final case object MoveUp                          extends CursorOp
  final case object MoveTop                         extends CursorOp
  final case class Field(p: IriNode => Boolean)     extends CursorOp
  final case class DownField(p: IriNode => Boolean) extends CursorOp
  final case class DownAt(o: Node => Boolean)       extends CursorOp

  implicit final val eqCursorOp: Eq[CursorOp] = Eq.fromUniversalEquals

  val eqCursorOpList: Eq[List[CursorOp]] = cats.instances.list.catsKernelStdEqForList[CursorOp]
}
