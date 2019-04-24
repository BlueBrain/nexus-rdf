package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode
import ch.epfl.bluebrain.nexus.rdf.RootedGraph
import ch.epfl.bluebrain.nexus.rdf.encoder.{GraphEncoder, RootNode}

trait GraphEncoderSyntax {

  implicit final def graphEncoderSyntax[A](value: A): GraphOpsEncoding[A] = new GraphOpsEncoding(value)
}

final class GraphOpsEncoding[A](private val value: A) extends AnyVal {

  def asGraph[F[_]](rootNode: IriOrBNode)(implicit enc: GraphEncoder[F, A]): F[RootedGraph] =
    enc(rootNode, value)

  def asGraph[F[_]](implicit enc: GraphEncoder[F, A], P: RootNode[A]): F[RootedGraph] =
    enc(value)
}
