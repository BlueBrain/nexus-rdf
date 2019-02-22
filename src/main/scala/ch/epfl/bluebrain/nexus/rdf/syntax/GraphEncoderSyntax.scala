package ch.epfl.bluebrain.nexus.rdf.syntax

import cats.Monad
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode
import ch.epfl.bluebrain.nexus.rdf.encoder.{GraphEncoder, RootNode}
import ch.epfl.bluebrain.nexus.rdf.{Graph, RootedGraph}

trait GraphEncoderSyntax {

  implicit final def graphEncoderSyntax[A](value: A): GraphOpsEncoding[A] = new GraphOpsEncoding(value)
}

final class GraphOpsEncoding[A](private val value: A) extends AnyVal {

  def asGraph[F[_]](rootNode: IriOrBNode)(implicit enc: GraphEncoder[F, A]): F[RootedGraph] =
    enc(rootNode, value)

  def asGraph[F[_]](id: AbsoluteIri)(implicit enc: GraphEncoder[F, A]): F[RootedGraph] =
    enc(id, value)

  def asGraph[F[_]](implicit enc: GraphEncoder[F, A], P: RootNode[A]): F[RootedGraph] =
    enc(value)

  def asGraph[F[_]: Monad](fRootNode: Graph => F[IriOrBNode])(implicit enc: GraphEncoder[F, A]): F[RootedGraph] =
    enc(fRootNode, value)
}
