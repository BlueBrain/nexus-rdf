package ch.epfl.bluebrain.nexus.rdf.syntax

import cats.Monad
import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.RootedGraph
import ch.epfl.bluebrain.nexus.rdf.decoder.GraphDecoder
import ch.epfl.bluebrain.nexus.rdf.encoder.{GraphEncoder, RootNode}
import io.circe.Json

trait GraphDecoderSyntax {

  implicit final def rootedGraphDecoderSyntax(graph: RootedGraph): RootedGraphOpsDecoder =
    new RootedGraphOpsDecoder(graph)

  implicit final def valueDecoderThroughGraph[A: RootNode](value: A): GraphOpsDecoderThoughGraph[A] =
    new GraphOpsDecoderThoughGraph(value)
}

final class RootedGraphOpsDecoder(private val graph: RootedGraph) extends AnyVal {

  def as[A]: DecoderApply[A] = new DecoderApply[A](graph)
}

final class GraphOpsDecoder(private val graph: RootedGraph) extends AnyVal {

  def as[A]: DecoderApply[A] = new DecoderApply[A](graph)
}

final class GraphOpsDecoderThoughGraph[A: RootNode](value: A) {

  def as[B]: DecoderThroughGraphApply[A, B] = new DecoderThroughGraphApply[A, B](value)

}

final class DecoderApply[A](graph: RootedGraph) {
  def apply[F[_]](context: Json = Json.obj())(
      implicit
      dec: GraphDecoder[F, A]
  ): F[A] =
    dec(graph, context)
}

final class DecoderThroughGraphApply[A: RootNode, B](value: A) {
  def apply[F[_]](context: Json = Json.obj())(
      implicit
      dec: GraphDecoder[F, B],
      enc: GraphEncoder[F, A],
      F: Monad[F]
  ): F[B] =
    enc(value).flatMap(rGraph => dec(rGraph, context))
}
