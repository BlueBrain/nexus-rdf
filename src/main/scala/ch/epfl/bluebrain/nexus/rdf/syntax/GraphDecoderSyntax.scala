package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.decoder.GraphDecoder
import ch.epfl.bluebrain.nexus.rdf.decoder.GraphDecoder.GraphDecoderResult
import ch.epfl.bluebrain.nexus.rdf.encoder.{GraphEncoder, PrimaryNode}
import ch.epfl.bluebrain.nexus.rdf.{Graph, MarshallingError, RootedGraph}
import io.circe.Json

trait GraphDecoderSyntax {

  implicit final def graphDecoderSyntax(graph: Graph): GraphOpsDecoder =
    new GraphOpsDecoder(graph)

  implicit final def subjectGraphDecoderSyntax(graph: RootedGraph): SubjectGraphOpsDecoder =
    new SubjectGraphOpsDecoder(graph)

  implicit final def subjectGraphDecoderSyntax(value: (IriNode, Graph)): SubjectGraphOpsDecoder = {
    val (rootNode, graph) = value
    new SubjectGraphOpsDecoder(RootedGraph(rootNode, graph))
  }

  implicit final def valueDecoderThroughGraph[A: GraphEncoder](value: A): GraphOpsDecoderThoughGraph[A] =
    new GraphOpsDecoderThoughGraph(value)
}

final class SubjectGraphOpsDecoder(private val graph: RootedGraph) extends AnyVal {

  def as[A](context: Json)(implicit dec: GraphDecoder[A]): GraphDecoderResult[A] =
    dec(graph, context)

  def as[A: GraphDecoder]: GraphDecoderResult[A] =
    as(Json.obj())
}

final class GraphOpsDecoder(private val value: Graph) extends AnyVal {

  def as[A](id: IriOrBNode, context: Json)(implicit dec: GraphDecoder[A]): GraphDecoderResult[A] =
    dec(RootedGraph(id, value), context)

  def as[A: GraphDecoder](id: IriOrBNode): GraphDecoderResult[A] =
    as(id, Json.obj())
}

final class GraphOpsDecoderThoughGraph[A](value: A)(implicit enc: GraphEncoder[A]) {

  def as[B](context: Json)(implicit dec: GraphDecoder[B], P: PrimaryNode[A]): Either[MarshallingError, B] =
    enc(value).flatMap(subjectGraph => dec(subjectGraph, context))

  def as[B: GraphDecoder](implicit P: PrimaryNode[A]): Either[MarshallingError, B] =
    as(Json.obj())

  def as[B](id: IriOrBNode, context: Json)(implicit dec: GraphDecoder[B]): Either[MarshallingError, B] =
    enc(id, value).flatMap(subjectGraph => dec(subjectGraph, context))

  def as[B](id: IriOrBNode)(implicit dec: GraphDecoder[B]): Either[MarshallingError, B] =
    as(id, Json.obj())

}
