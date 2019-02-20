package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.decoder.GraphDecoder
import ch.epfl.bluebrain.nexus.rdf.decoder.GraphDecoder.GraphDecoderResult
import ch.epfl.bluebrain.nexus.rdf.encoder.{GraphEncoder, PrimaryNode}
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.SubjectGraph
import ch.epfl.bluebrain.nexus.rdf.{Graph, MarshallingError}
import io.circe.Json

trait GraphDecodingSyntax {

  implicit final def graphDecodingSyntax(value: Graph): GraphOpsDecoding =
    new GraphOpsDecoding(value)

  implicit final def subjectGraphDecodingSyntax(value: SubjectGraph): SubjectGraphOpsDecoding =
    new SubjectGraphOpsDecoding(value)

  implicit final def subjectGraphDecodingSyntax(value: (IriNode, Graph)): SubjectGraphOpsDecoding = {
    val (primaryNode, graph) = value
    new SubjectGraphOpsDecoding(SubjectGraph(primaryNode, graph))
  }

  implicit final def valueDecodingThroughGraph[A: GraphEncoder](value: A): GraphOpsDecodingThoughGraph[A] =
    new GraphOpsDecodingThoughGraph(value)
}

final class SubjectGraphOpsDecoding(private val value: SubjectGraph) extends AnyVal {

  def as[A](context: Json)(implicit dec: GraphDecoder[A]): GraphDecoderResult[A] =
    dec(value, context)

  def as[A: GraphDecoder]: GraphDecoderResult[A] =
    as(Json.obj())
}

final class GraphOpsDecoding(private val value: Graph) extends AnyVal {

  def as[A](id: IriOrBNode, context: Json)(implicit dec: GraphDecoder[A]): GraphDecoderResult[A] =
    dec(SubjectGraph(id, value), context)

  def as[A: GraphDecoder](id: IriOrBNode): GraphDecoderResult[A] =
    as(id, Json.obj())
}

final class GraphOpsDecodingThoughGraph[A](value: A)(implicit enc: GraphEncoder[A]) {

  def as[B](context: Json)(implicit dec: GraphDecoder[B], P: PrimaryNode[A]): Either[MarshallingError, B] =
    enc(value).flatMap(subjectGraph => dec(subjectGraph, context))

  def as[B: GraphDecoder](implicit P: PrimaryNode[A]): Either[MarshallingError, B] =
    as(Json.obj())

  def as[B](id: IriOrBNode, context: Json)(implicit dec: GraphDecoder[B]): Either[MarshallingError, B] =
    enc(id, value).flatMap(subjectGraph => dec(subjectGraph, context))

  def as[B](id: IriOrBNode)(implicit dec: GraphDecoder[B]): Either[MarshallingError, B] =
    as(id, Json.obj())

}
