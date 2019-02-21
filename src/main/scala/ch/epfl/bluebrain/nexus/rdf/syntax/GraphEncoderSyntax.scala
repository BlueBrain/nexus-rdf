package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.GraphEncoderResult
import ch.epfl.bluebrain.nexus.rdf.encoder.{GraphEncoder, PrimaryNode}

trait GraphEncoderSyntax {

  implicit final def graphEncoderSyntax[A](value: A): GraphOpsEncoding[A] = new GraphOpsEncoding(value)
}

final class GraphOpsEncoding[A](private val value: A) extends AnyVal {

  def asGraph(rootNode: IriOrBNode)(implicit enc: GraphEncoder[A]): GraphEncoderResult =
    enc(rootNode, value)

  def asGraph(id: AbsoluteIri)(implicit enc: GraphEncoder[A]): GraphEncoderResult =
    enc(id, value)

  def asGraph(implicit enc: GraphEncoder[A], P: PrimaryNode[A]): GraphEncoderResult =
    enc(value)

  def asGraph(fRootNode: Graph => Option[IriOrBNode])(implicit enc: GraphEncoder[A]): GraphEncoderResult =
    enc(fRootNode, value)
}
