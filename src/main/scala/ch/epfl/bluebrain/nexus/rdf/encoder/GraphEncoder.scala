package ch.epfl.bluebrain.nexus.rdf.encoder

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Graph.Triple
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.GraphEncoderResult
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoderError._
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel.JenaModelErr
import ch.epfl.bluebrain.nexus.rdf.jena.{JenaConversions, JenaModel}
import io.circe.Json
import org.apache.jena.rdf.model.Model

import scala.collection.JavaConverters._

/**
  * Defines an encoder from ''A'' to [[Graph]]
  */
trait GraphEncoder[A] {

  /**
    * Attempts to transform a value of type ''A'' to a [[ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.SubjectGraph]].
    *
    * @param primaryNode the primary node of the graph
    * @param value       the value to convert into a [[ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.SubjectGraph]]
    */
  def apply(primaryNode: IriOrBNode, value: A): GraphEncoderResult

  /**
    * Attempts to transform a value of type ''A'' to a [[ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.SubjectGraph]].
    *
    * @param primaryNode the id which is the primary node of the graph
    * @param value       the value to convert into a [[ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.SubjectGraph]]
    */
  def apply(primaryNode: AbsoluteIri, value: A): GraphEncoderResult = apply(IriNode(primaryNode), value)

  /**
    * Attempts to transform a value of type ''A'' to a [[Graph]] with the primary node being extracted from [[PrimaryNode]]
    *
    * @param value the value to convert into a [[Graph]]
    */
  def apply(value: A)(implicit primaryExtractor: PrimaryNode[A]): GraphEncoderResult =
    apply(primaryExtractor(value), value)

}

object GraphEncoder {

  type GraphEncoderResult = Either[GraphEncoderError, SubjectGraph]

  def apply[A](f: (IriOrBNode, A) => Graph): GraphEncoder[A] =
    (primaryNode, v) => Right(SubjectGraph(primaryNode, f(primaryNode, v)))

  implicit val jenaModelGraphEncoder: GraphEncoder[Model] =
    (id, model) =>
      model
        .listStatements()
        .asScala
        .foldLeft[Either[GraphEncoderError, Set[Triple]]](Right(Set.empty)) {
          case (Right(acc), s) =>
            val results = for {
              ss <- JenaConversions.toIriOrBNode(s.getSubject)
              pp <- JenaConversions.propToIriNode(s.getPredicate)
              oo <- JenaConversions.rdfNodeToNode(s.getObject)
            } yield ((ss, pp, oo))
            results.map(acc + _).left.map(msg => ConversionError(msg, Some(JenaModelErr.InvalidJsonLD(msg))))
          case (l @ Left(_), _) => l
        }
        .map(triples => SubjectGraph(id, Graph(triples)))

  implicit val jsonGraphEncoder: GraphEncoder[Json] =
    (id, json) =>
      JenaModel(json) match {
        case Right(model)                                    => jenaModelGraphEncoder(id, model)
        case Left(err @ JenaModelErr.InvalidJsonLD(message)) => Left(ConversionError(message, Some(err)))
        case Left(JenaModelErr.Unexpected(message))          => Left(Unexpected(message))
    }

  /**
    * A graph with its primary node
    *
    * @param subject the primary node
    * @param graph   the graph
    */
  final case class SubjectGraph(subject: IriOrBNode, graph: Graph)
}
