package ch.epfl.bluebrain.nexus.rdf.encoder

import cats.Id
import ch.epfl.bluebrain.nexus.rdf.Graph.Triple
import ch.epfl.bluebrain.nexus.rdf.MarshallingError.{ConversionError, RootNodeNotFound, Unexpected}
import ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.EncoderResult
import ch.epfl.bluebrain.nexus.rdf.jena.{JenaConversions, JenaModel}
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel.JenaModelErr
import ch.epfl.bluebrain.nexus.rdf.{Graph, MarshallingError, RootedGraph}
import io.circe.Json
import org.apache.jena.rdf.model.Model
import scala.collection.JavaConverters._

/**
  * Defines an encoder from ''A'' to [[Graph]]
  */
trait GraphEncoder[F[_], A] {

  /**
    * Attempts to transform a value of type ''A'' to a [[ch.epfl.bluebrain.nexus.rdf.RootedGraph]].
    *
    * @param rootNode the root node of the graph
    * @param value    the value to convert into a [[ch.epfl.bluebrain.nexus.rdf.RootedGraph]]
    */
  def apply(rootNode: IriOrBNode, value: A): F[RootedGraph]

  /**
    * Attempts to transform a value of type ''A'' to a [[Graph]] with the root node being extracted from [[RootNode]]
    *
    * @param value the value to convert into a [[Graph]]
    */
  def apply(value: A)(implicit rootExtractor: RootNode[A]): F[RootedGraph] =
    apply(rootExtractor(value), value)

  def toEither(implicit ev: F[RootedGraph] =:= Id[RootedGraph]): GraphEncoder[EncoderResult, A] =
    (rootNode: IriOrBNode, value: A) => Right(apply(rootNode, value))

}

object GraphEncoder {

  type EncoderResult[F] = Either[MarshallingError, F]

  def apply[F[_], A](f: (IriOrBNode, A) => Graph): GraphEncoder[Id, A] =
    (rootNode: IriOrBNode, v: A) => RootedGraph(rootNode, f(rootNode, v))

  implicit val jenaModelRootedGraphEncoder: GraphEncoder[EncoderResult, Model] =
    (id: IriOrBNode, model: Model) =>
      model
        .listStatements()
        .asScala
        .foldLeft[EncoderResult[Set[Triple]]](Right(Set.empty)) {
          case (Right(acc), s) =>
            val results = for {
              ss <- JenaConversions.toIriOrBNode(s.getSubject)
              pp <- JenaConversions.propToIriNode(s.getPredicate)
              oo <- JenaConversions.rdfNodeToNode(s.getObject)
            } yield (ss, pp, oo)
            results.map(acc + _).left.map(msg => ConversionError(msg, Some(JenaModelErr.InvalidJsonLD(msg))))
          case (l @ Left(_), _) => l
        }
        .flatMap { triples =>
          val graph = RootedGraph(id, triples)
          if (graph.subjects().isEmpty || graph.subjects().contains(id)) Right(graph)
          else Left(RootNodeNotFound(Some(id), s"The provided id '$id' is not part of the graph"))
      }

  implicit val jsonRootedGraphEncoder: GraphEncoder[EncoderResult, Json] =
    (id, json) =>
      JenaModel(json) match {
        case Right(model)                                    => jenaModelRootedGraphEncoder(id, model)
        case Left(err @ JenaModelErr.InvalidJsonLD(message)) => Left(ConversionError(message, Some(err)))
        case Left(JenaModelErr.Unexpected(message))          => Left(Unexpected(message))
    }

}
