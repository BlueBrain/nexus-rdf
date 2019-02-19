package ch.epfl.bluebrain.nexus.rdf.encoder

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.GraphResult

/**
  * Defines an encoder from ''A'' to [[Graph]]
  */
trait GraphEncoder[A] {

  /**
    * Transform a value of type ''A'' to a [[GraphResult]]
    * @param value the value to convert into a [[GraphResult]]
    * @return a [[GraphResult]]
    */
  def apply(value: A): GraphResult
}
object GraphEncoder {

  def apply[A](f: A => (IriOrBNode, Graph)): GraphEncoder[A] =
    (v: A) => f(v) match { case (s, g) => GraphResult(s, g) }

  /**
    * A graph with its primary node
    *
    * @param subject the primary node
    * @param graph   the graph
    */
  final case class GraphResult(subject: IriOrBNode, graph: Graph)
}
