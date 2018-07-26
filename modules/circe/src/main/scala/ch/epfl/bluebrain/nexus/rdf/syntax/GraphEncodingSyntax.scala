package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder
import ch.epfl.bluebrain.nexus.rdf.encoder.GraphEncoder.GraphResult
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import io.circe._

trait GraphEncodingSyntax {

  implicit final def circeEncoderSyntax[A](value: A): CirceOpsEncoding[A] = new CirceOpsEncoding(value)
}

final class CirceOpsEncoding[A](private val value: A) extends AnyVal {

  /**
    * Convert [[A]] into JSON-LD representation using the implicitly available [[GraphEncoder]]
    * and the provided context. Beware, that currently IRI contexts are not resolved and will be ignored.
    *
    * @param context context to use when creating JSON-LD representation
    * @return [[Json]] containing JSON-LD representation
    */
  def asJson(context: Json)(implicit enc: GraphEncoder[A]): Json = {
    val GraphResult(subject, graph) = enc(value)
    graph.asJson(context, Some(subject)).getOrElse(graph.asJson)
  }
}
