package ch.epfl.bluebrain.nexus.rdf.encoder

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node
import ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoder.EncoderResult
import ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoderError.{IllegalConversion, IllegalType}

import scala.util.Try

/**
  * Defines an encoder form [[Node]] to ''A''
  * @tparam A
  */
trait NodeEncoder[A] {

  /**
    * Attempts to transform a [[Node]] to ''A''
    * @param node the node
    * @return a [[ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoderError]] when failed and an ''A'' when successful
    */
  def apply(node: Node): EncoderResult[A]
}

object NodeEncoder {
  type EncoderResult[A] = Either[NodeEncoderError, A]

  implicit val intEncoder: NodeEncoder[Int] = (node: Node) => numeric(node, _.toInt)

  implicit val longEncoder: NodeEncoder[Long] = (node: Node) => numeric(node, _.toLong)

  implicit val floatEncoder: NodeEncoder[Float] = (node: Node) => numeric(node, _.toFloat)

  implicit val doubleEncoder: NodeEncoder[Double] = (node: Node) => numeric(node, _.toDouble)

  implicit val booleanEncoder: NodeEncoder[Boolean] = (node: Node) =>
    node.asLiteral.toRight(IllegalType("Literal", node)).flatMap { literal =>
      Try(literal.lexicalForm.toBoolean).toEither.left.map(err => IllegalConversion(err.getMessage, Some(err)))
  }

  implicit val stringEncoder: NodeEncoder[String] = (node: Node) =>
    node.asLiteral.toRight(IllegalType("Literal", node)).flatMap { literal =>
      if (literal.isString) Right(literal.lexicalForm)
      else Left(IllegalConversion("Expected a string literal, but found otherwise"))
  }

  implicit val uuidEncoder: NodeEncoder[UUID] = (node: Node) =>
    stringEncoder(node).flatMap(s =>
      Try(UUID.fromString(s)).toEither.left.map(err => IllegalConversion(err.getMessage, Some(err))))

  implicit val absoluteIriEncoder: NodeEncoder[AbsoluteIri] = (node: Node) =>
    node.asIri.toRight(IllegalType("Iri", node)).map(_.value)

  private def numeric[A](node: Node, f: String => A): EncoderResult[A] = {
    node.asLiteral.toRight(IllegalType("Literal", node)).flatMap { literal =>
      if (literal.isNumeric)
        Try(f(literal.lexicalForm)).toEither.left.map(err => IllegalConversion(err.getMessage, Some(err)))
      else Left(IllegalConversion("Expected a numeric literal, but found otherwise"))
    }
  }

}
