package ch.epfl.bluebrain.nexus.rdf.encoder

import ch.epfl.bluebrain.nexus.rdf.Node

/**
  * Enumeration type for encoding errors
  */
sealed abstract class NodeEncoderError(val message: String) extends Product with Serializable

object NodeEncoderError {

  /**
    * No element to be encoded
    */
  final case object NoElementToEncode extends NodeEncoderError("No element to be encoded")

  /**
    * The encoding failed when attempting to convert the [[ch.epfl.bluebrain.nexus.rdf.Node]] to ''A''
    *
    * @param msg the human readable message
    * @param th  the optionally available [[Throwable]]
    */
  final case class IllegalConversion(msg: String, th: Option[Throwable] = None) extends NodeEncoderError(msg)

  /**
    * The encoding failed because the [[ch.epfl.bluebrain.nexus.rdf.Node]] was of an unexpected type
    *
    * @param expectedType the expected type of [[ch.epfl.bluebrain.nexus.rdf.Node]] to be able to do the conversion to ''A''
    * @param current  the actual [[ch.epfl.bluebrain.nexus.rdf.Node]]
    */
  final case class IllegalType(expectedType: String, current: Node)
      extends NodeEncoderError(s"Expected a node of type $expectedType but found $current")
}
