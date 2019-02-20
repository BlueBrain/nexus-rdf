package ch.epfl.bluebrain.nexus.rdf.encoder

import ch.epfl.bluebrain.nexus.rdf.MarshallingError

/**
  * Enumeration type for graph encoding errors
  */
sealed trait GraphEncoderError extends MarshallingError {
  def message: String
}

object GraphEncoderError {

  /**
    * Signals the impossibility to generate the graph.
    *
    * @param message    the human readable error details
    * @param underlying the undrlying error
    */
  final case class ConversionError[A](message: String, underlying: Option[A] = None) extends GraphEncoderError

  /**
    * Signals an unexpected error.
    *
    * @param message the human readable error details
    */
  final case class Unexpected(message: String) extends GraphEncoderError

}
