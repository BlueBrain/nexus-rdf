package ch.epfl.bluebrain.nexus.rdf.decoder

import ch.epfl.bluebrain.nexus.rdf.MarshallingError

/**
  * Enumeration type for graph decoding errors
  */
sealed trait GraphDecoderError extends MarshallingError {
  def message: String
}

object GraphDecoderError {

  /**
    * Signals the impossibility to perform the conversion.
    *
    * @param message    the human readable error details
    * @param underlying the undrlying error
    */
  final case class ConversionError[A](message: String, underlying: Option[A] = None) extends GraphDecoderError

  /**
    * Signals an unexpected error while performing the conversion.
    *
    * @param message the human readable error details
    */
  final case class Unexpected(message: String) extends GraphDecoderError

}
