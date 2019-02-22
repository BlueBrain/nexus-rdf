package ch.epfl.bluebrain.nexus.rdf

/**
  * Enumeration type for graph marshalling errors
  */
trait MarshallingError extends Product with Serializable {
  def message: String
}

object MarshallingError {

  /**
    * Signals the impossibility to generate the perform the desired conversion.
    *
    * @param message    the human readable error details
    * @param underlying the underlying error
    */
  final case class ConversionError[A](message: String, underlying: Option[A] = None) extends MarshallingError

  /**
    * Signals the impossibility to retrieve or generate the root node.
    *
    * @param message the human readable error details
    */
  final case class RootNodeNotFound(message: String) extends MarshallingError

  /**
    * Signals an unexpected error.
    *
    * @param message the human readable error details
    */
  final case class Unexpected(message: String) extends MarshallingError

  /**
    * @param message the human readable error details
    * @return returns the  [[RootNodeNotFound]] error
    */
  def rootNotFound(message: String = "The root node could not be found"): MarshallingError = RootNodeNotFound(message)

}
