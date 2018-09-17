package ch.epfl.bluebrain.nexus.rdf

/**
  * Set of rules to convert a Json-LD document into a [[Graph]]
  *
  * @param castDateTypes flag to automatically attempt to cast string literals to ''xsd:dateTime'', ''xsd:date'' or ''xsd:time''
  *                      when they match the expected format.
  */
final case class GraphConfiguration(castDateTypes: Boolean)
