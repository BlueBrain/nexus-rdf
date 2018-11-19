package ch.epfl.bluebrain.nexus.rdf

import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Path}
import io.circe.{Decoder, Encoder}

package object instances {

  final implicit val absoluteIriEncoder: Encoder[AbsoluteIri] = Encoder.encodeString.contramap(_.asString)
  final implicit val absoluteIriDecoder: Decoder[AbsoluteIri] = Decoder.decodeString.emap(Iri.absolute)

  final implicit val iriPathEncoder: Encoder[Path] = Encoder.encodeString.contramap(_.asString)
  final implicit val iriPathDecoder: Decoder[Path] = Decoder.decodeString.emap(Path.apply)

  final implicit val iriEncoder: Encoder[Iri] = Encoder.encodeString.contramap(_.asString)
  final implicit val iriDecoder: Decoder[Iri] = Decoder.decodeString.emap(Iri.apply)
}
