package ch.epfl.bluebrain.nexus.rdf.jsonld.instances

import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Path, RelativeIri, Url, Urn}
import io.circe.{Decoder, Encoder}

trait RdfCirceInstances {
  final implicit val absoluteIriEncoder: Encoder[AbsoluteIri] = Encoder.encodeString.contramap(_.asString)
  final implicit val absoluteIriDecoder: Decoder[AbsoluteIri] = Decoder.decodeString.emap(Iri.absolute)

  final implicit val iriPathEncoder: Encoder[Path] = Encoder.encodeString.contramap(_.asString)
  final implicit val iriPathDecoder: Decoder[Path] = Decoder.decodeString.emap(Path.apply)

  final implicit val iriEncoder: Encoder[Iri] = Encoder.encodeString.contramap(_.asString)
  final implicit val iriDecoder: Decoder[Iri] = Decoder.decodeString.emap(Iri.apply)

  implicit def urlEncoder(implicit E: Encoder[AbsoluteIri]): Encoder[Url] = E.contramap(identity)
  implicit val urlDecoder: Decoder[Url]                                   = Decoder.decodeString.emap(Url.apply)

  implicit def urnEncoder(implicit E: Encoder[AbsoluteIri]): Encoder[Urn] = E.contramap(identity)
  implicit val urnDecoder: Decoder[Urn]                                   = Decoder.decodeString.emap(Urn.apply)

  final implicit val relativeIriEncoder: Encoder[RelativeIri] = Encoder.encodeString.contramap(_.asString)
  final implicit val relativeIriDecoder: Decoder[RelativeIri] = Decoder.decodeString.emap(Iri.relative)

}

object RdfCirceInstances extends RdfCirceInstances
