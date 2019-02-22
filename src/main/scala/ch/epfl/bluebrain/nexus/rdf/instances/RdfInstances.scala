package ch.epfl.bluebrain.nexus.rdf.instances

import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Path, RelativeIri, Url, Urn}
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.{Iri, Node}
import io.circe.{Decoder, Encoder}

trait RdfInstances {
  final implicit val absoluteIriEncoder: Encoder[AbsoluteIri] = Encoder.encodeString.contramap(_.asString)
  final implicit val absoluteIriDecoder: Decoder[AbsoluteIri] = Decoder.decodeString.emap(Iri.absolute)

  final implicit val iriPathEncoder: Encoder[Path] = Encoder.encodeString.contramap(_.asString)
  final implicit val iriPathDecoder: Decoder[Path] = Decoder.decodeString.emap(Path.apply)

  final implicit val iriEncoder: Encoder[Iri] = Encoder.encodeString.contramap(_.asString)
  final implicit val iriDecoder: Decoder[Iri] = Decoder.decodeString.emap(Iri.apply)

  implicit val urlEncoder: Encoder[Url] = Encoder[AbsoluteIri].contramap(identity)
  implicit val urlDecoder: Decoder[Url] = Decoder.decodeString.emap(Url.apply)

  implicit val urnEncoder: Encoder[Urn] = Encoder[AbsoluteIri].contramap(identity)
  implicit val urnDecoder: Decoder[Urn] = Decoder.decodeString.emap(Urn.apply)

  final implicit val relativeIriEncoder: Encoder[RelativeIri] = Encoder.encodeString.contramap(_.asString)
  final implicit val relativeIriDecoder: Decoder[RelativeIri] = Decoder.decodeString.emap(Iri.relative)

  final implicit def toIriNode(iri: AbsoluteIri): IriNode =
    IriNode(iri)

  final implicit def toLiteral(value: String): Literal =
    Literal(value)

  final implicit def toLiteral(value: Int): Literal =
    Literal(value)

  final implicit def toLiteral(value: Long): Literal =
    Literal(value)

  final implicit def toLiteral(value: Float): Literal =
    Literal(value)

  final implicit def toLiteral(value: Double): Literal =
    Literal(value)

  final implicit def toLiteral(value: Boolean): Literal =
    Literal(value)

  final implicit def sToEq(s: IriOrBNode): IriOrBNode => Boolean = _ == s
  final implicit def oToEq(o: Node): Node => Boolean             = _ == o
  final implicit def pToEq(p: IriNode): IriNode => Boolean       = _ == p

}

object RdfInstances extends RdfInstances
