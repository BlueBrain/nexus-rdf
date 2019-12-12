package ch.epfl.bluebrain.nexus.rdf.derivation.decoder

import ch.epfl.bluebrain.nexus.rdf.Decoder
import ch.epfl.bluebrain.nexus.rdf.derivation.{Configuration, MagnoliaDecoder}
import magnolia.{CaseClass, Magnolia, SealedTrait}

object semiauto {

  type Typeclass[T] = Decoder[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    MagnoliaDecoder.combine(caseClass)(Configuration.default)

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    MagnoliaDecoder.dispatch(sealedTrait)(Configuration.default)

  def deriveDecoder[T]: Typeclass[T] = macro Magnolia.gen[T]
}
