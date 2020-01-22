package ch.epfl.bluebrain.nexus.rdf.derivation.encoder

import ch.epfl.bluebrain.nexus.rdf.Encoder
import ch.epfl.bluebrain.nexus.rdf.derivation.{Configuration, MagnoliaEncoder}
import magnolia.{CaseClass, Magnolia, SealedTrait}

object semiauto {

  type Typeclass[T] = Encoder[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
    MagnoliaEncoder.combine(caseClass)(Configuration.default)

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    MagnoliaEncoder.dispatch(sealedTrait)(Configuration.default)

  def deriveEncoder[T]: Typeclass[T] = macro Magnolia.gen[T]
}
