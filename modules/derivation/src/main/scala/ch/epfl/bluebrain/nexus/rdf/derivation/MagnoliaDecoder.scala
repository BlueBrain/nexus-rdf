package ch.epfl.bluebrain.nexus.rdf.derivation

import ch.epfl.bluebrain.nexus.rdf.Decoder.Result
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode
import ch.epfl.bluebrain.nexus.rdf.{Cursor, Decoder, DecodingFailure}
import magnolia.{CaseClass, SealedTrait}

private[derivation] object MagnoliaDecoder {

  def combine[A](caseClass: CaseClass[Decoder, A])(implicit config: Configuration): Decoder[A] = {
    val paramPredicateLookup = caseClass.parameters.map { p =>
      val idAnnotation = p.annotations.collectFirst {
        case ann: id => ann
      }
      idAnnotation match {
        case Some(ann) => p.label -> IriNode(ann.value)
        case None      => p.label -> IriNode(config.base + config.transformMemberNames(p.label))
      }
    }.toMap

    if (paramPredicateLookup.values.toList.distinct.size != caseClass.parameters.length) {
      throw DerivationError("Duplicate key detected after applying transformation function for case class parameters")
    }

    new Decoder[A] {
      override def apply(cursor: Cursor): Result[A] =
        caseClass.constructMonadic { p =>
          if (p.label == config.idMemberName) p.typeclass.apply(cursor)
          else {
            val next = cursor.downField(_ == paramPredicateLookup(p.label))
            next.focus match {
              case None    => p.default.fold(p.typeclass.apply(next))(Right[DecodingFailure, p.PType])
              case Some(_) => p.typeclass.apply(next)
            }
          }
        }
    }
  }

  def dispatch[A](sealedTrait: SealedTrait[Decoder, A])(implicit config: Configuration): Decoder[A] =
    new Decoder[A] {
      override def apply(c: Cursor): Result[A] = {
        c.downField(_ == IriNode(config.discriminatorPredicate)).values match {
          case Some(values) =>
            val vset = values.toSet
            sealedTrait.subtypes.find(st => vset.contains(IriNode(config.base + st.typeName.short))) match {
              case Some(st) => st.typeclass.apply(c)
              case None =>
                Left(DecodingFailure(s"Unable to find type discriminator for ${sealedTrait.typeName.short}", c.history))
            }
          case None =>
            Left(DecodingFailure(s"Unable to find type discriminator for ${sealedTrait.typeName.short}", c.history))
        }
      }
    }
}
