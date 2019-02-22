package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class NidSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "A Nid" should {
    "be constructed successfully" in {
      val strings = List("aa", "a-a", "1a", "11", "AA", s"a${List.fill(30)("1").mkString}a")
      forAll(strings) { s =>
        Nid(s).right.value
      }
    }
    "fail to construct" in {
      val strings = List("", "-a", "a-", "a", "%20a", "-", s"a${List.fill(31)("1").mkString}a")
      forAll(strings) { s =>
        Nid(s).left.value
      }
    }
    val normalized = Nid("IbAn")

    "normalize input during construction" in {
      normalized.right.value.value shouldEqual "iban"
    }
    "show" in {
      normalized.right.value.show shouldEqual "iban"
    }
    "eq" in {
      Eq.eqv(Nid("iban").right.value, normalized.right.value) shouldEqual true
    }
  }
}
