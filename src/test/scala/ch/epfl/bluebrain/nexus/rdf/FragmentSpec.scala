package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class FragmentSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "A Fragment" should {
    val pct =
      "%C2%A3%C2%A4%C2%A5%C2%A6%C2%A7%C2%A8%C2%A9%C2%AA%C2%AB%C2%AC%C2%AD%C2%AE%C2%AF%C2%B0%C2%B1%C2%B2%C2%B3%C2%B4%C2%B5%C2%B6%C2%B7%C2%B8%C2%B9%C2%BA%C2%BB%C2%BC%C2%BD%C2%BE%C2%BF%C3%80%C3%81%C3%82%C3%83%C3%84%C3%85%C3%86"
    val ucs    = "£¤¥¦§¨©ª«¬\u00AD®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆ"
    val delims = "!$&'()*+,;=:"
    val rest   = "?/:@"

    "be parsed correctly from a string" in {
      Fragment(pct + ucs + delims + rest).right.value.value shouldEqual ucs + ucs + delims + rest
    }

    "succeed for empty" in {
      Fragment("").right.value
    }

    "show" in {
      Fragment(pct + ucs + delims + rest).right.value.show shouldEqual ucs + ucs + delims + rest
    }

    "eq" in {
      val lhs = Fragment(pct + ucs + delims + rest).right.value
      val rhs = Fragment(ucs + ucs + delims + rest).right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }
  }
}
