package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class UserInfoSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "An UserInfo" should {
    val pct =
      "%C2%A3%C2%A4%C2%A5%C2%A6%C2%A7%C2%A8%C2%A9%C2%AA%C2%AB%C2%AC%C2%AD%C2%AE%C2%AF%C2%B0%C2%B1%C2%B2%C2%B3%C2%B4%C2%B5%C2%B6%C2%B7%C2%B8%C2%B9%C2%BA%C2%BB%C2%BC%C2%BD%C2%BE%C2%BF%C3%80%C3%81%C3%82%C3%83%C3%84%C3%85%C3%86"
    val ucsUp  = "£¤¥¦§¨©ª«¬\u00AD®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆ"
    val ucsLow = "£¤¥¦§¨©ª«¬\u00AD®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæ"
    val delims = "!$&'()*+,;=:"
    val up     = "ABCD"
    val low    = "abcd"

    "be parsed correctly from a string" in {
      UserInfo("aBcd:Efgh").right.value.value shouldEqual "aBcd:Efgh"
    }

    "equal when compared with ignored casing" in {
      UserInfo("aBcd:Efgh").right.value equalsIgnoreCase UserInfo("Abcd:efgH").right.value shouldEqual true
    }

    "be parsed correctly from percent encoded string" in {
      UserInfo(pct).right.value.value shouldEqual ucsUp
    }

    "be parsed correctly from ucs chars" in {
      UserInfo(ucsUp).right.value.value shouldEqual ucsUp
    }

    "be parsed correctly from delimiters" in {
      UserInfo(delims).right.value.value shouldEqual delims
    }

    "be parsed correctly from mixed characters" in {
      val in  = ucsUp + ucsLow + pct + delims + up
      val out = ucsUp + ucsLow + ucsUp + delims + up
      UserInfo(in).right.value.value shouldEqual out
    }

    "fail for empty" in {
      UserInfo("").left.value
    }

    "show" in {
      UserInfo(up + low).right.value.show shouldEqual (up + low)
    }

    "eq" in {
      Eq.eqv(UserInfo(ucsUp + ucsLow).right.value, UserInfo(ucsUp + ucsLow).right.value) shouldEqual true
    }

    "not eq" in {
      Eq.eqv(UserInfo(ucsUp).right.value, UserInfo(ucsLow).right.value) shouldEqual false
    }
  }
}
