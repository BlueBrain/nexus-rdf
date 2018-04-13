package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}
import cats.syntax.show._

class SchemeSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "A Scheme" should {
    "be constructed successfully" in {
      val strings = List("urn", "https", "http", "file", "ftp", "ssh", "a", "a0", "a-", "a+", "a.", "HTTPS", "A0-+.")
      forAll(strings) { s =>
        Scheme(s).right.value
      }
    }
    "fail to construct" in {
      val strings = List("", "0", "0a", "as_", "%20a", "0-+.")
      forAll(strings) { s =>
        Scheme(s).left.value
      }
    }
    "return the appropriate boolean flag" in {
      val cases = List(
        ("urn", true, false, false),
        ("URN", true, false, false),
        ("https", false, true, false),
        ("HTTPS", false, true, false),
        ("http", false, false, true),
        ("HTTP", false, false, true)
      )
      forAll(cases) {
        case (string, isUrn, isHttps, isHttp) =>
          val scheme = Scheme(string).right.value
          scheme.isUrn shouldEqual isUrn
          scheme.isHttps shouldEqual isHttps
          scheme.isHttp shouldEqual isHttp
      }
    }

    val normalized = Scheme("HtTpS").right.value

    "normalize input during construction" in {
      normalized.value shouldEqual "https"
    }
    "show" in {
      normalized.show shouldEqual "https"
    }
    "eq" in {
      Eq.eqv(Scheme("https").right.value, normalized) shouldEqual true
    }
  }
}
