package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class UrlSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "An Url" should {
    "be parsed correctly" in {
      val cases = List(
        "hTtps://me:me@hOst:443/a/b?a&e=f&b=c#frag"   -> "https://me:me@host/a/b?a&b=c&e=f#frag",
        "hTtps://me:me@hOst#frag"                     -> "https://me:me@host#frag",
        "hTtp://hOst%C2%A3:80/a%C2%A3/b%C3%86c//:://" -> "http://host£/a£/bÆc//:://",
        "hTtp://1.2.3.4:80/a%C2%A3/b%C3%86c//:://"    -> "http://1.2.3.4/a£/bÆc//:://"
      )
      forAll(cases) {
        case (in, expected) =>
          Url(in).right.value.show shouldEqual expected
      }
    }
    "eq" in {
      val lhs = Url("hTtp://gooGle.com/?q=asd#1").right.value
      val rhs = Url("http://google.com/?q=asd#1").right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }
  }
}
