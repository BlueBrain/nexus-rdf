package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri.Path._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class UrlSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "An Url" should {
    "be parsed correctly" in {
      val cases = List(
        "hTtps://me:me@hOst:443/a/b?a&e=f&b=c#frag"   -> "https://me:me@host/a/b?a&b=c&e=f#frag",
        "hTtps://me:me@hOst#frag"                     -> "https://me:me@host#frag",
        "hTtps://me:me@hOst?"                         -> "https://me:me@host?",
        "hTtp://hOst%C2%A3:80/a%C2%A3/b%C3%86c//:://" -> "http://host£/a£/bÆc//:://",
        "hTtp://1.2.3.4:80/a%C2%A3/b%C3%86c//:://"    -> "http://1.2.3.4/a£/bÆc//:://",
        "hTtp://1.2.3.4:80/a%C2%A3/b%C3%86c//:://"    -> "http://1.2.3.4/a£/bÆc//:://",
        "http://google.com/#"                         -> "http://google.com/#",
        "FiLe:///bin/bash"                            -> "file:///bin/bash",
        "FiLe:/bin/bash"                              -> "file:/bin/bash",
        "MailtO:test@example.com"                     -> "mailto:test@example.com",
        "http://google.com/.."                        -> "http://google.com",
        "http://google.com/./"                        -> "http://google.com/",
        "http://google.com/a/../search/."             -> "http://google.com/search",
        "http://google.com/a/../search/.."            -> "http://google.com"
      )
      forAll(cases) {
        case (in, expected) => Url(in).right.value.asString shouldEqual expected
      }
    }
    val withHash = Iri.absolute("hTtp://1.2.3.4:80/a%C2%A3/b%C3%86c//:://#hash").right

    "be absolute" in {
      withHash.value.isAbsolute shouldEqual true
    }

    "be a Url" in {
      withHash.value.isUrl shouldEqual true
    }

    "show" in {
      Iri
        .absolute("hTtp://1.2.3.4:80/a%C2%A3/b%C3%86c/£/#hash")
        .right
        .value
        .show shouldEqual "http://1.2.3.4/a%C2%A3/b%C3%86c/%C2%A3/#hash"
    }

    "return an optional self" in {
      withHash.value.asUrl shouldEqual Some(withHash.value)
    }

    "return an optional self from asAbsolute" in {
      withHash.value.asAbsolute shouldEqual Some(withHash.value)
    }

    "not be an Urn" in {
      withHash.value.isUrn shouldEqual false
    }

    "not return a urn" in {
      withHash.value.asUrn shouldEqual None
    }

    "not be a RelativeIri" in {
      withHash.value.isRelative shouldEqual false
    }

    "not return a RelativeIri" in {
      withHash.value.asRelative shouldEqual None
    }

    "append segment" in {
      val cases = List(
        ("http://google.com/a/", "bcd", "http://google.com/a/bcd"),
        ("http://google.com/a/", "/bcd", "http://google.com/a/bcd"),
        ("http://google.com/a/", "/", "http://google.com/a/"),
        ("http://google.com/a?one=two&three", "bcd", "http://google.com/a/bcd?one=two&three"),
        ("http://google.com/a#other", "bcd", "http://google.com/a/bcd#other")
      )
      forAll(cases) {
        case (in, segment, expected) => (Url(in).right.value + segment) shouldEqual Url(expected).right.value
      }
    }

    "append path" in {
      val cases = List(
        ("http://google.com/a/", "/b/c/d", "http://google.com/a/b/c/d"),
        ("http://google.com/a/", "/", "http://google.com/a/"),
        ("http://google.com/a/", "/bcd", "http://google.com/a/bcd"),
        ("http://google.com/a?one=two&three", "/b/c/d", "http://google.com/a/b/c/d?one=two&three"),
        ("http://google.com/a#other", "/b/c/d", "http://google.com/a/b/c/d#other")
      )
      forAll(cases) {
        case (in, p, expected) => (Url(in).right.value + Path(p).right.value) shouldEqual Url(expected).right.value
      }
      Url("http://google.com/a").right.value + ("b" / "c") shouldEqual Url("http://google.com/a/b/c").right.value
    }

    "eq" in {
      val lhs = Url("hTtp://gooGle.com/a/../search?q=asd#1").right.value
      val rhs = Url("http://google.com/search?q=asd#1").right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }
  }
}
