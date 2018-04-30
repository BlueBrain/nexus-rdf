package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Curie.Prefix
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class CurieSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {
  "A Prefix" should {

    val valid   = List("prefix", "PrEfIx", "_prefix", "__prefix", "_.prefix", "pre-fix", "_123.456", "Àprefix", "Öfix")
    val invalid = List("-prefix", "!prefix", ":prefix", ".prefix", "6prefix", "prefi!x", "prefix:", "")

    "be parsed correctly from string" in {
      forAll(valid) {
        case in => Prefix(in).right.value.value shouldEqual in
      }
    }

    "fail parsing from an invalid string" in {
      forAll(invalid) {
        case in => Prefix(in).left.value should not be 'empty
      }
    }

    "show" in {
      forAll(valid) {
        case in => Prefix(in).right.value.show shouldEqual in
      }
    }

    "eq" in {
      val lhs = Prefix("prefix").right.value
      val rhs = Prefix("prefix").right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }
  }

  "A Curie" should {
    val valid = List(
      ("rdf:type", "rdf", "type"),
      ("prefix://me:me@hOst:443/a/b?a&e=f&b=c#frag", "prefix", "//me:me@host:443/a/b?a&b=c&e=f#frag"),
      ("PrEfIx://me:me@hOst#frag", "PrEfIx", "//me:me@host#frag"),
      ("_prefix:/some/:/path", "_prefix", "/some/:/path"),
      ("_.prefix:/:/some/path", "_.prefix", "/:/some/path"),
      ("pre-fix:some/:/path", "pre-fix", "some/:/path"),
      ("_123.456:?q=v", "_123.456", "?q=v"),
      ("Àprefix:#frag", "Àprefix", "#frag"),
      ("Öfix://hOst%C2%A3:80/a%C2%A3/b%C3%86c//:://", "Öfix", "//host£:80/a£/bÆc//:://")
    )
    val invalid = List("-prefix",
                       "!prefix",
                       ":prefix",
                       ".prefix",
                       "6prefix",
                       "prefi!x",
                       "prefix:",
                       "//hOst%C2%A3:80/a%C2%A3/b%C3%86c//:://",
                       "?q=v",
                       "")

    "be parsed correctly from string" in {
      forAll(valid) {
        case (in, p, r) =>
          val curie = Curie(in).right.value
          curie.prefix.value shouldEqual p
          curie.reference.asString shouldEqual r
          curie.show shouldEqual s"$p:${curie.reference.asUri}"
      }
    }

    "fail parsing from an invalid string" in {
      forAll(invalid) {
        case in => Curie(in).left.value should not be 'empty
      }
    }

    "eq" in {
      val lhs = Curie("rdf:type").right.value
      val rhs = Curie("rdf:type").right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }

    "not eq" in {
      val lhs = Curie("rdf:type").right.value
      val rhs = Curie("RdF:type").right.value
      Eq.eqv(lhs, rhs) shouldEqual false
    }
  }
}
