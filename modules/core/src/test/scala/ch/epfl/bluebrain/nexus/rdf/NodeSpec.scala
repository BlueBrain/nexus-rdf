package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Node.Literal.LanguageTag
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class NodeSpec extends WordSpecLike with Matchers with EitherValues with Inspectors {

  "A BNode" should {
    "be constructed correctly" in {
      val cases = List("a", "a-_", "a123")
      forAll(cases) { el =>
        Node.blank(el).right.value
      }
    }
    "fail to construct" in {
      val cases = List("", " ", "a#", "_", "-", "-a", "_a")
      forAll(cases) { el =>
        Node.blank(el).left.value
      }
    }
    "show" in {
      Node.blank.show
    }
    "eq" in {
      val lhs = Node.blank("a1").right.value
      val rhs = Node.blank("a1").right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }
  }

  "A LanguageTag" should {
    "be constructed correctly" in {
      val cases = List(
        "zh-guoyu",
        "sgn-BE-FR",
        "i-default",
        "en-GB-oed",
        "x-123asd-123asd78",
        "es-419",
        "en-US-x-twain",
        "de-Latn-DE-1996",
        "zh-Hans"
      )
      forAll(cases) { el =>
        LanguageTag(el).right.value
      }
    }
    "fail to construct" in {
      val cases = List(
        "",
        " ",
        "a",
        "213456475869707865433",
        "!",
        "aaaaaaaa4h5kj324h54"
      )
      forAll(cases) { el =>
        LanguageTag(el).left.value
      }
    }
  }

  "A Node" should {
    "expose the appropriate properties" in {
      val cases = List(
        (Node.blank, true, false, false),
        (Node.iri("https://a.b").right.value, false, true, false),
        (Node.literal(2), false, false, true)
      )
      forAll(cases) {
        case (node, isBlank, isIri, isLiteral) =>
          node.isBlank shouldEqual isBlank
          node.isIri shouldEqual isIri
          node.isLiteral shouldEqual isLiteral
          node.asBlank.isDefined shouldEqual isBlank
          node.asIri.isDefined shouldEqual isIri
          node.asLiteral.isDefined shouldEqual isLiteral
      }
    }
//    "show" in {
//      val cases = List[(Node, String)](
//        (Node.blank("123").right.value, "_:123"),
//        (Node.iri("https://a.b").right.value, "https://a.b"),
//        (Node.literal(2), """"2"^^http://www.w3.org/2001/XMLSchema#integer"""),
//        (Node.literal("a"), """"a""""),
//        (Node.literal("a", LanguageTag("en").right.value), """"a"@en""")
//      )
//      forAll(cases) {
//        case (node: Node, str) =>
//          pass
//      }
//    }
  }

  "A Literal" should {
    "be numeric" in {
      val cases = List(
        Node.literal(1),
        Node.literal(1.toByte),
        Node.literal(0.1.toFloat),
        Node.literal(0.1),
        Node.literal(1.toLong),
        Node.literal(1.toShort)
      )
      forAll(cases) { el =>
        el.isNumeric shouldEqual true
      }
    }
    "be string" in {
      Node.literal("a").isString shouldEqual true
      Node.literal("a", LanguageTag("en").right.value).isString shouldEqual true
    }
  }

}
