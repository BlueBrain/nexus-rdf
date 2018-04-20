package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class ComponentSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "A R or Q Component" should {
    "be constructed successfully" in {
      val cases = List(
        "a"                     -> "a",
        "a%C2%A3/?:@;&b%C3%86c" -> "a£/?:@;&bÆc",
        "a£/?:@;&bÆc"           -> "a£/?:@;&bÆc"
      )
      forAll(cases) {
        case (str, expected) =>
          Component(str).right.value.value shouldEqual expected
      }
    }
    "fail to construct" in {
      val strings = List("", "asd?=", "asd?+", "asd?=a", "asd?+a")
      forAll(strings) { s =>
        Component(s).left.value
      }
    }
    "show" in {
      Component("a%C2%A3/?:@;&b%C3%86c").right.value.show shouldEqual "a£/?:@;&bÆc"
    }
    "eq" in {
      val lhs = Component("a%C2%A3/?:@;&b%C3%86c").right.value
      val rhs = Component("a£/?:@;&bÆc").right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }
  }
}
