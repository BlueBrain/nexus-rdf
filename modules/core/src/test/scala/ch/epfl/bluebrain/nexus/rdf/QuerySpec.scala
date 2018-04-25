package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

import scala.collection.immutable.{SortedMap, SortedSet}

class QuerySpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "A Query" should {
    "be constructed successfully" in {
      // format: off
      val cases = List(
        "" -> SortedMap.empty[String, SortedSet[String]],
        "a=b&a=b&a=c&a&b&b&b=c&d/&e?" -> SortedMap(
          "a" -> SortedSet("", "b", "c"),
          "b" -> SortedSet("", "c"),
          "d/" -> SortedSet(""),
          "e?" -> SortedSet("")),
        "%3D%26=%3D%26&%3D&%26" -> SortedMap(
          "=&" -> SortedSet("=&"),
          "="  -> SortedSet(""),
          "&"  -> SortedSet("")),
        "%C2%A3=%C3%86" -> SortedMap("£" -> SortedSet("Æ"))
      )
      // format: on
      forAll(cases) {
        case (raw, map) =>
          Query(raw).right.value.value shouldEqual map
      }
    }
    "fail to parse" in {
      val cases = List("a==b", "a=b&", "a#", "a&&", "a=&b")
      forAll(cases) { str =>
        Query(str).left.value
      }
    }
    "show" in {
      Query("a=b&a=b&a=c&a&b&b&b=c&d&e").right.value.show shouldEqual "a&a=b&a=c&b&b=c&d&e"
    }
    "eq" in {
      val lhs = Query("a=b&a=b&a=c&a&b&b&b=c&d&e").right.value
      val rhs = Query("a=b&a=b&a=c&a&b&b=c&d&e").right.value
      Eq.eqv(lhs, rhs) shouldEqual true
    }
  }
}
