package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import ch.epfl.bluebrain.nexus.rdf.Iri.Path._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class PathSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "A Path" should {
    val abcd = Path("/a/b//c/d")
    "be parsed in the correct ADT" in {
      // format: off
      val cases = List(
        ""                        -> Empty,
        "/"                       -> Slash(Empty),
        "///"                     -> Slash(Slash(Slash(Empty))),
        "/a/b//c/d"               -> Segment("d", Slash(Segment("c", Slash(Slash(Segment("b", Slash(Segment("a", Slash(Empty))))))))),
        "/a/b//c//"               -> Slash(Slash(Segment("c", Slash(Slash(Segment("b", Slash(Segment("a", Slash(Empty))))))))),
        "/a/b//:@//"              -> Slash(Slash(Segment(":@", Slash(Slash(Segment("b", Slash(Segment("a", Slash(Empty))))))))),
        "/a/b//:://"              -> Slash(Slash(Segment("::", Slash(Slash(Segment("b", Slash(Segment("a", Slash(Empty))))))))),
        "/a/b/%20/:://"           -> Slash(Slash(Segment("::", Slash(Segment(" ", Slash(Segment("b", Slash(Segment("a", Slash(Empty)))))))))),
        "/a£/bÆc//:://"           -> Slash(Slash(Segment("::", Slash(Slash(Segment("bÆc", Slash(Segment("a£", Slash(Empty))))))))),
        "/a%C2%A3/b%C3%86c//:://" -> Slash(Slash(Segment("::", Slash(Slash(Segment("bÆc", Slash(Segment("a£", Slash(Empty)))))))))
      )
      // format: on
      forAll(cases) {
        case (str, expected) =>
          Path(str).right.value shouldEqual expected
      }
    }
    "fail to construct for invalid chars" in {
      val cases = List("/a/b?", "abc", "/a#", ":asd", " ")
      forAll(cases) { c =>
        Path(c).left.value
      }
    }
    "return the correct information about the internal structure" in {
      val cases = List(
        ("", true, false, false, Some(Empty), None, None),
        ("/", false, true, false, None, Some(Slash(Empty)), None),
        ("/a/", false, true, false, None, Some(Slash(Segment("a", Slash(Empty)))), None),
        ("/a", false, false, true, None, None, Some(Segment("a", Slash(Empty))))
      )
      forAll(cases) {
        case (str, isEmpty, isSlash, isSegment, asEmpty, asSlash, asSegment) =>
          val p = Path(str).right.value
          p.isEmpty shouldEqual isEmpty
          p.isSlash shouldEqual isSlash
          p.isSegment shouldEqual isSegment
          p.asEmpty shouldEqual asEmpty
          p.asSlash shouldEqual asSlash
          p.asSegment shouldEqual asSegment
      }
    }
    "show" in {
      abcd.right.value.show shouldEqual "/a/b//c/d"
    }
    "show decoded" in {
      Path("/a%C2%A3/b%C3%86c//:://").right.value.show shouldEqual "/a£/bÆc//:://"
    }
    "eq" in {
      Eq.eqv(abcd.right.value, Segment("d", Path("/a/b//c/").right.value)) shouldEqual true
    }
  }
}
