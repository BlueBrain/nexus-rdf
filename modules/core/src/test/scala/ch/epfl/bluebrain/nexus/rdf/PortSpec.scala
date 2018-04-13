package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class PortSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "A Port" should {
    "be constructed successfully" in {
      val strings = List("0", "1", "10", "65535", "60000")
      forAll(strings) { s =>
        Port(s).right.value
      }
    }
    "be range checked" in {
      val ints = List(-1, 65536)
      forAll(ints) { i =>
        Port(i).left.value
      }
    }
    "fail to construct" in {
      val strings = List("", "00", "01", "-1", "65536")
      forAll(strings) { s =>
        Port(s).left.value
      }
    }
    "show" in {
      Port(1).right.value.show shouldEqual "1"
    }
    "eq" in {
      Eq.eqv(Port("1").right.value, Port(1).right.value) shouldEqual true
    }
  }
}
