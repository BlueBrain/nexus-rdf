package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import org.scalatest.{EitherValues, Inspectors, Matchers, WordSpecLike}

class HostSpec extends WordSpecLike with Matchers with Inspectors with EitherValues {

  "An IPv4Host" should {
    val one = Host.ipv4("1.1.1.1").right.value
    "be parsed correctly from string" in {
      val values = List("127.0.0.1", "255.255.255.255", "199.99.9.0", "249.249.249.249")
      forAll(values) { v =>
        Host.ipv4(v).right.value.show shouldEqual v
      }
    }
    "be constructed correctly from bytes" in {
      Host.ipv4(1, 1, 1, 1).show shouldEqual one.show
    }
    "be constructed correctly from array" in {
      Host.ipv4(Array.fill[Byte](4)(1)).right.value.show shouldEqual one.show
    }
    "fail to construct from incorrect array length" in {
      forAll(List(Array.fill[Byte](3)(1), Array.fill[Byte](5)(1))) { arr =>
        Host.ipv4(arr).left.value should not be 'empty
      }
    }
    "fail to parse from string" in {
      val values = List(
        "1",
        "1.1.1",
        "1.1.1.",
        "1..1.1",
        "a.b.c.d",
        "01.1.1.1",
        "1.01.1.1",
        "1.1.01.1",
        "1.1.1.01",
        "1.1.1.1a",
        "256.255.255.255",
        "255.256.255.255",
        "255.255.256.255",
        "255.255.255.256",
        "355.255.255.255",
        "260.255.255.256"
      )
      forAll(values) { v =>
        Host.ipv4(v).left.value should not be 'empty
      }
    }
    "be an IPv4 address" in {
      one.isIPv4 shouldEqual true
    }
    "return itself as IPv4 address" in {
      one.asIPv4 shouldEqual Some(one)
    }
    "not be an IPv6 address" in {
      one.isIPv6 shouldEqual false
    }
    "not be a named host" in {
      one.isNamed shouldEqual false
    }
    "not return an IPv6Host" in {
      one.asIPv6 shouldEqual None
    }
    "not return a NamedHost" in {
      one.asNamed shouldEqual None
    }
    "show" in {
      one.show shouldEqual "1.1.1.1"
    }
    "eq" in {
      Eq.eqv(Host.ipv4("1.1.1.1").right.value, one) shouldEqual true
    }
  }

  "An IPv6Host" should {
    val one       = Host.ipv6(Array.fill(16)(1.toByte))
    val oneString = "101:101:101:101:101:101:101:101"
    "be constructed correctly from array" in {
      one.right.value.show shouldEqual oneString
    }
    "be rendered correctly as mixed ipv4/ipv6" in {
      one.right.value.asMixedString shouldEqual "101:101:101:101:101:101:1.1.1.1"
    }
    "fail to construct from incorrect array length" in {
      forAll(List(Array.fill(17)(1.toByte), Array.fill(3)(1.toByte), Array.fill(15)(1.toByte))) { arr =>
        Host.ipv6(arr).left.value should not be 'empty
      }
    }
    "be an IPv6 address" in {
      one.right.value.isIPv6 shouldEqual true
    }
    "return itself as IPv6 address" in {
      one.right.value.asIPv6 shouldEqual Some(one.right.value)
    }
    "not be an IPv4 address" in {
      one.right.value.isIPv4 shouldEqual false
    }
    "not be a named host" in {
      one.right.value.isNamed shouldEqual false
    }
    "not return an IPv4Host" in {
      one.right.value.asIPv4 shouldEqual None
    }
    "not return a NamedHost" in {
      one.right.value.asNamed shouldEqual None
    }
    "show" in {
      one.right.value.show shouldEqual oneString
    }
    "eq" in {
      Eq.eqv(Host.ipv6(Array.fill(16)(1.toByte)).right.value, one.right.value) shouldEqual true
    }
  }
}
