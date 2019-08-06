package ch.epfl.bluebrain.nexus.rdf

import cats.kernel.Eq
import cats.syntax.show._
import ch.epfl.bluebrain.nexus.rdf.Iri._
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

  "A NamedHost" should {
    val pct =
      "%C2%A3%C2%A4%C2%A5%C2%A6%C2%A7%C2%A8%C2%A9%C2%AA%C2%AB%C2%AC%C2%AD%C2%AE%C2%AF%C2%B0%C2%B1%C2%B2%C2%B3%C2%B4%C2%B5%C2%B6%C2%B7%C2%B8%C2%B9%C2%BA%C2%BB%C2%BC%C2%BD%C2%BE%C2%BF%C3%80%C3%81%C3%82%C3%83%C3%84%C3%85%C3%86"
    val pctLow =
      "%C2%A3%C2%A4%C2%A5%C2%A6%C2%A7%C2%A8%C2%A9%C2%AA%C2%AB%C2%AC%C2%AD%C2%AE%C2%AF%C2%B0%C2%B1%C2%B2%C2%B3%C2%B4%C2%B5%C2%B6%C2%B7%C2%B8%C2%B9%C2%BA%C2%BB%C2%BC%C2%BD%C2%BE%C2%BF%C3%A0%C3%A1%C3%A2%C3%A3%C3%A4%C3%A5%C3%A6"
    val ucsUp  = "£¤¥¦§¨©ª«¬\u00AD®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆ"
    val ucsLow = "£¤¥¦§¨©ª«¬\u00AD®¯°±²³´µ¶·¸¹º»¼½¾¿àáâãäåæ"
    val delims = "!$&'()*+,;="
    val up     = "ABCD"
    val low    = "abcd"

    "be parsed correctly from a string" in {
      Host.named("epfl.ch").right.value.value shouldEqual "epfl.ch"
    }

    "be parsed correctly from percent encoded string" in {
      Host.named(pct).right.value.value shouldEqual ucsLow
    }

    "be parsed correctly from ucs chars" in {
      Host.named(ucsUp).right.value.value shouldEqual ucsLow
    }

    "be parsed correctly from delimiters" in {
      Host.named(delims).right.value.value shouldEqual delims
    }

    "be parsed correctly from mixed characters" in {
      Host.named(ucsUp + pct + delims + up).right.value.value shouldEqual (ucsLow + ucsLow + delims + low)
    }

    "show" in {
      val encodedDelim = urlEncode("[]#")
      Host.named(up + encodedDelim).right.value.show shouldEqual (low + encodedDelim)
    }

    "pct encoded representation" in {
      val encodedDelim = urlEncode("[]#")
      Host.named(ucsUp + pct + ucsLow + delims + up + encodedDelim).right.value.pctEncoded shouldEqual (pctLow + pctLow + pctLow + delims + low+ encodedDelim)
    }

    "be named" in {
      Host.named(up).right.value.isNamed shouldEqual true
    }

    "return an optional self" in {
      Host.named(up).right.value.asNamed shouldEqual Some(Host.named(up).right.value)
    }

    "eq" in {
      Eq.eqv(Host.named(ucsUp).right.value, Host.named(ucsLow).right.value) shouldEqual true
    }
  }
}
