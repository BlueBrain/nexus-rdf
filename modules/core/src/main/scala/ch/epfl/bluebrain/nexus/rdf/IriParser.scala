package ch.epfl.bluebrain.nexus.rdf

import org.parboiled2.CharPredicate._
import org.parboiled2.{CharPredicate, Parser, ParserInput, Rule1}
import org.parboiled2.CharUtils._

//noinspection TypeAnnotation
// format: off
class IriParser(val input: ParserInput) extends Parser {

  def _scheme = rule {
    capture(Alpha ~ zeroOrMore(AlphaNum ++ "+-.")) ~> ((str: String) => new Scheme(str.toLowerCase))
  }

  def `scheme` = rule { _scheme ~ EOI }

  val Digit04 = CharPredicate('0' to '4')
  val Digit05 = CharPredicate('0' to '5')

  def `dec-octet` = rule {
    capture(
      (ch('2') ~ ((Digit04 ~ Digit) | (ch('5') ~ Digit05)))
        | (ch('1') ~ Digit ~ Digit)
        | (Digit19 ~ Digit)
        | Digit
    ) ~> ((str: String) => str.toInt.toByte)
  }

  def _ipv4Address = rule {
    `dec-octet` ~ "." ~ `dec-octet` ~ "." ~ `dec-octet` ~ "." ~ `dec-octet` ~> (IPv4Host(_, _, _, _))
  }

  def `IPv4Address` = rule { _ipv4Address ~ EOI }

  val ucscharRanges = Set(
    0xA0    to 0xD7FF,  0xF900  to 0xFDCF,  0xFDF0  to 0xFFEF,
    0x10000 to 0x1FFFD, 0x20000 to 0x2FFFD, 0x30000 to 0x3FFFD,
    0x40000 to 0x4FFFD, 0x50000 to 0x5FFFD, 0x60000 to 0x6FFFD,
    0x70000 to 0x7FFFD, 0x80000 to 0x8FFFD, 0x90000 to 0x9FFFD,
    0xA0000 to 0xAFFFD, 0xB0000 to 0xBFFFD, 0xC0000 to 0xCFFFD,
    0xD0000 to 0xDFFFD, 0xE1000 to 0xEFFFD
  )
  val iprivateRanges = Set(0xE000 to 0xF8FF, 0xF0000 to 0xFFFFD, 0x100000 to 0x10FFFD)

  def `sub-delims` = CharPredicate("!$&'()*+,;=")
  def `ucschar` = CharPredicate.from(ch => ucscharRanges.exists(_.contains(ch.toInt)))
  def `iunreserved` = AlphaNum ++ CharPredicate("-._~") ++ `ucschar`

  def `pct-encoded` = rule {
    ch('%') ~ capture(HexDigit) ~ capture(HexDigit) ~> ((c1, c2) => (hexValue(c1.charAt(0)) * 16 + hexValue(c2.charAt(0))).toByte)
  }

  def _pctEncoded: Rule1[String] = rule {
    oneOrMore(`pct-encoded`) ~> ((seq: Seq[Byte]) => new String(seq.toArray, "UTF-8"))
  }

  def _iRegName: Rule1[NamedHost] = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved`))) ~> ((seq: Seq[String]) => new NamedHost(seq.mkString.toLowerCase))
  }

  def `ireg-name` = rule { _iRegName ~ EOI }
}
