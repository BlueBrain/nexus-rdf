package ch.epfl.bluebrain.nexus.rdf

import ch.epfl.bluebrain.nexus.rdf.Iri.Host.{IPv4Host, NamedHost}
import ch.epfl.bluebrain.nexus.rdf.Iri._
import ch.epfl.bluebrain.nexus.rdf.Iri.Path._
import org.parboiled2.CharPredicate._
import org.parboiled2.CharUtils._
import org.parboiled2.{CharPredicate, Parser, ParserInput, Rule1}

import scala.collection.immutable.{SortedMap, SortedSet}

//noinspection TypeAnnotation
// format: off
@SuppressWarnings(Array("MethodNames"))
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
  def `iprivate`: CharPredicate = CharPredicate.from(ch => iprivateRanges.exists(_.contains(ch.toInt)))

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

  def _port = rule {
    capture((Digit19 ~ (1 to 4).times(Digit).?) | ch('0')) ~> ((str: String) => Integer.parseInt(str))
  }

  def `port` = rule { _port ~ EOI ~> (p => Port(p)) }

  def _userInfo = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ ':'))) ~> ((seq: Seq[String]) => new UserInfo(seq.mkString))
  }

  def `iuserinfo` = rule { _userInfo ~ EOI }

  def _pathAbEmpty: Rule1[Path] = rule {
    zeroOrMore(ch('/') ~ `isegment`) ~> ((seq: Seq[String]) => seq.foldLeft[Path](Path.Empty) {
      case (acc, el) if el.length == 0 => Slash(acc)
      case (acc, el)                   => Segment(el, Slash(acc))
    })
  }

  def `ipath-abempty`: Rule1[Path] = rule { _pathAbEmpty ~ EOI }

//  def _pathAbsolute: Rule1[Path] = rule {
//    (ch('/') ~ `isegment-nz` ~ zeroOrMore(ch('/') ~ `isegment`)) ~> ((str: String, seq: Seq[String]) => seq.foldLeft[Path](Segment(str, Slash(Empty))) {
//      case (acc, el) if el.length == 0 => Slash(acc)
//      case (acc, el)                   => Segment(el, Slash(acc))
//    })
//  }
//
//  def `ipath-absolute`: Rule1[Path] = rule { _pathAbsolute ~ EOI }

//  def `ipath-noscheme` = rule {
//    `isegment-nz-nc` ~ `ipath-abempty` ~> ((str, path) => Segment(str, Slash(path)))
//  }

  def `isegment`: Rule1[String] = rule {
    zeroOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ ':' ++ '@'))) ~> ((seq: Seq[String]) => seq.mkString)
  }

//  def `isegment-nz`: Rule1[String] = rule {
//    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ ':' ++ '@'))) ~> ((seq: Seq[String]) => seq.mkString)
//  }
//
//  def `isegment-nz-nc`: Rule1[String] = rule {
//    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ '@'))) ~> ((seq: Seq[String]) => seq.mkString)
//  }

  def _query: Rule1[Query] = rule {
    oneOrMore(_queryElement).separatedBy('&') ~> ((seq: Seq[(String, String)]) => {
      val map = seq.groupBy(_._1).mapValues(e => SortedSet(e.map(_._2): _*))
      Query(SortedMap(map.toList: _*))
    })
  }

  def _queryElement: Rule1[(String, String)] = rule {
    _queryElementPart ~ optional(ch('=') ~ _queryElementPart) ~> ((str: String, strOpt: Option[String]) => (str, strOpt.getOrElse("")))
  }

  def _queryElementPart: Rule1[String] = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ `iprivate` ++ CharPredicate(":@/?") -- CharPredicate("=&")))) ~> ((seq: Seq[String]) => seq.mkString)
  }

  def `iquery` = rule { _query ~ EOI }

  def _fragment: Rule1[Fragment] = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ CharPredicate(":@/?")))) ~> ((seq: Seq[String]) => new Fragment(seq.mkString))
  }

  def `ifragment` = rule { _fragment ~ EOI }

}
// format: on
