package ch.epfl.bluebrain.nexus.rdf

import ch.epfl.bluebrain.nexus.rdf.Curie.Prefix
import ch.epfl.bluebrain.nexus.rdf.Iri.Host.{IPv4Host, NamedHost}
import ch.epfl.bluebrain.nexus.rdf.Iri._
import ch.epfl.bluebrain.nexus.rdf.Iri.Path._
import org.parboiled2.CharPredicate._
import org.parboiled2.CharUtils._
import org.parboiled2._

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
    zeroOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved`))) ~> ((seq: Seq[String]) => new NamedHost(seq.mkString.toLowerCase))
  }

  def `ireg-name` = rule { _iRegName ~ EOI }

  def _portLiteral = rule {
    capture((Digit19 ~ (1 to 4).times(Digit).?) | ch('0')) ~> ((str: String) => Integer.parseInt(str))
  }

  def _port = rule {
    _portLiteral ~> (up => test(Port(up).isRight) ~ push(new Port(up)))
  }

  def `port` = rule { _port ~ EOI }

  def _userInfo = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ ':'))) ~> ((seq: Seq[String]) => new UserInfo(seq.mkString))
  }

  def `iuserinfo` = rule { _userInfo ~ EOI }

  private val pathF = (p: Path, e: String)  => (p, e) match {
    case (Segment(_, Slash(acc)), "..")     => acc
    case (Slash(acc), "..")                 => acc
    case (acc, "..")                        => acc
    case (acc, ".")                         => acc
    case (acc, el) if el.length == 0        => Slash(acc)
    case (acc, el)                          => Segment(el, Slash(acc))
  }

  def _pathAbEmpty: Rule1[Path] = rule {
    zeroOrMore(ch('/') ~ `isegment`) ~> ((seq: Seq[String]) => seq.foldLeft[Path](Path.Empty)(pathF))
  }

  def `ipath-abempty`: Rule1[Path] = rule { _pathAbEmpty ~ EOI }

  def _pathRootless: Rule1[Path] = rule {
    `isegment-nz` ~ zeroOrMore(ch('/') ~ `isegment`) ~> ((str: String, seq: Seq[String]) => seq.foldLeft[Path](Segment(str, Path.Empty)) {
      case (acc, el) if el.length == 0 => Slash(acc)
      case (acc, el)                   => Segment(el, Slash(acc))
    })
  }

  def relativeWithAuthority: Rule1[(Option[Authority], Path)] = rule {
    '/' ~ '/' ~ _authority ~ _pathAbEmpty ~> ((a: Authority, p: Path) => Some(a) -> p)
  }

  def relativeIpathAbsolute: Rule1[(Option[Authority], Path)] = rule {
    _pathAbsolute ~> ((p: Path) => None -> p)
  }

  def relativeIpathNoScheme: Rule1[(Option[Authority], Path)] = rule {
    `ipath-noscheme` ~> ((p: Path) => None -> p)
  }

  def `irelative-part`: Rule1[(Option[Authority], Path)] = rule {
    optional(relativeWithAuthority | relativeIpathAbsolute | relativeIpathNoScheme) ~> ((opt: Option[(Option[Authority], Path)]) => opt.getOrElse((None, Path.Empty)))
  }

  def _irelativeRef: Rule1[RelativeIri] = rule {
    `irelative-part` ~ optional(ch('?') ~ _query) ~ optional(ch('#') ~ _fragment) ~> {
      (t: (Option[Authority], Path), q: Option[Query], f: Option[Fragment]) =>
        val (auth, path) = t
        test(path.nonEmpty || auth.isDefined || q.isDefined || f.isDefined) ~ push(RelativeIri(auth, path, q, f))
    }
  }

  def `irelative-ref`: Rule1[RelativeIri] = rule { _irelativeRef ~ EOI }

  private def _pathAbsoluteStart(optString: Option[String]): Path = optString.map(Segment(_, Slash(Path.Empty))).getOrElse(Slash(Path.Empty))

  def _pathAbsolute: Rule1[Path] = rule {
    (ch('/') ~ optional(`isegment-nz`) ~ optional(zeroOrMore(ch('/') ~ `isegment`))) ~> ((str: Option[String], seq: Option[Seq[String]]) => seq.getOrElse(Seq.empty).foldLeft[Path](_pathAbsoluteStart(str))(pathF))
  }

  def `ipath-noscheme` = rule {
    `isegment-nz-nc` ~ zeroOrMore(ch('/') ~ `isegment`) ~> ((str: String, seq: Seq[String]) => seq.foldLeft[Path](Segment(str, Path.Empty)) {
      case (Segment(el, Slash(acc)), "..") if el != ".."  => acc
      case (Segment(el, acc), "..") if el != ".."         => acc
      case (Slash(acc), "..")                             => acc
      case (acc, ".")                                     => acc
      case (acc, el) if el.length == 0                    => Slash(acc)
      case (acc, el)                                      => Segment(el, Slash(acc))
    })
  }

  def `isegment`: Rule1[String] = rule {
    zeroOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ ':' ++ '@'))) ~> ((seq: Seq[String]) => seq.mkString)
  }

  def `isegment-nz`: Rule1[String] = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ ':' ++ '@'))) ~> ((seq: Seq[String]) => seq.mkString)
  }

  def `isegment-nz-nc`: Rule1[String] = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ '@'))) ~> ((seq: Seq[String]) => seq.mkString)
  }

  private val queryFold = (seq: Seq[(String, String)]) => {
    val map = seq.groupBy(_._1).mapValues(e => SortedSet(e.map(_._2): _*))
    Query(SortedMap(map.toList: _*))
  }

  def _query: Rule1[Query] = rule {
    zeroOrMore(_queryElement).separatedBy('&') ~> queryFold
  }

  def _queryElement: Rule1[(String, String)] = rule {
    _queryElementPart ~ optional(ch('=') ~ _queryElementPart) ~> ((str: String, strOpt: Option[String]) => (str, strOpt.getOrElse("")))
  }

  def _queryElementPart: Rule1[String] = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(!"?+" ~ (`sub-delims` ++ `iunreserved` ++ `iprivate` ++ CharPredicate(":@/?") -- CharPredicate("=&"))))) ~> ((seq: Seq[String]) => seq.mkString)
  }

  def `iquery` = rule { _query ~ EOI }

  def _fragment: Rule1[Fragment] = rule {
    zeroOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ CharPredicate(":@/?")))) ~> ((seq: Seq[String]) => new Fragment(seq.mkString))
  }

  def `ifragment` = rule { _fragment ~ EOI }

  def _host: Rule1[Host] = rule {
    _ipv4Address | _iRegName
  }

  def _authority: Rule1[Authority] = rule {
    optional(_userInfo ~ ch('@')) ~ _host ~ optional(ch(':') ~ _port) ~> ((ui, h, p) => Authority(ui, h, p))
  }

  def authorityAndPath: Rule1[(Option[Authority], Path)] = rule {
    "//" ~ _authority ~ optional(_pathAbEmpty)  ~> ((a: Authority, p: Option[Path]) => Some(a) -> p.getOrElse(Path.Empty))
  }

  def otherPaths: Rule1[(Option[Authority], Path)] = rule {
    (_pathAbsolute | _pathRootless) ~> ((p: Path) => None -> p)
  }

  def `hier-part`: Rule1[(Option[Authority], Path)] = rule { authorityAndPath | otherPaths }

  def _url: Rule1[Url] = rule {
    _scheme ~ ch(':') ~ `hier-part` ~ optional(ch('?') ~ _query) ~ optional(ch('#') ~ _fragment) ~> {
      (s: Scheme, ap: (Option[Authority],Path), q: Option[Query], f: Option[Fragment]) =>
        val (a, p) = ap
        Url(s, a, p, q, f)
    }
  }

  def url: Rule1[Url] = rule { _url ~ EOI }

  val ldh = AlphaNum ++ '-'

  def _nid: Rule1[Nid] = rule {
    capture(AlphaNum ~ (1 to 31).times(ldh)) ~> ((chars: String) => test(lastChar.isLetterOrDigit) ~ push(new Nid(chars.toLowerCase)))
  }

  def `nid`: Rule1[Nid] = rule { _nid ~ EOI }

  def _nss: Rule1[Path] = rule { _pathRootless }

  def `nss`: Rule1[Path] = rule { _nss ~ EOI }

  def _component: Rule1[Component] = rule {
    oneOrMore(_pctEncoded | capture(oneOrMore(`sub-delims` ++ `iunreserved` ++ ":@/")) | capture('?' ~ &(!(ch('+') | '=')))) ~> ((seq: Seq[String]) => new Component(seq.mkString))
  }

  def `component`: Rule1[Component] = rule { _component ~ EOI }

  def _rFollowedByOptQ: Rule1[(Option[Component], Option[Query])] = rule {
    "?+" ~ _component ~ optional("?=" ~ _queryNonEmpty) ~> ((c: Component, q: Option[Query]) => Some(c) -> q)
  }

  def _queryNonEmpty: Rule1[Query] = rule {
    oneOrMore(_queryElement).separatedBy('&') ~> queryFold
  }

  def _qFollowedByOptR: Rule1[(Option[Component], Option[Query])] = rule {
    "?=" ~ _queryNonEmpty ~ optional("?+" ~ _component) ~> ((q: Query, r: Option[Component]) => r -> Some(q))
  }

  def _rqComponents: Rule1[(Option[Component], Option[Query])] = rule {
    (_rFollowedByOptQ | _qFollowedByOptR).? ~> ((res: Option[(Option[Component], Option[Query])]) => res match {
      case Some(v) => v
      case None    => (None, None)
    })
  }

  def _urn: Rule1[Urn] = rule {
    "urn:" ~ _nid ~ ":" ~ _nss ~ _rqComponents ~ optional('#' ~ _fragment) ~> ((nid: Nid, nss: Path, rq: (Option[Component], Option[Query]), f: Option[Fragment]) => Urn(nid, nss, rq._1, rq._2, f))
  }

  def urn: Rule1[Urn] = rule { _urn ~ EOI }

  private val nameStartUtfRanges = Set(
    0xC0 to 0xD6,     0xD8 to 0xF6,     0xF8 to 0x2FF,
    0x370 to 0x37D,   0x37F to 0x1FFF,  0x200C to 0x200D,
    0x2070 to 0x218F, 0x2C00 to 0x2FEF, 0x300 to 0xD7FF,
    0xF900 to 0xFDCF, 0xFDF0 to 0xFFFD, 0x10000 to 0xEFFFF
  )
  private val nameStartUtf = CharPredicate.from(ch => nameStartUtfRanges.exists(_.contains(ch.toInt)))

  private def nameStart = rule { capture(LowerAlpha | UpperAlpha | ch('_') | nameStartUtf) }
  private def name = rule {
    nameStart | capture(ch('-') | ch('.') | Digit | ch('\u00B7') | CharPredicate('\u0300' to '\u036F') | CharPredicate('\u203F' to '\u2040'))
  }
  private def ncName = rule {
    nameStart ~ zeroOrMore(name) ~> ((s: String, c: Seq[String]) => new Prefix(s"$s${c.mkString}"))
  }

  def `prefix`: Rule1[Prefix] = rule { ncName ~ EOI }

  def _curie: Rule1[Curie] = rule {
    ncName ~ ch(':') ~ _irelativeRef ~> ((p: Prefix, r: RelativeIri) => new Curie(p, r))
  }

  def `curie`: Rule1[Curie] = rule { _curie ~ EOI }

}
// format: on
