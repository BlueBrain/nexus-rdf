package ch.epfl.bluebrain.nexus.rdf

import cats.syntax.either._
import cats.{Eq, Show}
import ch.epfl.bluebrain.nexus.rdf.Iri.Host.{IPv4Host, IPv6Host, NamedHost}
import ch.epfl.bluebrain.nexus.rdf.Iri.Path._
import ch.epfl.bluebrain.nexus.rdf.Iri._
import org.parboiled2.ErrorFormatter
import org.parboiled2.Parser.DeliveryScheme.{Either => E}

import scala.collection.immutable.SortedSet
import scala.collection.{immutable, SeqView, SortedMap}

/**
  * An Iri as defined by RFC 3987.
  */
sealed abstract class Iri extends Product with Serializable {

  /**
    * @return true if this Iri is absolute, false otherwise
    */
  def isAbsolute: Boolean

  /**
    * @return true if this Iri is relative, false otherwise
    */
  def isRelative: Boolean = !isAbsolute

  /**
    * @return Some(this) if this Iri is absolute, None otherwise
    */
  def asAbsolute: Option[AbsoluteIri]

  /**
    * @return Some(this) if this Iri is an Url, None otherwise
    */
  def asUrl: Option[Url]
}

object Iri {

  /**
    * Attempt to construct a new Url from the argument validating the structure and the character encodings as per
    * RFC 3987.
    *
    * @param string the string to parse as an absolute url.
    * @return Right(url) if the string conforms to specification, Left(error) otherwise
    */
  final def url(string: String): Either[String, Url] =
    Url(string)

  /**
    * An absolute Iri as defined by RFC 3987.
    */
  sealed abstract class AbsoluteIri extends Iri {
    override def isAbsolute: Boolean             = true
    override def asAbsolute: Option[AbsoluteIri] = Some(this)
    override def asUrl: Option[Url]              = None
  }

  /**
    * An absolute Url.
    *
    * @param scheme    the scheme part
    * @param authority the authority part
    * @param path      the path part
    * @param query     an optional query part
    * @param fragment  an optional fragment part
    */
  final case class Url(
      scheme: Scheme,
      authority: Authority,
      path: Path,
      query: Option[Query],
      fragment: Option[Fragment]
  ) extends AbsoluteIri {
    override def asUrl: Option[Url] = Some(this)
  }

  object Url {

    private val defaultSchemePortMapping: Map[Scheme, Port] = Map(
      "ftp"    -> 21,
      "ssh"    -> 22,
      "telnet" -> 23,
      "smtp"   -> 25,
      "domain" -> 53,
      "tftp"   -> 69,
      "http"   -> 80,
      "ws"     -> 80,
      "pop3"   -> 110,
      "nntp"   -> 119,
      "imap"   -> 143,
      "snmp"   -> 161,
      "ldap"   -> 389,
      "https"  -> 443,
      "wss"    -> 443,
      "imaps"  -> 993,
      "nfs"    -> 2049
    ).map { case (s, p) => (new Scheme(s), new Port(p)) }

    private def normalize(scheme: Scheme, authority: Authority): Authority =
      if (authority.port == defaultSchemePortMapping.get(scheme)) authority.copy(port = None) else authority

    /**
      * Constructs an Url from its constituents.
      *
      * @param scheme    the scheme part
      * @param authority the authority part
      * @param path      the path part
      * @param query     an optional query part
      * @param fragment  an optional fragment part
      */
    final def apply(
        scheme: Scheme,
        authority: Authority,
        path: Path,
        query: Option[Query],
        fragment: Option[Fragment]
    ): Url = new Url(scheme, normalize(scheme, authority), path, query, fragment)

    /**
      * Attempt to construct a new Url from the argument validating the structure and the character encodings as per
      * RFC 3987.
      *
      * @param string the string to parse as an absolute url.
      * @return Right(url) if the string conforms to specification, Left(error) otherwise
      */
    final def apply(string: String): Either[String, Url] =
      new IriParser(string).url
        .run()
        .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

    final implicit def urlShow(implicit s: Show[Scheme],
                               a: Show[Authority],
                               p: Show[Path],
                               q: Show[Query],
                               f: Show[Fragment]): Show[Url] = Show.show { url =>
      import cats.syntax.show._
      val query = url.query match {
        case Some(v) => "?" + v.show
        case _       => ""
      }
      val fragment = url.fragment match {
        case Some(v) => "#" + v.show
        case _       => ""
      }
      s"${url.scheme.show}://${url.authority.show}${p.show(url.path)}$query$fragment"
    }

    final implicit val urlEq: Eq[Url] = Eq.fromUniversalEquals
  }

  /**
    * Scheme part of an Iri as defined by RFC 3987.
    *
    * @param value the string value of the scheme
    */
  final case class Scheme private[rdf] (value: String) {

    /**
      * Whether this scheme identifies an URN.
      */
    def isUrn: Boolean = value equalsIgnoreCase "urn"

    /**
      * Whether this scheme identifies an HTTPS Iri.
      */
    def isHttps: Boolean = value equalsIgnoreCase "https"

    /**
      * Whether this scheme identifies an HTTP Iri.
      */
    def isHttp: Boolean = value equalsIgnoreCase "http"
  }

  object Scheme {

    /**
      * Attempts to construct a scheme from the argument string value.  The provided value, if correct, will be normalized
      * such that:
      * {{{
      *   Scheme("HTTPS").right.get.value == "https"
      * }}}
      *
      * @param value the string representation of the scheme
      * @return Right(value) if successful or Left(error) if the string does not conform to the RFC 3987 format
      */
    final def apply(value: String): Either[String, Scheme] = {
      new IriParser(value).`scheme`
        .run()
        .leftMap(_.format(value, new ErrorFormatter(showExpected = false, showTraces = false)))
    }

    final implicit val schemeShow: Show[Scheme] = Show.show(_.value)
    final implicit val schemeEq: Eq[Scheme]     = Eq.fromUniversalEquals
  }

  /**
    * The Authority part of an IRI as defined by RFC 3987.
    *
    * @param userInfo the optional user info part
    * @param host     the host part
    * @param port     the optional port part
    */
  final case class Authority(userInfo: Option[UserInfo], host: Host, port: Option[Port])

  object Authority {

    final implicit def authorityShow(implicit u: Show[UserInfo], h: Show[Host], p: Show[Port]): Show[Authority] =
      Show.show { authority =>
        import cats.syntax.show._
        val userInfo = authority.userInfo match {
          case Some(v) => v.show + "@"
          case _       => ""
        }
        val port = authority.port match {
          case Some(v) => ":" + v.show
          case _       => ""
        }
        s"$userInfo${authority.host.show}$port"
      }

    final implicit val authorityEq: Eq[Authority] = Eq.fromUniversalEquals
  }

  /**
    * A user info representation as specified by RFC 3987.
    *
    * @param value the underlying string representation
    */
  final case class UserInfo private[rdf] (value: String) {

    /**
      * As per the specification the user info is case sensitive.  This method allows comparing two user info values
      * disregarding the character casing.
      *
      * @param that the user info to compare to
      * @return true if the underlying values are equal (diregarding their case), false otherwise
      */
    def equalsIgnoreCase(that: UserInfo): Boolean =
      this.value equalsIgnoreCase that.value
  }

  object UserInfo {

    /**
      * Attempt to construct a new UserInfo from the argument validating the character encodings as per RFC 3987.
      *
      * @param string the string to parse as a user info.
      * @return Right(UserInfo(value)) if the string conforms to specification, Left(error) otherwise
      */
    final def apply(string: String): Either[String, UserInfo] =
      new IriParser(string).`iuserinfo`
        .run()
        .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

    final implicit val userInfoShow: Show[UserInfo] = Show.show(_.value)
    final implicit val userInfoEq: Eq[UserInfo]     = Eq.fromUniversalEquals
  }

  /**
    * Host part of an Iri as defined in RFC 3987.
    */
  sealed abstract class Host extends Product with Serializable {

    /**
      * @return true if the host is an IPv4 address, false otherwise
      */
    def isIPv4: Boolean = false

    /**
      * @return true if the host is an IPv6 address, false otherwise
      */
    def isIPv6: Boolean = false

    /**
      * @return true if the host is a named host, false otherwise
      */
    def isNamed: Boolean = false

    /**
      * @return Some(this) if this is an IPv4Host, None otherwise
      */
    def asIPv4: Option[IPv4Host] = None

    /**
      * @return Some(this) if this is an IPv6Host, None otherwise
      */
    def asIPv6: Option[IPv6Host] = None

    /**
      * @return Some(this) if this is a NamedHost, None otherwise
      */
    def asNamed: Option[NamedHost] = None

    /**
      * @return a string representation of this host, as specified by RFC 3987.
      */
    def asString: String
  }

  object Host {

    /**
      * Constructs a new IPv4Host from the argument bytes.
      *
      * @param byte1 the first byte of the address
      * @param byte2 the second byte of the address
      * @param byte3 the third byte of the address
      * @param byte4 the fourth byte of the address
      * @return the IPv4Host represented by these bytes
      */
    final def ipv4(byte1: Byte, byte2: Byte, byte3: Byte, byte4: Byte): IPv4Host =
      IPv4Host(byte1, byte2, byte3, byte4)

    /**
      * Attempt to construct a new IPv4Host from its 32bit representation.
      *
      * @param bytes a 32bit IPv4 address
      * @return Right(IPv4Host(bytes)) if the bytes is a 4byte array, Left(error) otherwise
      */
    final def ipv4(bytes: Array[Byte]): Either[String, IPv4Host] =
      IPv4Host(bytes)

    /**
      * Attempt to construct a new IPv4Host from its string representation as specified by RFC 3987.
      *
      * @param string the string to parse as an IPv4 address.
      * @return Right(IPv4Host(bytes)) if the string conforms to specification, Left(error) otherwise
      */
    final def ipv4(string: String): Either[String, IPv4Host] =
      IPv4Host(string)

    /**
      * Attempt to construct a new IPv6Host from its 128bit representation.
      *
      * @param bytes a 128bit IPv6 address
      * @return Right(IPv6Host(bytes)) if the bytes is a 16byte array, Left(error) otherwise
      */
    def ipv6(bytes: Array[Byte]): Either[String, IPv6Host] =
      IPv6Host(bytes)

    /**
      * Attempt to construct a new NamedHost from the argument validating the character encodings as per RFC 3987.
      *
      * @param string the string to parse as a named host.
      * @return Right(NamedHost(value)) if the string conforms to specification, Left(error) otherwise
      */
    def named(string: String): Either[String, NamedHost] =
      NamedHost(string)

    /**
      * An IPv4 host representation as specified by RFC 3987.
      *
      * @param bytes the underlying bytes
      */
    final case class IPv4Host private[rdf] (bytes: immutable.Seq[Byte]) extends Host {
      override def isIPv4: Boolean          = true
      override def asIPv4: Option[IPv4Host] = Some(this)
      override lazy val asString: String    = bytes.map(_ & 0xFF).mkString(".")
    }

    object IPv4Host {

      /**
        * Attempt to construct a new IPv4Host from its 32bit representation.
        *
        * @param bytes a 32bit IPv4 address
        * @return Right(IPv4Host(bytes)) if the bytes is a 4byte array, Left(error) otherwise
        */
      final def apply(bytes: Array[Byte]): Either[String, IPv4Host] =
        Either
          .catchNonFatal(fromBytes(bytes))
          .leftMap(_ => "Illegal IPv4Host byte representation")

      /**
        * Attempt to construct a new IPv4Host from its string representation as specified by RFC 3987.
        *
        * @param string the string to parse as an IPv4 address.
        * @return Right(IPv4Host(bytes)) if the string conforms to specification, Left(error) otherwise
        */
      final def apply(string: String): Either[String, IPv4Host] =
        new IriParser(string).`IPv4Address`
          .run()
          .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

      /**
        * Constructs a new IPv4Host from the argument bytes.
        *
        * @param byte1 the first byte of the address
        * @param byte2 the second byte of the address
        * @param byte3 the third byte of the address
        * @param byte4 the fourth byte of the address
        * @return the IPv4Host represented by these bytes
        */
      final def apply(byte1: Byte, byte2: Byte, byte3: Byte, byte4: Byte): IPv4Host =
        fromBytes(Array(byte1, byte2, byte3, byte4))

      private def fromBytes(bytes: Array[Byte]): IPv4Host = {
        require(bytes.length == 4)
        new IPv4Host(immutable.Seq(bytes: _*))
      }

      final implicit val ipv4HostShow: Show[IPv4Host] =
        Show.show(_.asString)

      final implicit val ipv4HostEq: Eq[IPv4Host] =
        Eq.fromUniversalEquals
    }

    /**
      * An IPv6 host representation as specified by RFC 3987.
      *
      * @param bytes the underlying bytes
      */
    final case class IPv6Host private[rdf] (bytes: immutable.Seq[Byte]) extends Host {
      override def isIPv6: Boolean          = true
      override def asIPv6: Option[IPv6Host] = Some(this)

      override lazy val asString: String =
        asString(bytes.view)

      lazy val asMixedString: String =
        asString(bytes.view(0, 12)) + ":" + bytes.view(12, 16).map(_ & 0xFF).mkString(".")

      private def asString(bytes: SeqView[Byte, immutable.Seq[Byte]]): String =
        bytes.grouped(2).map(two => Integer.toHexString(BigInt(two.toArray).intValue())).mkString(":")
    }

    object IPv6Host {

      /**
        * Attempt to construct a new IPv6Host from its 128bit representation.
        *
        * @param bytes a 128bit IPv6 address
        * @return Right(IPv6Host(bytes)) if the bytes is a 16byte array, Left(error) otherwise
        */
      final def apply(bytes: Array[Byte]): Either[String, IPv6Host] =
        Either
          .catchNonFatal(fromBytes(bytes))
          .leftMap(_ => "Illegal IPv6Host byte representation")

      private def fromBytes(bytes: Array[Byte]): IPv6Host = {
        require(bytes.length == 16)
        new IPv6Host(immutable.Seq(bytes: _*))
      }

      final implicit val ipv6HostShow: Show[IPv6Host] =
        Show.show(_.asString)

      final implicit val ipv6HostEq: Eq[IPv6Host] =
        Eq.fromUniversalEquals
    }

    /**
      * A named host representation as specified by RFC 3987.
      *
      * @param value the underlying string representation
      */
    final case class NamedHost private[rdf] (value: String) extends Host {
      override def isNamed: Boolean           = true
      override def asNamed: Option[NamedHost] = Some(this)
      override def asString: String           = value
    }

    object NamedHost {

      /**
        * Attempt to construct a new NamedHost from the argument validating the character encodings as per RFC 3987.
        *
        * @param string the string to parse as a named host.
        * @return Right(NamedHost(value)) if the string conforms to specification, Left(error) otherwise
        */
      final def apply(string: String): Either[String, NamedHost] =
        new IriParser(string).`ireg-name`
          .run()
          .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

      final implicit val namedHostShow: Show[NamedHost] =
        Show.show(_.asString)

      final implicit val namedHostEq: Eq[NamedHost] =
        Eq.fromUniversalEquals
    }

    final implicit def hostShow(implicit ipv4: Show[IPv4Host],
                                ipv6: Show[IPv6Host],
                                named: Show[NamedHost]): Show[Host] = Show.show {
      case v: IPv4Host  => ipv4.show(v)
      case v: IPv6Host  => ipv6.show(v)
      case v: NamedHost => named.show(v)
    }
  }

  /**
    * Port part of an Iri as defined by RFC 3987.
    *
    * @param value the underlying int value
    */
  final case class Port private[rdf] (value: Int)

  object Port {

    /**
      * Attempts to construct a port from the argument int value.  Valid values are [0, 65535].
      *
      * @param value the underlying port value
      * @return Right(port) if successful, Left(error) otherwise
      */
    final def apply(value: Int): Either[String, Port] =
      if (value >= 0 && value < 65536) Right(new Port(value))
      else Left("Port value must in range [0, 65535]")

    /**
      * Attempts to construct a port from the argument string value.  Valid values are [0, 65535] without leading zeroes.
      *
      * @param string the string representation of the port
      * @return Right(port) if successful, Left(error) otherwise
      */
    final def apply(string: String): Either[String, Port] =
      new IriParser(string).`port`
        .run()
        .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

    final implicit val portShow: Show[Port] = Show.show(_.value.toString)
    final implicit val portEq: Eq[Port]     = Eq.fromUniversalEquals
  }

  /**
    * Path part of an Iri as defined in RFC 3987.
    */
  sealed abstract class Path extends Product with Serializable {

    /**
      * @return true if the path contains no characters, false otherwise
      */
    def isEmpty: Boolean

    /**
      * @return true if this path is a [[ch.epfl.bluebrain.nexus.rdf.Iri.Path.Slash]] (ends with a slash '/'), false otherwise
      */
    def isSlash: Boolean

    /**
      * @return true if this path is a [[ch.epfl.bluebrain.nexus.rdf.Iri.Path.Segment]] (ends with a segment), false otherwise
      */
    def isSegment: Boolean

    /**
      * @return Some(this) if this path is an Empty path, None otherwise
      */
    def asEmpty: Option[Empty]

    /**
      * @return Some(this) if this path is a Slash (ends with a slash '/') path, None otherwise
      */
    def asSlash: Option[Slash]

    /**
      * @return Some(this) if this path is a Segment (ends with a segment) path, None otherwise
      */
    def asSegment: Option[Segment]

    /**
      * @return true if this path ends with a slash ('/'), false otherwise
      */
    def endsWithSlash: Boolean = isSlash
  }

  object Path {

    /**
      * Constant value for a single slash.
      */
    final val / = Slash(Empty)

    /**
      * Attempts to parse the argument string as an `ipath-abempty` Path as defined by RFC 3987.
      *
      * @param string the string to parse as a Path
      * @return Right(Path) if the parse succeeds, Left(error) otherwise
      */
    final def apply(string: String): Either[String, Path] =
      abempty(string)

    /**
      * Attempts to parse the argument string as an `ipath-abempty` Path as defined by RFC 3987.
      *
      * @param string the string to parse as a Path
      * @return Right(Path) if the parse succeeds, Left(error) otherwise
      */
    final def abempty(string: String): Either[String, Path] =
      new IriParser(string).`ipath-abempty`
        .run()
        .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

    /**
      * An empty path.
      */
    sealed trait Empty extends Path

    /**
      * An empty path.
      */
    final case object Empty extends Empty {
      def isEmpty: Boolean           = true
      def isSlash: Boolean           = false
      def isSegment: Boolean         = false
      def asEmpty: Option[Empty]     = Some(this)
      def asSlash: Option[Slash]     = None
      def asSegment: Option[Segment] = None
    }

    /**
      * A path that ends with a '/' character.
      *
      * @param rest the remainder of the path (excluding the '/')
      */
    final case class Slash(rest: Path) extends Path {
      def isEmpty: Boolean           = false
      def isSlash: Boolean           = true
      def isSegment: Boolean         = false
      def asEmpty: Option[Empty]     = None
      def asSlash: Option[Slash]     = Some(this)
      def asSegment: Option[Segment] = None
    }

    /**
      * A path that ends with a segment.
      *
      * @param rest the remainder of the path (excluding the segment denoted by this)
      */
    final case class Segment private[rdf] (segment: String, rest: Path) extends Path {
      def isEmpty: Boolean           = false
      def isSlash: Boolean           = false
      def isSegment: Boolean         = true
      def asEmpty: Option[Empty]     = None
      def asSlash: Option[Slash]     = None
      def asSegment: Option[Segment] = Some(this)
    }

    final implicit val pathShow: Show[Path] = Show.show {
      case Empty                  => ""
      case Slash(rest)            => pathShow.show(rest) + "/"
      case Segment(segment, rest) => pathShow.show(rest) + segment
    }

    final implicit val pathEq: Eq[Path] = Eq.fromUniversalEquals
  }

  /**
    * Query part of an Iri as defined in RFC 3987.
    *
    * @param value a sorted multi map that represents all key -> value pairs
    */
  final case class Query private[rdf] (value: SortedMap[String, SortedSet[String]])

  object Query {

    /**
      * Attempts to parse the argument string as an `iquery` as defined by RFC 3987 and evaluate the key=value pairs.
      *
      * @param string the string to parse as a Query
      * @return Right(Query) if the parse succeeds, Left(error) otherwise
      */
    final def apply(string: String): Either[String, Query] =
      new IriParser(string).`iquery`
        .run()
        .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

    final implicit val queryShow: Show[Query] =
      Show.show(
        _.value
          .map {
            case (k, s) =>
              s.map {
                  case v if v.isEmpty => k
                  case v              => s"$k=$v"
                }
                .mkString("&")
          }
          .mkString("&"))

    final implicit val queryEq: Eq[Query] =
      Eq.fromUniversalEquals
  }

  /**
    * Fragment part of an Iri as defined by RFC 3987.
    *
    * @param value the string value of the fragment
    */
  final case class Fragment private[rdf] (value: String)

  object Fragment {

    /**
      * Attempts to parse the argument string as an `ifragment` as defined by RFC 3987.
      *
      * @param string the string to parse as a Fragment
      * @return Right(Fragment) if the parse succeeds, Left(error) otherwise
      */
    final def apply(string: String): Either[String, Fragment] =
      new IriParser(string).`ifragment`
        .run()
        .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

    final implicit val fragmentShow: Show[Fragment] = Show.show(_.value)
    final implicit val fragmentEq: Eq[Fragment]     = Eq.fromUniversalEquals
  }

}
